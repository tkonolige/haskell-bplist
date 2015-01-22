{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.PList.Binary ( PList(..)
                         , decodePList
                         , encodePList
                         ) where

import Prelude as P hiding (mapM)

import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V hiding (replicateM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754
import Data.Bits

import Data.Int
import GHC.Float

import Data.Time.Clock.POSIX
import qualified Data.Text as T
import Data.Text.Encoding as TE

import Control.Applicative
import Control.Monad as M hiding (mapM)
import Control.Monad.Except hiding (mapM)
import Control.Monad.Identity hiding (mapM)
import qualified Control.Monad.State as S

import Data.Bifunctor
import Control.Lens

import Data.Traversable

-- see http://opensource.apple.com/source/CF/CF-744.18/CFBinaryPList.c for reference

-- | A property list. Used by OS X and iOS
data PList = PBool Bool                           -- ^ boolean
           | PInt Int64                           -- ^ signed integer
           | PReal Double                         -- ^ floating point
           | PDate POSIXTime                      -- ^ date
           | PData B.ByteString                   -- ^ binary data
           | PASCII B.ByteString                  -- ^ ascii string
           | PUTF16 T.Text                        -- ^ utf-16 string
           | PUID Word64                          -- ^ unsigned integer
           | PArray (V.Vector PList)              -- ^ array
           | PDict (H.HashMap B.ByteString PList) -- ^ dictionary
           deriving (Show, Eq)

-- Intemediate plist data structure
-- TODO: dont use intermediate data structure
data ImPList = IBool Bool
             | IInt Int64
             | IReal Double
             | IDate POSIXTime
             | IData B.ByteString
             | IASCII B.ByteString
             | IUTF16 T.Text
             | IUID Word64
             | IArray [Int]       -- an array of references to other objects
             | IDict [(Int, Int)] -- an dictionary of references
             deriving (Show)

fromIntermediate :: [ImPList] -> [PList]
fromIntermediate xs = converted
  where
    converted = P.map convert xs
    convert (IBool  x) = PBool x
    convert (IInt   x) = PInt x
    convert (IReal  x) = PReal x
    convert (IDate  x) = PDate x
    convert (IData  x) = PData x
    convert (IASCII x) = PASCII x
    convert (IUTF16 x) = PUTF16 x
    convert (IUID   x) = PUID x
    convert (IArray x) = PArray $ V.fromList (P.map (converted !!)                                       x)
    convert (IDict  x) = PDict $ H.fromList (P.map (\(a,b) -> (unwrap (xs !! a), converted !! b)) x)
    unwrap (IASCII x) = x

data Trailer = Trailer { unused1           :: Word8
                       , unused2           :: Word32
                       , shortVersion      :: Word8
                       , offsetIntSize     :: Word8
                       , objectRefSize     :: Word8
                       , numObjects        :: Word64
                       , topObject         :: Word64
                       , offsetTableOffset :: Word64
                       } deriving (Show)

instance Binary Trailer where
  get = Trailer <$> getWord8 <*> getWord32be <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord64be <*> getWord64be <*> getWord64be
  put t = do
    putWord8 0
    putWord32be 0
    putWord8 $ shortVersion t
    putWord8 $ offsetIntSize t
    putWord8 $ objectRefSize t
    putWord64be $ numObjects t
    putWord64be $ topObject t
    putWord64be $ offsetTableOffset t

trailerSize :: Int64
trailerSize = 32

decodePList :: BL.ByteString -> Either String PList
decodePList s = runExcept $ do
  unless (BL.take 8 s == "bplist00") $ throwError "invalid file format, must be bplist00"
  -- decode the trailer to figure out where offsets are
  trailer <- decodeBinary (BL.drop (BL.length s - trailerSize) s) get
  -- decode all the offsets
  offsets <- decodeBinary (BL.drop (fromIntegral $ offsetTableOffset trailer) s) 
                          (replicateM (fromIntegral $ numObjects trailer) $ getWordbe $ fromIntegral $ offsetIntSize trailer)
  -- transform each offset into an object
  objects <- mapM (\off -> decodeBinary (BL.drop (fromIntegral off) s) (getObject (fromIntegral $ objectRefSize trailer))) offsets
  return $ fromIntermediate objects !! fromIntegral (topObject trailer)
    where
      thrd (_, _, c) = c
      decodeBinary :: BL.ByteString -> Get a -> Except String a
      decodeBinary str g = ExceptT $ Identity $ bimap thrd thrd $ runGetOrFail g str
      getWordbe :: Int -> Get Int
      getWordbe size = do
        bytes <- replicateM size getWord8
        return $ P.foldl (\b a -> shiftL b 8 .|. fromIntegral a) 0 bytes

      getObject refSize = do
        let getRef = getWordbe refSize
        -- items are prefixed by type and size
        w <- getWord8
        let l = w .&. 0xf -- lower bytes are length
            i = shiftR w 4 -- high bytes are type
        len <- fromIntegral <$> 
                 case l of
                   15 | i /= 0 && i /= 1 && i /= 2 && i /= 3 -> do
                     -- if size if 0xf, then the actualy size is encoded as a plist integer following this byte
                     plint <- getObject refSize :: Get ImPList
                     case plint of
                       IInt int -> return int
                       _      -> fail "Expected integer for extended length"
                   _ -> return $ fromIntegral l
        case i of
          0x0 -> case l of -- bool is encoded in length as 0b1001 for true and 0b1000 for false
                   8 -> return $ IBool False
                   9 -> return $ IBool True
                   _ -> fail $ "unexpected bool constant " ++ show l
          0x1 -> IInt <$> case l of -- 2^i = byte length of integer TODO: support arbitrary lengths
                            0 -> fromIntegral <$> (get :: Get Int8)
                            1 -> fromIntegral <$> (get :: Get Int16)
                            2 -> fromIntegral <$> (get :: Get Int32)
                            3 -> fromIntegral <$> (get :: Get Int64)
                            x -> fail $ "invalid integer length: " P.++ show x
          0x2 -> IReal <$> case l of -- similar encoding to integer
                             2 -> float2Double <$> getFloat32be
                             3 -> getFloat64be
                             _ -> fail "invalid float size"
          0x3 -> IDate <$> realToFrac <$> getFloat64be
          0x4 -> IData <$> getByteString len
          0x5 -> IASCII <$> getByteString len
          0x6 -> IUTF16 <$> (decodeUtf16BE <$> getByteString (len * 2))
          0x8 -> IUID <$> case l of
                                          0 -> fromIntegral <$> (get :: Get Word8)
                                          1 -> fromIntegral <$> (get :: Get Word16)
                                          2 -> fromIntegral <$> (get :: Get Word32)
                                          3 -> fromIntegral <$> (get :: Get Word64)
                                          _ -> fail "invalid UID length"
          -- arrays and dictionaries contain references to other elements
          0xa -> IArray <$> replicateM len getRef -- temp store as ints
          0xd -> do
                   keys <- replicateM len getRef
                   vals <- replicateM len getRef
                   return $ IDict $ zip keys vals
          x   -> fail $ "Unexpected type: " P.++ show x

-- types as defined by apple
typeId :: PList -> Word8
typeId (PBool  _) = 0x0
typeId (PInt   _) = 0x1
typeId (PReal  _) = 0x2
typeId (PDate  _) = 0x3
typeId (PData  _) = 0x4
typeId (PASCII _) = 0x5
typeId (PUTF16 _) = 0x6
typeId (PUID   _) = 0x8
typeId (PArray _) = 0xa
typeId (PDict  _) = 0xd

-- TODO: pack ints and words based on size
-- | How many bytes an elcoded object occupies
elemLen :: PList -> Int
elemLen (PBool  _) = 0
elemLen (PInt   _) = 3
elemLen (PReal  _) = 3
elemLen (PDate  _) = 3
elemLen (PData  x) = B.length x
elemLen (PASCII x) = B.length x
elemLen (PUTF16 x) = T.length x
elemLen (PUID   _) = 3
elemLen (PArray x) = V.length x
elemLen (PDict  x) = H.size x

data PState = PState { _refNum     :: Int
                     , _offset     :: Int
                     , _objOffsets :: [Int]
                     } deriving (Show)
makeLenses ''PState

type PutState = S.StateT PState PutM ()

-- | binary encode a PList
encodePList :: PList -> BL.ByteString
encodePList plist = runPut $ do 
  (r,s) <- S.runStateT (do
      putByteString' "bplist00"
      putObjectOffset plist
    ) PState { _refNum = 1, _offset = 0, _objOffsets = [] }
  let trailer = Trailer { unused1 = 0
                        , unused2 = 0
                        , shortVersion = 0
                        , offsetIntSize = fromIntegral $ bytesToEncode $ fromIntegral $ length $ s ^. objOffsets
                        , objectRefSize = fromIntegral $ bytesToEncode $ fromIntegral numRefs
                        , numObjects = fromIntegral numRefs
                        , topObject = 0
                        , offsetTableOffset = fromIntegral $ s ^. offset
                        }
  S.evalStateT (mapM_ (putWordbe (offsetIntSize trailer)) (s ^. objOffsets)) PState { _refNum = 1, _offset = 0, _objOffsets = [] }
  put trailer
  return r
  where
    numRefs :: Int
    numRefs = numRefs' plist + 1
    numRefs' (PArray x) = V.length x + V.sum (V.map numRefs' x)
    numRefs' (PDict x)  = H.size x * 2 + sum (map numRefs' (H.elems x))
    numRefs' _          = 0
   
    bytesToEncode :: Int -> Int 
    bytesToEncode numThings = let go 0 i = i
                                  go x i = go (shiftR x 8) (i+1)
                               in go numThings 0 

    putRef :: Int -> PutState
    putRef x = do
      refNum += 1
      putWordbe (bytesToEncode numRefs) x

    putWordbe maxSize x | maxSize <= 1 = putWord8'    $ fromIntegral x
    putWordbe maxSize x | maxSize <= 2 = putWord16be' $ fromIntegral x
    putWordbe maxSize x | maxSize <= 4 = putWord32be' $ fromIntegral x
    putWordbe maxSize x | maxSize <= 8 = putWord64be' $ fromIntegral x

    putObjectLen :: PList -> PutState
    putObjectLen (PBool True)       = putWord8' 9
    putObjectLen (PBool False)      = putWord8' 8
    putObjectLen x | elemLen x < 15 = putWord8' $ shiftL (typeId x) 4 .|. fromIntegral (elemLen x)
    putObjectLen x                  = putWord8' (shiftL (typeId x) 4 .|. 0xf) >> putObject (PInt $ fromIntegral $ elemLen x)

    putObjectOffset :: PList -> PutState
    putObjectOffset x = do
      curOffset <- use offset
      objOffsets <>= [curOffset]
      putObject x
    putObject :: PList -> PutState
    putObject x = putObjectLen x >> putObject' x
    putObject' :: PList -> PutState
    putObject' (PBool  _) = return () -- bool is encoded with size of type
    putObject' (PInt   x) = putInt64' x
    putObject' (PReal  x) = putDouble' x
    putObject' (PDate  x) = putDouble' $ fromRational $ toRational x
    putObject' (PData  x) = putByteString' x
    putObject' (PASCII x) = putByteString' x
    putObject' (PUTF16 x) = putByteString' $ TE.encodeUtf16BE x
    putObject' (PUID   x) = putWord64' x
    putObject' (PArray x) = do
      ind <- use refNum
      mapM_ putRef $ getRefs ind $ V.toList x
      V.mapM_ putObjectOffset x
    putObject' (PDict  x) = do
      ind <- use refNum
      mapM_ putRef [ind.. ind + H.size x - 1]
      mapM_ putRef $ getRefs (ind + H.size x - 1) $ H.elems x
      mapM_ (putObjectOffset . PASCII) $ H.keys x
      mapM_ putObjectOffset $ H.elems x

    getRefs ind [] = []
    getRefs ind (x:xs) = ind : getRefs (ind + numRefs' x + 1) xs

    putInt64' x = do
      lift $ put x
      offset += 8
    putWord64' x = do
      lift $ put x
      offset += 8
    putDouble' x = do
      lift $ putFloat64be x
      offset += 8
    putByteString' x = do
      lift $ putByteString x
      offset += B.length x
    putWord8' x = do
      lift $ putWord8 x
      offset += 1
    putWord16be' x = do
      lift $ putWord16be x
      offset += 2
    putWord32be' x = do
      lift $ putWord32be x
      offset += 4
    putWord64be' x = do
      lift $ putWord64be x
      offset += 8
