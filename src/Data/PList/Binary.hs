{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Data.PList.Binary (
                           PList(..)
                         , decodePList
                         ) where

import Prelude as P hiding (mapM)

import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V hiding (replicateM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits

import Data.Word
import Data.Int
import GHC.Float

import Data.Time
import qualified Data.Text as T
import Data.Text.Encoding as TE

import Control.Applicative
import Control.Monad as M hiding (mapM)
import Control.Monad.Except hiding (mapM)
import Control.Monad.Identity hiding (mapM)

import Data.Bifunctor

import Data.Traversable

-- | A property list. Used by OS X and iOS
data PList = PBool Bool                       -- ^ boolean
           | PInt Int64                       -- ^ signed integer
           | PReal Double                     -- ^ floating point
           | PDate UTCTime                    -- ^ date
           | PData B.ByteString                 -- ^ binary data
           | PASCII B.ByteString                -- ^ ascii string
           | PUTF16 T.Text                      -- ^ utf-16 string
           | PUID Word64                      -- ^ unsigned integer
           | PArray (V.Vector PList)            -- ^ array
           | PDict (H.HashMap B.ByteString PList) -- ^ dictionary
           deriving (Show, Read, Eq)

-- Intemediate plist data structure
data ImPList = IBool Bool
             | IInt Int64
             | IReal Double
             | IDate UTCTime
             | IData B.ByteString
             | IASCII B.ByteString
             | IUTF16 T.Text
             | IUID Word64
             | IArray [Int]       -- an array of references to other objects
             | IDict [(Int, Int)] -- an dictionary of references
             deriving (Show)

toIntermediate :: PList -> [ImPList]
toIntermediate = undefined

fromIntermediate :: [ImPList] -> [PList]
fromIntermediate xs = converted
  where
    converted = P.map convert xs
    convert (IBool x) = PBool x
    convert (IInt x) = PInt x
    convert (IReal x) = PReal x
    convert (IDate x) = PDate x
    convert (IData x) = PData x
    convert (IASCII x) = PASCII x
    convert (IUTF16 x) = PUTF16 x
    convert (IUID x) = PUID x
    convert (IArray x) = PArray $ V.fromList (P.map (converted !!) x)
    convert (IDict x) = PDict $ H.fromList (P.map (\(a,b) -> (unwrap (converted !! a), converted !! b)) x)
    unwrap (PASCII x) = x

{-
-- http://opensource.apple.com/source/CF/CF-744.18/CFBinaryPList.c
-- TODO: grab all remaining bytes
instance Binary PList where
  get = do
    -- binary plist starts with bplist00 or bplist01
    header <- getByteString 8
    if header == "bplist00" then return () else fail "bad header"
    let l = M.sequence [get' l]
    l <- get'
    fromintermediate l
    where
      -- get' :: [Get PList] -> Get PList
      get' = do
        -- items are prefixed by type and size
        w::Word8 <- get
        let l = (w .&. 0xf) -- lower bytes are length
            i = shiftR w 4 -- high bytes are type
        len <- fromIntegral <$> 
                 case l of
                   15 | i /= 0 && i /= 1 && i /= 2 && i /= 3 -> do
                     -- if size if 0xf, then the actualy size is encoded as a plist integer following this byte
                     plint <- get' list :: Get PList
                     case plint of
                       PInt i -> return i
                       _      -> fail "Expected integer for extended length"
                   _ -> return $ fromIntegral l
        case i of
          0x0 -> return $ PBool $ l /= 0 -- bool is encoded in length
          0x1 -> PInt <$> case l of -- 2^i = byte length of integer TODO: support arbitrary lengths
                                          0 -> fromIntegral <$> (get :: Get Int8)
                                          1 -> fromIntegral <$> (get :: Get Int16)
                                          2 -> fromIntegral <$> (get :: Get Int32)
                                          3 -> fromIntegral <$> (get :: Get Int64)
                                          x -> fail $ "invalid integer length: " P.++ show x
          0x2 -> PReal <$> case l of -- similar encoding to integer
                                            2 -> float2Double <$> (get :: Get Float)
                                            3 -> get :: Get Double
                                            _ -> fail "invalid float size"
          0x3 -> undefined
          0x4 -> PData <$> getByteString len
          0x5 -> PASCII <$> getByteString len
          0x6 -> PUTF16 <$> (decodeUtf16BE <$> getByteString len)
          0x8 -> PUID <$> case l of
                                          0 -> fromIntegral <$> (get :: Get Word8)
                                          1 -> fromIntegral <$> (get :: Get Word16)
                                          2 -> fromIntegral <$> (get :: Get Word32)
                                          3 -> fromIntegral <$> (get :: Get Word64)
                                          _ -> fail "invalid UID length"
          -- arrays and dictionaries contain references to other elements
          0xa -> PArray <$> (V.fromList <$> replicateM len (getWord8 >>= return . (list !!) . fromIntegral)) -- temp store as ints
          0xd -> do
                   keys <- replicateM (len*2) (getWord8 >>= return . (list !!) . fromIntegral)
                   vals <- replicateM (len*2) (getWord8 >>= return . (list !!) . fromIntegral)
                   keys' <- M.mapM unwrap keys
                   return $ PDict $ H.fromList $ P.zip keys' vals
                    where
                      unwrap (PASCII s) = return s
                      unwrap a          = fail $ show a  -- "non-string key"
          x   -> fail $ "Unexpected type: " P.++ show x
      lookup' (x:xs) 0 = x
      lookup' (x:xs) i = x >> lookup' xs (i-1)

  -- TODO: broken
  put m = do
    -- header
    putByteString "bplist00"
    go m
    where
      tag (PBool   _) = 0x0 :: Word8
      tag (PInt    _) = 0x1
      tag (PReal   _) = 0x2
      tag (PDate   _) = 0x3
      tag (PData   _) = 0x4
      tag (PASCII  _) = 0x5
      tag (PUTF16  _) = 0x6
      tag (PUID    _) = 0x8
      tag (PArray  _) = 0xa
      tag (PDict   _) = 0xd
      go m = do
        let len = case m of
                    PBool _  -> 0
                    PInt _   -> 3 -- TODO: set sized based on highest set bit
                    PReal _  -> 3 -- TODO: see above
                    PDate _  -> undefined
                    PData b  -> B.length b
                    PASCII a -> B.length a
                    PUTF16 t -> T.length t
                    PUID _   -> 3 -- TODO: same issue as PInt
                    PArray a -> V.length a
                    PDict d  -> H.size d
        case m of
          PBool b -> put (shiftL (tag m) 4 .|. if b then 1 else 0)
          _ -> do
                 if len < 15 then
                   putWord8 (shiftL (tag m) 4 .|. (fromIntegral len))
                 else
                   put $ PInt $ fromIntegral len
                 case m of
                   PBool _   -> fail "unreachable"
                   PInt i    -> put i
                   PReal f   -> put f
                   PDate d   -> undefined
                   PData b   -> putByteString b
                   PASCII a  -> putByteString a
                   PUTF16 t  -> putByteString (encodeUtf16BE t)
                   PUID u    -> put u
                   PArray a  -> V.mapM_ put a
                   PDict d   -> do
                                  M.mapM_ putWord8 [1..(fromIntegral len)*2] -- do not know why this needs to be here
                                  M.mapM_ put $ keys d
                                  M.mapM_ put $ elems d
                                  -}

data Trailer = Trailer { unused1           :: Word8
                       , unused2           :: Word32
                       , sortVersion       :: Word8
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
    putWord8 $ sortVersion t
    putWord8 $ offsetIntSize t
    putWord8 $ objectRefSize t
    putWord64be $ numObjects t
    putWord64be $ topObject t
    putWord64be $ offsetTableOffset t

trailerSize = 32

decodePList :: BL.ByteString -> Either String PList
decodePList s = runExcept $ do
  if BL.take 8 s == "bplist00" then return () else throwError "invalid file format, must be bplist00"
  trailer <- decodeBinary (BL.drop ((BL.length s) - trailerSize) s) get -- decode the trailer to figure out where offsets are
  offsets <- decodeBinary (BL.drop (fromIntegral $ offsetTableOffset trailer) s) 
                          (replicateM (fromIntegral $ numObjects trailer) $ getWordbe $ fromIntegral $ offsetIntSize trailer)
  objects <- mapM (\off -> decodeBinary (BL.drop (fromIntegral off) s) (getObject (fromIntegral $ objectRefSize trailer))) offsets
  return $ (fromIntermediate $ objects) !! fromIntegral (topObject trailer)
    where
      thrd (a,b,c) = c
      decodeBinary :: BL.ByteString -> Get a -> Except String a
      decodeBinary s g = ExceptT $ Identity $ bimap thrd thrd $ runGetOrFail g s
      getWordbe :: Int -> Get Int
      getWordbe size = do
        words <- replicateM size getWord8
        return $ P.foldl (\b a -> (shiftL b 8) .|. (fromIntegral a)) 0 words

      getObject refSize = do
        let getRef = getWordbe refSize
        -- items are prefixed by type and size
        w <- getWord8
        let l = (w .&. 0xf) -- lower bytes are length
            i = shiftR w 4 -- high bytes are type
        len <- fromIntegral <$> 
                 case l of
                   15 | i /= 0 && i /= 1 && i /= 2 && i /= 3 -> do
                     -- if size if 0xf, then the actualy size is encoded as a plist integer following this byte
                     plint <- getObject refSize :: Get ImPList
                     case plint of
                       IInt i -> return i
                       _      -> fail "Expected integer for extended length"
                   _ -> return $ fromIntegral l
        case i of
          0x0 -> return $ IBool $ l /= 0 -- bool is encoded in length
          0x1 -> IInt <$> case l of -- 2^i = byte length of integer TODO: support arbitrary lengths
                                          0 -> fromIntegral <$> (get :: Get Int8)
                                          1 -> fromIntegral <$> (get :: Get Int16)
                                          2 -> fromIntegral <$> (get :: Get Int32)
                                          3 -> fromIntegral <$> (get :: Get Int64)
                                          x -> fail $ "invalid integer length: " P.++ show x
          0x2 -> IReal <$> case l of -- similar encoding to integer
                                            2 -> float2Double <$> (get :: Get Float)
                                            3 -> get :: Get Double
                                            _ -> fail "invalid float size"
          0x3 -> undefined
          0x4 -> IData <$> getByteString len
          0x5 -> IASCII <$> getByteString len
          0x6 -> IUTF16 <$> (decodeUtf16BE <$> getByteString len)
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
