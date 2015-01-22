{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.PList.Binary

import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Data.DeriveTH

import Test.QuickCheck.Instances

import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H

import qualified Data.ByteString.Lazy as BL

import System.Process
import System.Exit
import System.IO.Unsafe

import GHC.IO.Handle
import System.IO.Temp

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = fmap V.fromList arbitrary
  shrink = map V.fromList . shrink . V.toList

derive makeArbitrary ''PList -- TODO: forbid 0 length UTF16 strings

decodePList' x = let Right res = decodePList x
                  in res

equal :: PList -> PList -> Bool
equal (PBool  x1) (PBool  x2) = x1 == x2
equal (PInt   x1) (PInt   x2) = x1 == x2
equal (PReal  x1) (PReal  x2) = abs (x1 - x2) < 1e-8
equal (PDate  x1) (PDate  x2) = abs (x1 - x2) < 1e-8
equal (PData  x1) (PData  x2) = x1 == x2
equal (PASCII x1) (PASCII x2) = x1 == x2
equal (PUTF16 x1) (PUTF16 x2) = x1 == x2
equal (PUID   x1) (PUID   x2) = x1 == x2
equal (PArray x1) (PArray x2) = V.all (==True) $ V.zipWith equal x1 x2 
equal (PDict  x1) (PDict  x2) = all (==True) (zipWith (==) (H.keys x1) (H.keys x2)) 
                                && all (==True) (zipWith equal (H.elems x1) (H.elems x2))
equal _           _           = False

prop_encode_decode x = (decodePList' $ encodePList x) `equal` x

prop_encode_decode_encode x = (encodePList $ decodePList' $ encodePList x) == encodePList x

-- would like to use stdin, but unpacking bytestring seems to change encoding
prop_plutil x = unsafePerformIO $ withSystemTempFile "XXXX.tmp" $ \filepath handle -> do
  BL.hPut handle $ encodePList $ PDict $ H.fromList [("root", x)]
  hClose handle
  res <- readProcessWithExitCode "plutil" ["-lint", filepath] [] 
  case res of
    (ExitSuccess, _, _)   -> return True
    (ExitFailure _, _, _) -> return False
{-# NOINLINE prop_plutil #-}

main = $defaultMainGenerator
