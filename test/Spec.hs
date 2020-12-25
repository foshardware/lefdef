{-# LANGUAGE TemplateHaskell #-}

import Control.Monad

import Data.Text (Text)
import Data.Text.IO (readFile)
import Prelude hiding (readFile)
import System.FilePath

import Test.Tasty
import Test.Tasty.HUnit

import Language.DEF.Parser



main :: IO ()
main = defaultMain $ testGroup "DEF"
  [ samples
  , ibms
  ]



samples :: TestTree
samples = testGroup "Samples"
  [ testCase "map9v3" $ either (fail . show) (void . pure) . parseDEF =<< map9v3_def
  , testCase "map9v3_route" $ either (fail . show) (void . pure) . parseDEF =<< map9v3_route_def
  , testCase "gcd_nan45_nontd_def" $ either (fail . show) (void . pure) . parseDEF =<< gcd_nan45_nontd_def
  ]



map9v3_def :: IO Text
map9v3_def = readFile "sample/map9v3.def"

map9v3_route_def :: IO Text
map9v3_route_def = readFile "sample/map9v3_route.def"

gcd_nan45_nontd_def :: IO Text
gcd_nan45_nontd_def = readFile "sample/gcd_nan45_nontd.def"



ibms :: TestTree
ibms = testGroup "ibm-place2" $ ibm_place2 <$>

  [ "ibm01-tr0-85"
  , "ibm01-tr0-88"

  , "ibm02-tr0-90"
  , "ibm02-tr0-95"

  , "ibm07-tr0-90"
  , "ibm07-tr0-95"

  , "ibm08-tr0-90"
  , "ibm08-tr0-95"

  , "ibm09-tr0-90"
  , "ibm09-tr0-95"

  , "ibm10-tr0-90"
  , "ibm10-tr0-95"

  , "ibm11-tr0-90"
  , "ibm11-tr0-95"

  , "ibm12-tr0-85"
  , "ibm12-tr0-90"
  ]


ibm_place2 :: String -> TestTree
ibm_place2 s = testCase s $ do
    file <- readFile $ "sample" </> "ibm-place2" </> take 5 s </> s <.> "def"
    either (fail . show) (void . pure) $ parseDEF file

