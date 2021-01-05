
module Spec.DEF where

import Control.Monad

import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Data.Text.IO (readFile)
import Prelude hiding (readFile)
import System.FilePath

import Test.Tasty
import Test.Tasty.HUnit

import Language.DEF.Builder (builderDEF)
import Language.DEF.Parser (parseDEF)
import Language.DEF.Syntax



defs :: TestTree
defs = testGroup "DEF"
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
ibms = testGroup "ibm-place2"

  [ ibm_place2 (132,4,3,12028,246,11753) "ibm01-tr0-85"
  , ibm_place2 (130,4,3,12028,246,11753) "ibm01-tr0-88"

  , ibm_place2 (153,5,4,19062,259,18688) "ibm02-tr0-90"
  , ibm_place2 (149,5,4,19062,259,18688) "ibm02-tr0-95"

  , ibm_place2 (233,5,4,44811,287,44681) "ibm07-tr0-90"
  , ibm_place2 (226,5,4,44811,287,44681) "ibm07-tr0-95"

  , ibm_place2 (243,5,4,50672,286,48230) "ibm08-tr0-90"
  , ibm_place2 (236,5,4,50672,286,48230) "ibm08-tr0-95"

  , ibm_place2 (246,5,4,51382,285,50678) "ibm09-tr0-90"
  , ibm_place2 (240,5,4,51382,285,50678) "ibm09-tr0-95"

  , ibm_place2 (321,5,4,66762,744,64971) "ibm10-tr0-90"
  , ibm_place2 (313,5,4,66762,744,64971) "ibm10-tr0-95"

  , ibm_place2 (281,5,4,68046,406,67422) "ibm11-tr0-90"
  , ibm_place2 (273,5,4,68046,406,67422) "ibm11-tr0-95"

  , ibm_place2 (347,5,4,68735,637,68376) "ibm12-tr0-85"
  , ibm_place2 (338,5,4,68735,637,68376) "ibm12-tr0-90"
  ]


ibm_place2 :: (Int, Int, Int, Int, Int, Int) -> String -> TestTree
ibm_place2 (r, t, v, c, p, n) s = testCase s $ do

    file <- readFile $ "sample" </> "ibm-place2" </> take 5 s </> s <.> "def"
    top@(DEF _ _ _ rows tracks _ vias components pins nets _) <- either (fail . show) pure . parseDEF $ file

    assertEqual "row count"   r $ length rows
    assertEqual "track count" t $ length tracks
    assertEqual "via count"   v $ length vias
    assertEqual "gate count"  c $ length components
    assertEqual "pin count"   p $ length pins
    assertEqual "net count"   n $ length nets

    bot <- either (fail . show) pure . parseDEF . toStrict . toLazyText . builderDEF $ top

    assertBool "builder" $ top == bot

