{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed
import Data.Text hiding (unpack)
import Data.Text.Lazy (fromStrict, unpack)
import Data.Text.Encoding

import Test.Tasty
import Test.Tasty.HUnit

import Language.DEF.Builder
import Language.DEF.Parser



main :: IO ()
main = defaultMain $ testGroup "DEF"
  [ map9v3
  ]


map9v3 :: TestTree
map9v3 = testGroup "map9v3"
  [ testCase "Parse DEF" $ either (error . show) (pure . const ()) (parseDEF map9v3Def)
  , testCase "Build DEF" $ testBuildDEF
  ]


testBuildDEF :: IO ()
testBuildDEF = do
    def_ <- either (error . show) pure (parseDEF map9v3Def)
    let str = unpack $ buildDEF def_
    writeFile "out" str
    assertBool str $ buildDEF def_ == fromStrict map9v3Def


map9v3Def :: Text
map9v3Def = decodeUtf8 $(embedFile "sample/map9v3.def")


