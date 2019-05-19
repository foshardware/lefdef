{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed
import Data.Text hiding (unpack)
import Data.Text.Lazy (fromStrict, unpack)
import Data.Text.Lazy.Builder
import Data.Text.Encoding

import Test.Tasty
import Test.Tasty.HUnit

import Language.DEF.Builder
import Language.DEF.Parser



main :: IO ()
main = defaultMain $ testGroup "DEF"
  [ map9v3
  , map9v3_route
  ]


map9v3 :: TestTree
map9v3 = testGroup "map9v3"
  [ testCase "Parse DEF" $ either (error . show) (pure . const ()) (parseDEF map9v3_def)
  , testCase "Build DEF" $ testBuildDEF map9v3_def
  ]


map9v3_route :: TestTree
map9v3_route = testGroup "map9v3_route"
  [ testCase "Parse DEF" $ either (error . show) (pure . const ()) (parseDEF map9v3_route_def)
  , testCase "Build DEF" $ testBuildDEF map9v3_route_def
  ]


testBuildDEF :: Text -> IO ()
testBuildDEF text = do
    def_ <- either (error . show) pure (parseDEF text)
    let str = toLazyText $ builderDEF def_
    assertBool (unpack str) $ str == fromStrict text


map9v3_def :: Text
map9v3_def = decodeUtf8 $(embedFile "sample/map9v3.def")

map9v3_route_def :: Text
map9v3_route_def = decodeUtf8 $(embedFile "sample/map9v3_route.def")



