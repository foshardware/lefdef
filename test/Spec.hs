{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed
import Data.Text
import Data.Text.Encoding

import Test.Tasty
import Test.Tasty.HUnit

import Language.DEF.Parser


main :: IO ()
main = defaultMain $ testGroup "DEF"
  [ map9v3
  ]


map9v3 :: TestTree
map9v3 = testGroup "map9v3"
  [ testCase "Parse DEF" $ either (error . show) (putStrLn . show) (parseDEF map9v3Def)
  ]


map9v3Def :: Text
map9v3Def = decodeUtf8 $(embedFile "sample/map9v3.def")


