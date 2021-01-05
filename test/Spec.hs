
import Test.Tasty

import Spec.LEF
import Spec.DEF


main :: IO ()
main = defaultMain $ testGroup "LEF/DEF"
  [ defs
  , lefs
  ]

