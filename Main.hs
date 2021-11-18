module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified TestSuite1
import qualified TestSuite2
import qualified TestSuite2a
import qualified TestSuite3
import qualified TestSuite4
import qualified TestSuite4a
import qualified TestSuite5
import qualified TestSuite5a
import qualified TestSuite6
import qualified TestSuite7

main :: IO ()
main = defaultMain spec

spec :: TestTree
spec =
  testGroup
    "All Tests"
    [ -- TestSuite1.spec,
      -- TestSuite2.spec,
      -- TestSuite2a.spec,
      -- TestSuite3.spec,
      -- TestSuite4.spec,
      -- TestSuite4a.spec
      TestSuite5.spec,
      TestSuite5a.spec
      -- TestSuite6.spec,
      -- TestSuite7.spec
    ]
