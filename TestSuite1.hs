module TestSuite1 where

import Angabe1
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup
    "Angabe1 Tests"
    [ testCase "1" $
        ist_tzr "Urknallexplosion" "knall" @?= True,
      testCase "1" $
        ist_tzr "Urknalexplosionkna" "knall" @?= False,
      testCase "1" $
        ist_tzr "aaabcd" "bcde" @?= False,
      testCase "1" $
        ist_tzr "aaabcdef" "abc" @?= True,
      testCase "2" $
        ist_tzr "Urknallexplosion" "Knall" @?= False,
      testCase "3" $
        ist_tzr "Urknallexplosion" "" @?= True,
      testCase "4" $
        ist_tzr "" "Urknallexplosion" @?= False,
      testCase "5" $
        ist_tzr "Urknallexplosionsknall" "knall" @?= True
    ]