module TestSuite1 where

import Angabe1
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup
    "Angabe1 Tests"
    [ testCase "A1.1" $
        ist_tzr "Urknallexplosion" "knall" @?= True,
      testCase "A1.2" $
        ist_tzr "Urknalexplosionkna" "knall" @?= False,
      testCase "A1.3" $
        ist_tzr "aaabcd" "bcde" @?= False,
      testCase "A1.4" $
        ist_tzr "aaabcdef" "abc" @?= True,
      testCase "A1.5" $
        ist_tzr "Urknallexplosion" "Knall" @?= False,
      testCase "A1.6" $
        ist_tzr "Urknallexplosion" "" @?= True,
      testCase "A1.7" $
        ist_tzr "" "Urknallexplosion" @?= False,
      testCase "A1.8" $
        ist_tzr "Urknallexplosionsknall" "knall" @?= True,
      testCase "A2.1" $
        tzr_zeuge "Urknallexplosion" "knall" @?= ("Ur", "knall", "explosion"),
      testCase "A2.2" $
        tzr_zeuge "Urknallexplosion" "knall"  @?= ("Ur","knall","explosion"),
      testCase "A2.3" $
        tzr_zeuge "Urknallexplosion" "Knall"  @?= ("","KnallKnall",""),
      testCase "A2.4" $
        tzr_zeuge "Urknallexplosionsknall" "knall" @?= ("Ur","knall","explosionsknall"),
      testCase "A2.5" $
        tzr_zeuge "" "" @?= ("","",""),
      testCase "A2.6" $
        tzr_zeuge "" "asd" @?= ("","asdasd","")
--       testCase "A2.1" $
-- tzr_zeuge "Urknall" ""  @?= ("","","Urknall") oder ("U","","rknall") oder...
    ]