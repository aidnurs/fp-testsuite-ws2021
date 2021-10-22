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
        tzr_zeuge "Urexplosionknal" "knall"  @?= ("","knallknall",""),
      testCase "A2.3" $
        tzr_zeuge "Urknallexplosion" "Knall"  @?= ("","KnallKnall",""),
      testCase "A2.4" $
        tzr_zeuge "Urknallexplosionsknall" "knall" @?= ("Ur","knall","explosionsknall"),
      testCase "A2.5" $
        tzr_zeuge "" "" @?= ("","",""),
      testCase "A2.6" $
        tzr_zeuge "" "asd" @?= ("","asdasd",""),
      testCase "A2.7" $
        tzr_zeuge "abckkkkkkkkk" "abc" @?= ("","abc","kkkkkkkkk"),
      testCase "A2.8" $
        tzr_zeuge "kkkkkkkkkabc" "abc" @?= ("kkkkkkkkk","abc",""),

      testCase "A3.1" $
        tzr_zeugen "Urknallexplosion" "knall" @?= [("Ur", "knall", "explosion")],
      testCase "A3.2" $
        tzr_zeugen "Urknallexplosiknallon" "knall" @?= [("Ur", "knall", "explosiknallon"),("Urknallexplosi", "knall", "on")],
      testCase "A3.3" $
        tzr_zeugen "Urknallexpknalllosionknall" "knall" @?= [("Ur", "knall", "expknalllosionknall"),("Urknallexp", "knall", "losionknall"),("Urknallexpknalllosion", "knall", "")],

      testCase "A4.1" $
        wieOft "Urknallexplosion" "knall" @?= 1,
      testCase "A4.1" $
        wieOft "Urknallknaexknaplosion" "kna" @?= 3,
      testCase "A4.1" $
        wieOft "aaaaaaa" "a" @?= 7,
      testCase "A4.1" $
        wieOft "ababab" "bab" @?= 1
    ]