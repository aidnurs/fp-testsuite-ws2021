module TestSuite2 where

import Test.Tasty
import Test.Tasty.HUnit
import Angabe2

ta :: Testart
ta = PCR

dgs1 :: DreiG_Status
dgs1 = Getestet PCR (D XX Okt 2021) (U (Viertel,Acht,VM)) 
dgs2 :: DreiG_Status
dgs2 = Geimpft (BioNTec,Einmal)
dgs3 :: DreiG_Status
dgs3 = Genesen
dgs4 :: DreiG_Status
dgs4 = Getestet Antigen (D XXII Okt 2021) (U (Schlag,Acht,VM))
dgs5 :: DreiG_Status
dgs5 = Geimpft (Sputnik,Zweimal)


buergermeister = P (Vorname "Michael") (Nachname "Ludwig") dgs1 :: Person
bundesminister = P (Vorname "Wolfgang") (Nachname "Mueckstein") dgs2 :: Person
bundeskanzler = P (Vorname "Alexander") (Nachname "Schallenberg") dgs3 :: Person
bundespraesident = P (Vorname "Alexander") (Nachname "van der Bellen") dgs4 :: Person
sputnik_person = P (Vorname "Alexander") (Nachname "van der Bellen") dgs5 :: Person
bgm  = buergermeister
bm   = bundesminister
bk   = bundeskanzler
bp   = bundespraesident
kzp1 = ((D XXII Okt 2021),(U (Dreiviertel,Acht,NM)))
kzp2 = ((D XXVIII Okt 2021),(U (Dreiviertel,Acht,NM)))
kzp3 = ((D XXX Nov 2021),(U (Dreiviertel,Acht,NM)))

spec :: TestTree
spec =
  testGroup
    "Angabe2 Tests"
    [
        isValidDateTests,
        convertDateAndTimeTests,
        checkTestTests,
        einzulassenTests
    ]


einzulassenTests :: TestTree
einzulassenTests =
  testGroup
    "isValidDate Tests"
    [ 
        testCase "einzulassen 1" $
          einzulassen (bgm,DreiG,kzp1) @?= Einlassen,
        testCase "einzulassen 2" $
          einzulassen (bgm,DreiG,kzp2) @?= Abweisen,
        testCase "einzulassen 3" $
          einzulassen (sputnik_person,DreiG,kzp2) @?= Abweisen,
        -- testCase "einzulassen 4" $
        --   einzulassen (bgm,DreiG,kzp3) @?= Ungueltig,

        testCase "einzulassen 5" $
          einzulassen (bm,DreiG,kzp1) @?= Abweisen,
        testCase "einzulassen 6" $
          einzulassen (bm,DreiG,kzp2) @?= Abweisen,
        -- testCase "einzulassen 7" $
        --   einzulassen (bm,DreiG,kzp3) @?= Ungueltig,

        testCase "einzulassen 8" $
          einzulassen (bk,DreiG,kzp1) @?= Einlassen ,
        testCase "einzulassen 9" $
          einzulassen (bk,DreiG,kzp2) @?= Einlassen ,
        -- testCase "einzulassen 10" $
        --   einzulassen (bk,DreiG,kzp3) @?= Ungueltig,

        testCase "einzulassen 11" $
          einzulassen (bp,ZweieinhalbG,kzp1) @?= Abweisen ,
        testCase "einzulassen 12" $
          einzulassen (bp,ZweieinhalbG,kzp2) @?= Abweisen
        -- testCase "einzulassen 13" $
        --   einzulassen (bp,ZweieinhalbG,kzp3) @?= Ungueltig
    ]

isValidDateTests :: TestTree
isValidDateTests = 
  testGroup
    "isValidDate Tests"
    [
      testCase "isValidDate 1" $
        isValidDate (D XXXI Okt 2021) @?= False,
      testCase "isValidDate 2" $
        isValidDate (D XXX Okt 2021) @?= True,
      testCase "isValidDate 3" $
        isValidDate (D XXXI Sep 2021) @?= True,
      testCase "isValidDate 4" $
        isValidDate (D XXIX Feb 2021) @?= False,
      testCase "isValidDate 5" $
        isValidDate (D XXIX Feb 2020) @?= True
    ]

convertDateAndTimeTests :: TestTree
convertDateAndTimeTests =
  testGroup
    "convertDateAndTime Tests"
    [
      testCase "convertDateAndTime 1" $
        convertDateAndTime (D X Okt 2021) (U (Dreiviertel,Acht,NM)) @?= 90362700
    ]

checkTestTests :: TestTree
checkTestTests =
  testGroup
    "checkTest Tests"
    [
      testCase "checkTest 1" $
        checkTest (bgm,DreiG,kzp2) @?= Abweisen,
      testCase "checkTest 2" $
        checkTest (bgm,DreiG,kzp1) @?= Einlassen
    ]