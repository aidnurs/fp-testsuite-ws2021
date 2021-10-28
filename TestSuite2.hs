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
dgs6 :: DreiG_Status
dgs6 = Getestet Antigen (D XXX Nov 2021) (U (Schlag,Sechs,VM))
dgs7 :: DreiG_Status
dgs7 = Getestet Antigen (D XXVIII Nov 2021) (U (Schlag,Acht,NM))

buergermeister = P (Vorname "Michael") (Nachname "Ludwig") dgs1 :: Person
bundesminister = P (Vorname "Wolfgang") (Nachname "Mueckstein") dgs2 :: Person
bundeskanzler = P (Vorname "Alexander") (Nachname "Schallenberg") dgs3 :: Person
bundespraesident = P (Vorname "Alexander") (Nachname "van der Bellen") dgs4 :: Person
sputnik_person = P (Vorname "Alexander") (Nachname "van der Bellen") dgs5 :: Person
antigen_person = P (Vorname "Alexander") (Nachname "van der Bellen") dgs6 :: Person
antigen_person_ungueltig = P (Vorname "Alexander") (Nachname "van der Bellen") dgs7 :: Person
bgm  = buergermeister
bm   = bundesminister
bk   = bundeskanzler
bp   = bundespraesident
kzp1 = ((D XXII Okt 2021),(U (Dreiviertel,Acht,NM)))
kzp2 = ((D XXVIII Okt 2021),(U (Dreiviertel,Acht,NM)))
kzp3 = ((D XXXI Nov 2021),(U (Dreiviertel,Acht,NM)))
kzp4 = ((D XXX Nov 2021),(U (Dreiviertel,Acht,NM)))

spec :: TestTree
spec =
  testGroup
    "Angabe2 Tests"
    [
        isValidDateTests,
        convertDateAndTimeTests,
        checkTestTests,
        einzulassenTests,
        einzulassendeTests,
        einzulassende_abzuweisendeTests,
        showTests
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
        testCase "einzulassen 4" $
          einzulassen (bgm,DreiG,kzp3) @?= Ungueltig,
        testCase "einzulassen 4a" $
          einzulassen (antigen_person,DreiG,kzp3) @?= Ungueltig,
        testCase "einzulassen 4b" $
          einzulassen (antigen_person_ungueltig,DreiG,kzp3) @?= Ungueltig,
        testCase "einzulassen antigen" $
          einzulassen (antigen_person,DreiG,kzp4) @?= Einlassen,
        testCase "einzulassen antigen ungueltig" $
          einzulassen (antigen_person_ungueltig,DreiG,kzp4) @?= Abweisen,

        testCase "einzulassen 5" $
          einzulassen (bm,DreiG,kzp1) @?= Abweisen,
        testCase "einzulassen 6" $
          einzulassen (bm,DreiG,kzp2) @?= Abweisen,
        testCase "einzulassen 7" $
          einzulassen (bm,DreiG,kzp3) @?= Ungueltig,

        testCase "einzulassen 8" $
          einzulassen (bk,DreiG,kzp1) @?= Einlassen,
        testCase "einzulassen 9" $
          einzulassen (bk,DreiG,kzp2) @?= Einlassen,
        testCase "einzulassen 10" $
          einzulassen (bk,DreiG,kzp3) @?= Ungueltig,

        testCase "einzulassen 11" $
          einzulassen (bp,ZweieinhalbG,kzp1) @?= Abweisen,
        testCase "einzulassen 12" $
          einzulassen (bp,ZweieinhalbG,kzp2) @?= Abweisen,
        testCase "einzulassen 13" $
          einzulassen (bp,ZweieinhalbG,kzp3) @?= Ungueltig
    ]


einzulassendeTests :: TestTree
einzulassendeTests =
  testGroup
    "einzulassende Tests"
    [ 
      testCase "einzulassende 1" $
        einzulassende [bgm, bm, bk] DreiG kzp2 @?= ["Alexander Schallenberg"]
    ]

einzulassende_abzuweisendeTests :: TestTree
einzulassende_abzuweisendeTests =
  testGroup
    "einzulassende_abzuweisende Tests"
    [ 
      testCase "einzulassende_abzuweisende 1" $
        einzulassende_abzuweisende [bgm, bm, bk, bp] ZweieinhalbG kzp1 @?= 
          (["Michael Ludwig","Alexander Schallenberg"],["Wolfgang Mueckstein","Alexander van der Bellen"])
    ]

showTests :: TestTree
showTests =
  testGroup
    "show Tests"
    [ 
      testCase "show 1" $
        show (U (Viertel,Zwoelf,VM)) @?= "11:15 Uhr",
      testCase "show 2" $
        show (U (Viertel,Zwoelf,NM)) @?= "23:15 Uhr",
      testCase "show 3" $
        show (U (Dreiviertel,Zwoelf,VM)) @?= "11:45 Uhr",
      testCase "show 4" $
        show (U (Dreiviertel,Zwoelf,NM)) @?= "23:45 Uhr" ,
      testCase "show 5" $
        show (U (Schlag,Zwoelf,VM)) @?= "12:00 Uhr",
      testCase "show 6" $
        show (U (Schlag,Zwoelf,NM)) @?= "24:00 Uhr",
      testCase "show 7" $
        show (U (Halb,Sechs,VM)) @?= "05:30 Uhr",
      testCase "show 8" $
        show (U (Halb,Sechs,NM)) @?= "17:30 Uhr",
      testCase "show 9" $
        show (D XXII Okt 2021) @?= "22.10.2021",
      testCase "show 10" $
        show (D XXIV Dez 2412) @?= "24.12.2412",
      testCase "show 11" $
        show (D I Jan 1) @?= "1.1.1",
      testCase "show 12" $
        show (D V Feb 54321) @?= "5.2.54321",
      testCase "show 13" $
        show (D XXXI Feb 1234) @?= "Datum ungueltig"
    ]


isValidDateTests :: TestTree
isValidDateTests = 
  testGroup
    "isValidDate Tests"
    [
      testCase "isValidDate 1" $
        isValidDate (D XXXI Okt 2021) @?= True,
      testCase "isValidDate 2" $
        isValidDate (D XXX Okt 2021) @?= True,
      testCase "isValidDate 3" $
        isValidDate (D XXXI Sep 2021) @?= False,
      testCase "isValidDate 4" $
        isValidDate (D XXIX Feb 2021) @?= False,
      testCase "isValidDate 5" $
        isValidDate (D XXIX Feb 2020) @?= True,
      testCase "isValidDate 6" $
        isValidDate (D XXXI Nov 2021) @?= False,
      testCase "isValidDate 7" $
        isValidDate (D I Jan 2000) @?= True,
      testCase "isValidDate 8" $
        isValidDate (D XXX Dez 1999) @?= True
    ]

convertDateAndTimeTests :: TestTree
convertDateAndTimeTests =
  testGroup
    "convertDateAndTime Tests"
    [
      testCase "convertDateAndTime 1" $
        convertDateAndTime (D X Okt 2021) (U (Dreiviertel,Acht,NM)) @?= 1633895100,
      testCase "convertDateAndTime 2" $
        convertDateAndTime (D I Jan 1970) (U (Dreiviertel,Acht,NM)) @?= 71100,
      testCase "convertDateAndTime 3" $
        convertDateAndTime (D I Okt 1999) (U (Schlag,Zwoelf,NM)) @?= 938822400,
      testCase "convertDateAndTime 4" $
        convertDateAndTime (D XX Feb 2003) (U (Schlag,Acht,VM)) @?= 1045728000,
      testCase "convertDateAndTime 5" $
        convertDateAndTime (D I Mar 2020) (U (Halb,Eins,VM)) @?= 1583022600
    ]

  
stundeVHDSInSecondsTests :: TestTree
stundeVHDSInSecondsTests =
  testGroup
    "stundeVHDSInSeconds Tests"
    [
      testCase "stundeVHDSInSeconds 1" $
        stundeVHDSInSeconds (U (Dreiviertel,Acht,NM)) @?= 71100,
      testCase "convertDateAndTime 1" $
        stundeVHDSInSeconds (U (Dreiviertel,Acht,VM)) @?= 74700,
      testCase "convertDateAndTime 1" $
        stundeVHDSInSeconds (U (Schlag,Zwoelf,NM)) @?= 86400,
      testCase "convertDateAndTime 1" $
        stundeVHDSInSeconds (U (Schlag,Acht,VM)) @?= 1633898700,
      testCase "convertDateAndTime 1" $
        stundeVHDSInSeconds (U (Halb,Eins,VM)) @?= 1800
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