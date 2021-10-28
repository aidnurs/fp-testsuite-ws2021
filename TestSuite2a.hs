module TestSuite2a where

import Angabe2
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] spec

{-----TESTDATEN BEGINN-----}
ta = PCR :: Testart
dgs1 = Getestet PCR (D XX Okt 2021) (U (Viertel,Acht,VM)) :: DreiG_Status
dgs2 = Geimpft (BioNTec,Einmal) :: DreiG_Status
dgs3 = Genesen :: DreiG_Status
dgs4 = Getestet Antigen (D XXII Sep 2021) (U (Schlag,Acht,VM)) :: DreiG_Status
dsg5 = Getestet PCR (D XXX Dez 1999) (U (Schlag,Zwoelf,NM)) :: DreiG_Status
dsg6 = Getestet Antigen (D XXIX Feb 2020) (U (Dreiviertel,Eins,VM)) :: DreiG_Status
buergermeister = P (Vorname "Michael") (Nachname "Ludwig") dgs1 :: Person
bundesminister = P (Vorname "Wolfgang") (Nachname "Mueckstein") dgs2 :: Person
bundeskanzler = P (Vorname "Alexander") (Nachname "Schallenberg") dgs3 :: Person
bundespraesident = P (Vorname "Alexander") (Nachname "van der Bellen") dgs4 :: Person
testperson1 = P (Vorname "HAL") (Nachname "9000") dsg5 :: Person
testperson2 = P (Vorname "Deep") (Nachname "Thought") dsg6 :: Person
bgm = buergermeister
bm = bundesminister
bk = bundeskanzler
bp = bundespraesident
tp1 = testperson1
tp2 = testperson2
kzp1 = ((D XXII Okt 2021),(U (Dreiviertel,Acht,NM))) :: Kontrollzeitpunkt
kzp2 = ((D XXVIII Okt 2021),(U (Dreiviertel,Acht,NM))) :: Kontrollzeitpunkt
kzp3 = ((D XXXI Nov 2021),(U (Dreiviertel,Acht,NM))) :: Kontrollzeitpunkt
kzp4 = ((D XXX Dez 1999),(U (Schlag,Zwoelf,NM))) :: Kontrollzeitpunkt
kzp5 = ((D I Jan 2000),(U (Schlag,Zwoelf,NM))) :: Kontrollzeitpunkt
kzp6 = ((D I Mar 2020),(U (Dreiviertel,Eins,VM))) :: Kontrollzeitpunkt
{-----TESTDATEN ENDE-----}

spec :: TestTree
spec =
	testGroup
		"Test2 Spec"
		[
			einzulassen_tests,
			einzulassende_tests,
			einzulassende_abzuweisende_tests,
			showDatum_tests,
			showUhrzeit_tests
		]
		
einzulassen_tests :: TestTree
einzulassen_tests =
	testGroup
		"-----einzulassen Tests-----"
		[
			testCase "PCRgetestet, DreiG, Datum gültig" $
				einzulassen (bgm,DreiG,kzp1) @?= Einlassen,
			testCase "PCR - Ungetestet, DreiG, Datum gültig" $
				einzulassen (bgm,DreiG,kzp2) @?= Abweisen,
			testCase "PCR - Ungetestet, DreiG, Kontroll-Datum ungültig" $
				einzulassen (bgm,DreiG,kzp3) @?= Ungueltig,
			testCase "Geimpft - Status: Ungeschuetzt, DreiG, Datum gültig" $
				einzulassen (bm,DreiG,kzp1) @?= Abweisen,
			testCase "Geimpft - Status: Ungeschuetzt, DreiG, Datum gültig" $
				einzulassen (bm,DreiG,kzp2) @?= Abweisen,
			testCase "Geimpft - Status: Ungeschuetzt, DreiG, Datum ungültig" $
				einzulassen (bm,DreiG,kzp3) @?= Ungueltig,
			testCase "Genesen, DreiG, Datum gültig" $
				einzulassen (bk,DreiG,kzp1) @?= Einlassen,
			testCase "Genesen, DreiG, Datum gültig" $
				einzulassen (bk,DreiG,kzp2) @?= Einlassen,
			testCase "Genesen, DreiG, Datum ungültig" $
				einzulassen (bk,DreiG,kzp3) @?= Ungueltig,
			testCase "AG - Ungetestet, ZweieinhalbG, Datum gültig" $
				einzulassen (bp,ZweieinhalbG,kzp1) @?= Abweisen,
			testCase "AG - Ungetestet, ZweieinhalbG, Datum gültig" $
				einzulassen (bp,ZweieinhalbG,kzp2) @?= Abweisen,
			testCase "AG - Ungetestet, ZweieinhalbG, Datum ungültig" $
				einzulassen (bp,ZweieinhalbG,kzp3) @?= Ungueltig,
			testCase "PCRgetestet, ZweeinhalbiG, Datum gültig" $
				einzulassen (tp1,ZweieinhalbG,kzp4) @?= Einlassen,
			testCase "PCRgetestet, ZweieinhalbG, kzp == tzp, Datum gültig" $
				einzulassen (tp1,ZweieinhalbG,kzp5) @?= Einlassen,
			testCase "AGgetestet, ZweiG, Datum gültig" $
				einzulassen (tp2,ZweiG,kzp6) @?= Abweisen,
			testCase "AGgetestet, DreiG, Datum gültig" $
				einzulassen (tp2,DreiG,kzp6) @?= Einlassen
		]
		
einzulassende_tests :: TestTree
einzulassende_tests =
	testGroup
		"-----einzulassende Tests-----"
		[
			testCase "2/4 Personen: [Michael Ludwig, Alexander Schallenberg]" $
				einzulassende [bgm,bm,bk,bp] DreiG kzp1 @?= ["Michael Ludwig","Alexander Schallenberg"],
			testCase "0/4 Personen: [], ungültiges Kontrolldatum" $
				einzulassende [bgm,bm,bk,bp] DreiG kzp3 @?= []
		]
		
einzulassende_abzuweisende_tests :: TestTree
einzulassende_abzuweisende_tests =
	testGroup
		"-----einzulassende_abzuweisende Tests-----"
		[
			testCase "2 Einlassen, 2 Abweisen" $
				einzulassende_abzuweisende [bgm,bm,bk,bp] DreiG kzp1 @?= (["Michael Ludwig","Alexander Schallenberg"],["Wolfgang Mueckstein","Alexander van der Bellen"]),
			testCase "0 Einlassen, 0 Abweisen, ungültiges Kontrolldatum" $
				einzulassende_abzuweisende [bgm,bm,bk,bp] DreiG kzp3 @?= ([],[])
		]

showDatum_tests :: TestTree
showDatum_tests =
	testGroup
		"-----show Datum Tests-----"
		[
			testCase "XXII Okt 2021" $
				show (D XXII Okt 2021) @?= "22.10.2021",
			testCase "XXIV Dez 2412" $
				show (D XXIV Dez 2412) @?= "24.12.2412",
			testCase "XXXI Feb 1234" $
				show (D XXXI Feb 1234) @?= "Datum ungueltig",
			testCase "XXIX Feb 2000" $
				show (D XXIX Feb 2000) @?= "29.2.2000",
			testCase "XXIX Feb 1900" $
				show (D XXIX Feb 1900) @?= "Datum ungueltig",
			testCase "XXIX Feb 2004" $
				show (D XXIX Feb 2004) @?= "29.2.2004",
			testCase "XXXI Jun 2021" $
				show (D XXXI Jun 2021) @?= "Datum ungueltig"
		]
		
showUhrzeit_tests :: TestTree
showUhrzeit_tests =
	testGroup
		"-----show Uhrzeit Tests-----"
		[
			testCase "Viertel, Zwoelf, VM" $
				show (U (Viertel, Zwoelf, VM)) @?= "11:15 Uhr",
			testCase "Viertel, Zwoelf, NM" $
				show (U (Viertel, Zwoelf, NM)) @?= "23:15 Uhr",
			testCase "Dreiviertel, Zwoelf, VM" $
				show (U (Dreiviertel, Zwoelf, VM)) @?= "11:45 Uhr",
			testCase "Dreiviertel, Zwoelf, NM" $
				show (U (Dreiviertel, Zwoelf, NM)) @?= "23:45 Uhr",
			testCase "Schlag, Zwoelf, VM" $
				show (U (Schlag, Zwoelf, VM)) @?= "12:00 Uhr",
			testCase "Schlag, Zwoelf, NM" $
				show (U (Schlag, Zwoelf, NM)) @?= "24:00 Uhr",
			testCase "Halb, Sechs, VM" $
				show (U (Halb, Sechs, VM)) @?= "05:30 Uhr",
			testCase "Halb, Sechs, NM" $
				show (U (Halb, Sechs, NM)) @?= "17:30 Uhr",
			testCase "Viertel, Eins, VM" $
				show (U (Viertel, Eins, VM)) @?= "00:15 Uhr"
		]