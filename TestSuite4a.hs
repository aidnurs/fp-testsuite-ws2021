module TestSuite4a where

import Angabe4
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] spec

{-----TESTDATEN BEGINN-----}

-- Geschaeftspartner
maxM = GP "Max" datum_1_r

maxM2 = GP "Max" datum_2_r

maxMar1 = GP "Max" datum_1_Mar_r

anna = GP "Annaa" datum_2_r

anna2 = GP "Annab" datum_2_r

--KBE Zahlung Normalfälle
kbe_Zahlung_sk = (maxM2, zahlung_sk)

kbe_Zahlung_s3 = (maxM, zahlung_s3)

kbe_Zahlung_s5 = (anna, zahlung_s5)

kbe_Zahlung_s10 = (anna, zahlung_s10)

kbe_Zahlung_sk_Antw = (maxM2, zahlung_sk_ap)

kbe_Zahlung_s3_Antw = (maxM, zahlung_s3_ap)

kbe_Zahlung_s5_Antw = (anna, zahlung_s5_ap)

kbe_Zahlung_s10_Antw = (anna, zahlung_s10_ap)

k_kbe_Zahlung_sk_Antw = (maxM2, k_zahlung_sk_ap)

k_kbe_Zahlung_s3_Antw = (maxM, k_zahlung_s3_ap)

k_kbe_Zahlung_s5_Antw = (anna, k_zahlung_s5_ap)

k_kbe_Zahlung_s10_Antw = (anna, k_zahlung_s10_ap)

zahlung_sk = Zahlung brut skonto_k datum_1_r

zahlung_s3 = Zahlung brut skonto_3 datum_1_r

zahlung_s5 = Zahlung brut skonto_5 datum_1_r

zahlung_s10 = Zahlung brut skonto_10 datum_1_r

zahlung_sk_ap = AP_Zahlung brut datum_1_r

zahlung_s3_ap = AP_Zahlung (C (55005 -1650)) datum_1_r

zahlung_s5_ap = AP_Zahlung (C (55005 -2750)) datum_1_r

zahlung_s10_ap = AP_Zahlung (C (55005 -5500)) datum_1_r

k_zahlung_sk_ap = K_Zahlung (EC 550 5) datum_1_r

k_zahlung_s3_ap = K_Zahlung (EC 533 55) datum_1_r

k_zahlung_s5_ap = K_Zahlung (EC 522 55) datum_1_r

k_zahlung_s10_ap = K_Zahlung (EC 495 5) datum_1_r

k_kbe_Zahlung_sk_EC10 = (maxM, k_zahlung_sk_EC10)

k_zahlung_sk_EC10 = K_Zahlung (EC 10 0) datum_1_r

k_kbe_Zahlung_sk_EC70_5 = (maxM, k_zahlung_sk_EC70_5)

k_zahlung_sk_EC70_5 = K_Zahlung (EC 70 5) datum_1_r

brut = C 55005

gutschriftsb = C 1000

skonto_k = KeinSkonto

skonto_3 = DreiProzent

skonto_5 = FuenfProzent

skonto_10 = ZehnProzent

--allgemeine Gutschrift
gutschrift1 = Gutschrift gutschriftsb datum_1_r

gutschrift1_ap = P_Gutschrift gutschriftsb datum_1_r

k_gutschrift1_ap = K_Gutschrift (EC 10 0) datum_1_r

--KBE Gutschrift Normalfall
kbe_Gutsch = (GP "Max" datum_1_r, gutschrift1)

kbe_Gutsch_Antw = (GP "Max" datum_1_r, gutschrift1_ap)

k_kbe_Gutsch_Antw = (GP "Max" datum_1_r, k_gutschrift1_ap)

kbe_Gutsch_2 = (maxM2, gutschrift1)

kbe_Gutsch_2_Antw = (maxM2, gutschrift1_ap)

k_kbe_Gutsch_2_Antw = (maxM2, k_gutschrift1_ap)

kbe_Gutsch_anna1 = (anna, gutschrift1)

kbe_Gutsch_anna2 = (anna2, gutschrift1)

--KBE Datum nicht richtig - Feb

kbe_29_Feb_NSJ_ng_d = (GP "Max" datum_1_Mar_r, gutschrift1)

kbe_30_Feb_ng_d = (GP "Max" datum_30_2_ng, gutschrift1)

kbe_31_Feb_ng_d = (GP "Max" datum_31_2_ng, gutschrift1)

kbe_29_Feb_SJ_ng_d = (GP "Max" datum_29_2_SJ_r, gutschrift1)

kbe_1_Mar_r = (GP "Max" datum_1_Mar_r, gutschrift1_ap)

kbe_29_Feb_SJ_r = (GP "Max" datum_29_2_SJ_r, gutschrift1_ap)

k_kbe_1_Mar_r = (GP "Max" datum_1_Mar_r, k_gutschrift1_ap)

k_kbe_29_Feb_SJ_r = (GP "Max" datum_29_2_SJ_r, k_gutschrift1_ap)

--KBE Datum nicht richtig - 31.

kbe_Nov_ng_d = (GP "Max" datum_Nov_ng, gutschrift1)

kbe_Apr_ng_d = (GP "Max" datum_Apr_ng, gutschrift1)

kbe_Jun_ng_d = (GP "Max" datum_Jun_ng, gutschrift1)

kbe_Sep_ng_d = (GP "Max" datum_Sep_ng, gutschrift1)

kbe_Nov_r = (GP "Max" datum_Nov_r, gutschrift1_ap)

kbe_Apr_r = (GP "Max" datum_Apr_r, gutschrift1_ap)

kbe_Jun_r = (GP "Max" datum_Jun_r, gutschrift1_ap)

kbe_Sep_r = (GP "Max" datum_Sep_r, gutschrift1_ap)

k_kbe_Nov_r = (GP "Max" datum_Nov_r, k_gutschrift1_ap)

k_kbe_Apr_r = (GP "Max" datum_Apr_r, k_gutschrift1_ap)

k_kbe_Jun_r = (GP "Max" datum_Jun_r, k_gutschrift1_ap)

k_kbe_Sep_r = (GP "Max" datum_Sep_r, k_gutschrift1_ap)

--Datum Feb
datum_29_2_SJ_r = D XXIX Feb 2000

datum_29_2_NSJ_ng = D XXIX Feb 2001

datum_30_2_ng = D XXX Feb 2000

datum_31_2_ng = D XXXI Feb 2000

datum_1_Mar_r = D I Mar 2000

--Datum 31 falsch
datum_Nov_ng = D XXXI Nov 2000

datum_Apr_ng = D XXXI Apr 2000

datum_Jun_ng = D XXXI Jun 2000

datum_Sep_ng = D XXXI Sep 2000

datum_Nov_r = D I Dez 2000

datum_Apr_r = D I Mai 2000

datum_Jun_r = D I Jul 2000

datum_Sep_r = D I Okt 2000

-- Datum richtig
datum_1_r = D I Jan 2000

datum_2_r = D II Jun 2000

-- Kassabuch
kassabuc_Guts =
  KB
    [ kbe_Gutsch
    ]

kassabuc_Guts_Antw =
  KKB
    [ k_kbe_Gutsch_Antw
    ]

kassabuc_Guts_Feb =
  KB
    [ kbe_29_Feb_NSJ_ng_d,
      kbe_30_Feb_ng_d,
      kbe_31_Feb_ng_d,
      kbe_29_Feb_SJ_ng_d
    ]

kassabuc_Guts_Feb_Antw =
  KKB
    [ k_kbe_1_Mar_r,
      k_kbe_1_Mar_r,
      k_kbe_1_Mar_r,
      k_kbe_29_Feb_SJ_r
    ]

kassabuc_Guts_31 =
  KB
    [ kbe_Nov_ng_d,
      kbe_Apr_ng_d,
      kbe_Jun_ng_d,
      kbe_Sep_ng_d
    ]

kassabuc_Guts_31_Antw =
  KKB
    [ k_kbe_Nov_r,
      k_kbe_Apr_r,
      k_kbe_Jun_r,
      k_kbe_Sep_r
    ]

kassabuc_Zahl =
  KB
    [ kbe_Zahlung_sk,
      kbe_Zahlung_s3,
      kbe_Zahlung_s5,
      kbe_Zahlung_s10
    ]

kassabuc_Zahl_Antw =
  KKB
    [ k_kbe_Zahlung_sk_Antw,
      k_kbe_Zahlung_s3_Antw,
      k_kbe_Zahlung_s5_Antw,
      k_kbe_Zahlung_s10_Antw
    ]

--KKB fuer saldo
kassabuc_kein_Geschaftsb =
  KKB
    [ k_kbe_Zahlung_sk_Antw --GP maxM2
    ]

kassabuc_Ausgeglichen =
  KKB
    [ k_kbe_Zahlung_sk_Antw, --GP maxM2 (not counting)
      k_kbe_Zahlung_sk_EC10, --EC -10
      k_kbe_Gutsch_Antw --EC +10
    ]

kassabuc_Forderungssaldo_50 =
  KKB
    [ k_kbe_Zahlung_sk_Antw, --GP maxM2 (not counting)
      k_kbe_Gutsch_Antw,
      k_kbe_Gutsch_Antw,
      k_kbe_Gutsch_Antw,
      k_kbe_Gutsch_Antw,
      k_kbe_Gutsch_Antw
    ]

kassabuc_Zahlungssaldo_100_5 =
  KKB
    [ k_kbe_Zahlung_sk_Antw, --GP maxM2 (not counting)
      k_kbe_Zahlung_sk_EC10,
      k_kbe_Zahlung_sk_EC10,
      k_kbe_Zahlung_sk_EC10,
      k_kbe_Zahlung_sk_EC70_5
    ]

-- KB + SKB fuer saldiere
kassab_lexikographisch_einfach =
  KB
    [ kbe_Gutsch_anna2, --anna2
      kbe_Gutsch_anna2, --anna2
      -- anna2 Gutschrift +20
      kbe_Gutsch_anna1 --anna +10
      -- anna Zahlung 917 60
    ]

sKB_lexikographisch_einfach =
  SKB
    [ (anna, Forderungssaldo (EC 10 0)),
      (anna2, Forderungssaldo (EC 20 0))
    ]

kassab_lexikographisch =
  KB
    [ kbe_Gutsch_anna1, --anna +10
      kbe_Zahlung_s5, --anna -522 55
      kbe_Zahlung_s10, --anna  -495 5
      -- anna Zahlung 1007 60
      kbe_Gutsch_anna2 --anna2
      -- anna2 Gutschrift +10
    ]

sKB_lexikographisch =
  SKB
    [ (anna, Zahlungssaldo (EC 1007 60)),
      (anna2, Forderungssaldo (EC 10 0))
    ]

kassab_lexikographisch_datum_vergl =
  KB
    [ kbe_Gutsch_2, --maxM2, +10 Datum OK
      kbe_Zahlung_sk, -- maxM2 -550 5 Datum OK
      -- maxM2(2.Juni) Zahl 540 5
      kbe_29_Feb_NSJ_ng_d, --MaxMar1 +10 Datum NOK
      kbe_30_Feb_ng_d, --MaxMar1 +10 Datum NOK
      kbe_31_Feb_ng_d --MaxMar1 +10 Datum NOK
      -- maxMar1 Gutschrift 30 0
    ]

sKB_lexikographisch_datum_vergl =
  SKB
    [ (maxMar1, Forderungssaldo (EC 30 0)),
      (maxM2, Zahlungssaldo (EC 540 5))
    ]

{-----TESTDATEN ENDE-----}

spec :: TestTree
spec =
  testGroup
    "Test2 Spec"
    [ a1_waup_tests,
      a2_konsolidiere_tests,
      a3_saldo_tests,
      a4_saldiere_tests
    ]

a1_waup_tests :: TestTree
a1_waup_tests =
  testGroup
    "-----A1 waup Tests-----"
    [ -- NF Gutschrift
      testCase "(GP OK, Gutschrift OK)" $
        waup kbe_Gutsch @?= kbe_Gutsch_Antw,
      -- Feb test cases Gutschrift
      testCase "(GP -Datum NOK, Gutschrift OK) - Feb 31-> Mar 1" $
        waup kbe_31_Feb_ng_d @?= kbe_1_Mar_r,
      testCase "(GP -Datum NOK, Gutschrift OK) - Feb 30-> Mar 1" $
        waup kbe_30_Feb_ng_d @?= kbe_1_Mar_r,
      testCase "(GP -Datum NOK, Gutschrift OK) - Feb 29-> Mar 1" $
        waup kbe_29_Feb_NSJ_ng_d @?= kbe_1_Mar_r,
      testCase "(GP -Datum OK, Gutschrift OK)- Feb 29 -> Feb 29" $
        waup kbe_29_Feb_SJ_ng_d @?= kbe_29_Feb_SJ_r,
      -- 31. test cases Gutschrift
      testCase "(GP -Datum NOK, Gutschrift OK) - Nov 31-> Dez 1" $
        waup kbe_Nov_ng_d @?= kbe_Nov_r,
      testCase "(GP -Datum NOK, Gutschrift OK)- Sep 31-> Okt 1" $
        waup kbe_Sep_ng_d @?= kbe_Sep_r,
      testCase "(GP -Datum NOK, Gutschrift OK)- Jun 31-> Jul 1" $
        waup kbe_Jun_ng_d @?= kbe_Jun_r,
      testCase "(GP -Datum NOK, Gutschrift OK) - Apr 31-> Apr 1" $
        waup kbe_Apr_ng_d @?= kbe_Apr_r,
      -- NF Zahlung
      testCase "(GP OK, Zahlung (skonto keins) OK)" $
        waup kbe_Zahlung_sk @?= kbe_Zahlung_sk_Antw,
      testCase "(GP OK, Zahlung (skonto 3) OK)" $
        waup kbe_Zahlung_s3 @?= kbe_Zahlung_s3_Antw,
      testCase "(GP OK, Zahlung (skonto 5) OK)" $
        waup kbe_Zahlung_s5 @?= kbe_Zahlung_s5_Antw,
      testCase "(GP OK, Zahlung (skonto 10) OK)" $
        waup kbe_Zahlung_s10 @?= kbe_Zahlung_s10_Antw
    ]

a2_konsolidiere_tests :: TestTree
a2_konsolidiere_tests =
  testGroup
    "-----A2 konsolidiere Tests-----"
    [ -- NF Gutschrift
      testCase "kassabuc_Guts" $
        konsolidiere kassabuc_Guts @?= kassabuc_Guts_Antw,
      -- Feb test cases Gutschrift
      testCase "kassabuc_Guts_Feb" $
        konsolidiere kassabuc_Guts_Feb @?= kassabuc_Guts_Feb_Antw,
      -- 31. test cases Gutschrift
      testCase "kassabuc_Guts_31" $
        konsolidiere kassabuc_Guts_31 @?= kassabuc_Guts_31_Antw,
      -- NF Zahlung
      testCase "kassabuc_Zahl" $
        konsolidiere kassabuc_Zahl @?= kassabuc_Zahl_Antw
    ]

a3_saldo_tests :: TestTree
a3_saldo_tests =
  testGroup
    "-----A3 saldo Tests-----"
    [ testCase "Forderungssaldo_EC50" $
        saldo maxM kassabuc_Forderungssaldo_50 @?= Forderungssaldo (EC 50 0),
      testCase "Zahlungssaldo_EC100_5" $
        saldo maxM kassabuc_Zahlungssaldo_100_5 @?= Zahlungssaldo (EC 100 5),
      testCase "Ausgeglichen" $
        saldo maxM kassabuc_Ausgeglichen @?= Ausgeglichen,
      testCase "Keine Geschäftsbez" $
        saldo maxM kassabuc_kein_Geschaftsb @?= Keine_Geschaeftsbeziehung
    ]

a4_saldiere_tests :: TestTree
a4_saldiere_tests =
  testGroup
    "-----A4 saldiere Tests-----"
    [ testCase "lexikographisch_einfach" $
        saldiere kassab_lexikographisch_einfach @?= sKB_lexikographisch_einfach,
      testCase "lexikographisch" $
        saldiere kassab_lexikographisch @?= sKB_lexikographisch,
      testCase "lexikographisch + datumsvergl noetig" $
        saldiere kassab_lexikographisch_datum_vergl @?= sKB_lexikographisch_datum_vergl
    ]
