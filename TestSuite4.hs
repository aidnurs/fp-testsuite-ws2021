module TestSuite4 where

import Angabe4
import Test.Tasty
import Test.Tasty.HUnit

gp1 :: Geschaeftspartner
gp1 = GP "Zuckerberg" (D XXXI Feb 2020)

gp1_fixed :: Geschaeftspartner
gp1_fixed = GP "Zuckerberg" (D I Mar 2020)

gp1_fixed_same_name :: Geschaeftspartner
gp1_fixed_same_name = GP "Zuckerberg" (D I Mar 2019)

gp1_fixed_same_name_diff_date :: Geschaeftspartner
gp1_fixed_same_name_diff_date = GP "Zuckerberg" (D I Mar 2016)

gp2 :: Geschaeftspartner
gp2 = GP "Gates" (D XXXI Nov 2016)

gp2_fixed :: Geschaeftspartner
gp2_fixed = GP "Gates" (D I Dez 2016)

gp3 :: Geschaeftspartner
gp3 = GP "Bezos" (D XXIII Feb 2019)

gp3_fixed :: Geschaeftspartner
gp3_fixed = GP "Bezos" (D XXIII Feb 2019)

gp4 :: Geschaeftspartner
gp4 = GP "Jobs" (D I Okt 2012)

gp4_fixed :: Geschaeftspartner
gp4_fixed = GP "Jobs" (D I Okt 2012)

gv1 :: Geschaeftsvorfall
gv1 = Zahlung (C 112) KeinSkonto (D XXXI Feb 2020)

gv1_fixed :: AP_Geschaeftsvorfall
gv1_fixed = AP_Zahlung (C 112) (D I Mar 2020)

gv1_fixed_euro :: K_Geschaeftsvorfall
gv1_fixed_euro = K_Zahlung (EC 1 12) (D I Mar 2020)

gv2 :: Geschaeftsvorfall
gv2 = Zahlung (C 1340) DreiProzent (D XII Apr 2020)

gv2_fixed :: AP_Geschaeftsvorfall
gv2_fixed = AP_Zahlung (C 1300) (D XII Apr 2020)

gv2_fixed_euro :: K_Geschaeftsvorfall
gv2_fixed_euro = K_Zahlung (EC 13 00) (D XII Apr 2020)

gv3 :: Geschaeftsvorfall
gv3 = Zahlung (C 5) FuenfProzent (D XXXI Jul 2020)

gv3_fixed :: AP_Geschaeftsvorfall
gv3_fixed = AP_Zahlung (C 5) (D XXXI Jul 2020)

gv3_fixed_euro :: K_Geschaeftsvorfall
gv3_fixed_euro = K_Zahlung (EC 0 5) (D XXXI Jul 2020)

gv4 :: Geschaeftsvorfall
gv4 = Gutschrift (C 100) (D XV Dez 2020)

gv4_fixed :: AP_Geschaeftsvorfall
gv4_fixed = P_Gutschrift (C 100) (D XV Dez 2020)

gv4_fixed_euro :: K_Geschaeftsvorfall
gv4_fixed_euro = K_Gutschrift (EC 1 0) (D XV Dez 2020)

gv4_fixed_euro_zahlung :: K_Geschaeftsvorfall
gv4_fixed_euro_zahlung = K_Zahlung (EC 1 0) (D XV Dez 2020)

eintrag1 :: Kassabucheintrag
eintrag1 = (gp1, gv1)

eintrag1_fixed :: AP_Kassabucheintrag
eintrag1_fixed = (gp1_fixed, gv1_fixed)

eintrag1_fixed_euro :: K_Kassabucheintrag
eintrag1_fixed_euro = (gp1_fixed, gv1_fixed_euro)

eintrag2 :: Kassabucheintrag
eintrag2 = (gp2, gv2)

eintrag2_fixed :: AP_Kassabucheintrag
eintrag2_fixed = (gp2_fixed, gv2_fixed)

eintrag2_fixed_euro :: K_Kassabucheintrag
eintrag2_fixed_euro = (gp2_fixed, gv2_fixed_euro)

eintrag3 :: Kassabucheintrag
eintrag3 = (gp3, gv3)

eintrag3_fixed :: AP_Kassabucheintrag
eintrag3_fixed = (gp3_fixed, gv3_fixed)

eintrag3_fixed_euro :: K_Kassabucheintrag
eintrag3_fixed_euro = (gp3_fixed, gv3_fixed_euro)

eintrag4 :: Kassabucheintrag
eintrag4 = (gp4, gv4)

eintrag4_fixed :: AP_Kassabucheintrag
eintrag4_fixed = (gp4_fixed, gv4_fixed)

eintrag4_fixed_euro :: K_Kassabucheintrag
eintrag4_fixed_euro = (gp4_fixed, gv4_fixed_euro)

eintrag4_gp1 :: Kassabucheintrag
eintrag4_gp1 = (gp1, gv4)

eintrag4_gp1_fixed :: AP_Kassabucheintrag
eintrag4_gp1_fixed = (gp1_fixed, gv4_fixed)

eintrag4_gp1_fixed_euro :: K_Kassabucheintrag
eintrag4_gp1_fixed_euro = (gp1_fixed, gv4_fixed_euro)

kb1 :: Kassabuch
kb1 = KB [eintrag1, eintrag2, eintrag3, (gp1_fixed_same_name, gv1)]

kb1_1 :: Kassabuch
kb1_1 = KB [eintrag1, eintrag2, eintrag3, eintrag1]

kkb1 :: KonsolidiertesKassabuch
kkb1 = KKB [eintrag1_fixed_euro, eintrag2_fixed_euro, eintrag3_fixed_euro, (gp1_fixed_same_name, gv1_fixed_euro)]

skb1 :: SaldiertesKassabuch
skb1 = SKB [saldo_eintrag3, saldo_eintrag2, saldo_eintrag1]

kkb2 :: KonsolidiertesKassabuch
kkb2 = KKB [eintrag1_fixed_euro, eintrag2_fixed_euro, eintrag3_fixed_euro, eintrag4_gp1_fixed_euro]

kkb3 :: KonsolidiertesKassabuch
kkb3 = KKB [eintrag4_gp1_fixed_euro]

kkb4 :: KonsolidiertesKassabuch
kkb4 = KKB [eintrag4_gp1_fixed_euro, (gp1_fixed, gv4_fixed_euro_zahlung), (gp1_fixed_same_name, gv4_fixed_euro_zahlung)]

saldo_eintrag1 :: (Geschaeftspartner, Saldo)
saldo_eintrag1 = (gp1, Zahlungssaldo (EC 2 24))

saldo_eintrag2 :: (Geschaeftspartner, Saldo)
saldo_eintrag2 = (gp2, Zahlungssaldo (EC 13 00))

saldo_eintrag3 :: (Geschaeftspartner, Saldo)
saldo_eintrag3 = (gp3, Zahlungssaldo (EC 0 5))

saldo_eintrag4 :: (Geschaeftspartner, Saldo)
saldo_eintrag4 = (gp4, Keine_Geschaeftsbeziehung)

spec :: TestTree
spec =
  testGroup
    "Angabe4 Tests"
    [ waupTests,
      konsolidiereTests,
      saldoTests,
      saldiereTests,
      sortTests,
      filterGPTests,
      utilsTests
    ]

waupTests :: TestTree
waupTests =
  testGroup
    "waup Tests"
    [ testCase "waup 1" $
        waup eintrag1 @?= eintrag1_fixed,
      testCase "waup 2" $
        waup eintrag2 @?= eintrag2_fixed,
      testCase "waup 3" $
        waup eintrag3 @?= eintrag3_fixed,
      testCase "waup 4" $
        waup eintrag4 @?= eintrag4_fixed
    ]

konsolidiereTests :: TestTree
konsolidiereTests =
  testGroup
    "konsolidiere Tests"
    [ testCase "konsolidiere 1" $
        konsolidiere kb1 @?= kkb1,
      testCase "konsolidiere 2" $
        konsolidiere (KB []) @?= KKB []
    ]

saldoTests :: TestTree
saldoTests =
  testGroup
    "saldo Tests"
    [ testCase "saldo 1" $
        saldo gp1_fixed kkb2 @?= Zahlungssaldo (EC 0 12),
      testCase "saldo 2" $
        saldo gp1_fixed kkb3 @?= Forderungssaldo (EC 1 0),
      testCase "saldo 3" $
        saldo gp1_fixed kkb4 @?= Ausgeglichen,
      testCase "saldo 4" $
        saldo gp4_fixed kkb2 @?= Keine_Geschaeftsbeziehung,
      testCase "saldo 5" $
        saldo gp4_fixed (KKB []) @?= Keine_Geschaeftsbeziehung
    ]

saldiereTests :: TestTree
saldiereTests =
  testGroup
    "saldiere Tests"
    [ testCase "saldiere 1" $
        saldiere kb1_1 @?= skb1,
      testCase "saldiere 2" $
        saldiere (KB []) @?= SKB []
    ]

sortTests :: TestTree
sortTests =
  testGroup
    "sort Tests"
    [ testCase "sort 1" $
        sortKKassabuch ([]) @?= [],
      testCase "sort 2" $
        sortKKassabuch ([saldo_eintrag1, saldo_eintrag2, saldo_eintrag3, saldo_eintrag4]) @?= [saldo_eintrag3, saldo_eintrag2, saldo_eintrag4, saldo_eintrag1]
    ]

filterGPTests :: TestTree
filterGPTests =
  testGroup
    "filterGP Tests"
    [ testCase "filterGP 1" $
        filterGP ([gp1, gp1, gp1_fixed_same_name]) @?= [gp1, gp1_fixed_same_name]
    ]

utilsTests :: TestTree
utilsTests =
  testGroup
    "utils Tests"
    [ testCase "compareSKEntry 1" $
        compareSKEntry (gp1, Forderungssaldo (EC 1 0)) (gp1, Forderungssaldo (EC 1 0)) @?= EQ,
      testCase "compareSKEntry 2" $
        compareSKEntry (gp1_fixed, Forderungssaldo (EC 1 0)) (GP "Zuckerberg" (D I Mar 2020), Forderungssaldo (EC 1 0)) @?= EQ,
      testCase "compareSKEntry 3" $
        compareSKEntry (gp1_fixed, Forderungssaldo (EC 1 0)) (GP "Zuckerberg" (D II Apr 2020), Forderungssaldo (EC 1 0)) @?= LT,
      testCase "compareSKEntry 4" $
        compareSKEntry (gp1_fixed, Forderungssaldo (EC 1 0)) (GP "Zuckerberg" (D II Mar 2020), Forderungssaldo (EC 1 0)) @?= LT,
      testCase "compareSKEntry 5" $
        compareSKEntry (gp1_fixed, Forderungssaldo (EC 1 0)) (GP "Zuckerberg" (D II Mar 2019), Forderungssaldo (EC 1 0)) @?= GT,
      testCase "compareSKEntry 5" $
        compareSKEntry (gp1_fixed, Forderungssaldo (EC 1 0)) (GP "Auckerberg" (D II Mar 2019), Forderungssaldo (EC 1 0)) @?= GT,
      testCase "isLeapYear 1" $
        isLeapYear 2021 @?= False,
      testCase "isLeapYear 2" $
        isLeapYear 2020 @?= True,
      testCase "isLeapYear 3" $
        isLeapYear 2016 @?= True,
      testCase "convertDate 1" $
        convertDate (D XXXI Dez 2021) @?= D XXXI Dez 2021,
      testCase "convertDate 2" $
        convertDate (D XXXI Nov 2021) @?= D I Dez 2021,
      testCase "convertDate 3" $
        convertDate (D XX Feb 2021) @?= D XX Feb 2021,
      testCase "convertDate 3" $
        convertDate (D XXX Feb 2021) @?= D I Mar 2021,
      testCase "centToEuro 1" $
        centToEuro (C 152) @?= EC 1 52,
      testCase "centToEuro 2" $
        centToEuro (C 0) @?= EC 0 0,
      testCase "centToEuro 3" $
        centToEuro (C 400) @?= EC 4 00,
      testCase "centToEuro 4" $
        centToEuro (C 401) @?= EC 4 01,
      testCase "sumKassabuchToSignedCents 1" $
        sumKassabuchToSignedCents kkb2 @?= -1317,
      testCase "signedVorfallAmount 1" $
        signedVorfallAmount gv1_fixed_euro @?= -112,
      testCase "signedVorfallAmount 1" $
        signedVorfallAmount gv4_fixed_euro @?= 100
    ]