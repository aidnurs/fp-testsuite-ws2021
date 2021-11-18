module TestSuite5a where

import Angabe5
import Control.Exception (ErrorCall (ErrorCallWithLocation), evaluate, try)
import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

main :: IO ()
main = defaultMainWithIngredients [consoleTestReporter] spec

{-----TESTDATEN BEGINN-----}
zahlraum010_liste = [N, I, II, III, IV, V, VI, VII, VIII, IX, X, F] :: [] Zahlraum_0_10

intMenge_1 = [0, 2, 3, 4] :: [] Int

intMenge_2 = [0, 2, 3, 5] :: [] Int

intMenge_3 = [3, 2] :: [] Int

intMultimenge = [1, 2, 2, 3] :: [] Int

zr010Menge_1 = [N, II, III, IV] :: [] Zahlraum_0_10

zr010Menge_2 = [N, II, III, V] :: [] Zahlraum_0_10

zr010Menge_3 = [III, II] :: [] Zahlraum_0_10

zr010Multimenge = [I, II, II, III] :: [] Zahlraum_0_10

fktMenge_1 = [Fkt abs, Fkt negate] :: [] Funktion

fktMenge_2 = [Fkt abs, Fkt signum] :: [] Funktion

fktMenge_3 = [Fkt absProxy] :: [] Funktion

fktMultimenge = [Fkt abs, Fkt absProxy, Fkt signum] :: [] Funktion

et010Multimenge_1 = [ET I, ET I, ET II, ET II, ET V] :: [] (ElemTyp Zahlraum_0_10)

et010Multimenge_1_2 = [ET I, ET I, ET II, ET II, ET V, ET VII] :: [] (ElemTyp Zahlraum_0_10)

et010Multimenge_2 = [ET II, ET V, ET I, ET I] :: [] (ElemTyp Zahlraum_0_10)

etFktMultimenge_1 = [ET (Fkt abs), ET (Fkt signum), ET (Fkt abs)] :: [] (ElemTyp Funktion)

etFktMultimenge_2 = [ET (Fkt absProxy), ET (Fkt signum), ET (Fkt negate), ET (Fkt signum)] :: [] (ElemTyp Funktion)

baum_1 = Knoten (Blatt V) II (Blatt V) :: (Baum Zahlraum_0_10)

baum_2 = Blatt V :: (Baum Zahlraum_0_10)

paar_1 = P (baum_1, N) :: (Paar (Baum Zahlraum_0_10) Zahlraum_0_10)

paar_2 = P (baum_2, V) :: (Paar (Baum Zahlraum_0_10) Zahlraum_0_10)

paar_3 = P (V, 5) :: (Paar Zahlraum_0_10 Int)

phETMenge_1 = [A baum_1, B paar_1, A baum_2, D (Fkt abs), E paar_3] :: [] (PH_ElemTyp (Baum Zahlraum_0_10) (Paar (Baum Zahlraum_0_10) Zahlraum_0_10) c (Funktion) (Paar Zahlraum_0_10 Int))

phETMenge_2 = [C baum_1, B paar_1] :: [] (PH_ElemTyp a (Paar (Baum Zahlraum_0_10) Zahlraum_0_10) (Baum Zahlraum_0_10) d e)

phETMenge_3 = [A baum_1] :: [] (PH_ElemTyp (Baum Zahlraum_0_10) b c d e)

phETMenge_4 = [B paar_1] :: [] (PH_ElemTyp a (Paar (Baum Zahlraum_0_10) Zahlraum_0_10) c d e)

phETMultiMenge_1 = [R fktMenge_1, R fktMenge_2, S X, S X] :: [] (PH_ElemTyp' q ([Funktion]) (Zahlraum_0_10))

phETMultiMenge_2 = [Q fktMenge_1, R fktMenge_2] :: [] (PH_ElemTyp' ([Funktion]) ([Funktion]) s)

phETMultiMenge_3 = [R et010Multimenge_1, S X, S X] :: [] (PH_ElemTyp' q ([ElemTyp Zahlraum_0_10]) (Zahlraum_0_10))

phETMultiMenge_4 = [S X, R et010Multimenge_1, S X] :: [] (PH_ElemTyp' q ([ElemTyp Zahlraum_0_10]) (Zahlraum_0_10))

absProxy :: Zahlraum_0_10 -> Zahlraum_0_10
absProxy x = abs x

{-----TESTDATEN ENDE-----}

-- Error Handling
getMsg :: ErrorCall -> String
getMsg (ErrorCallWithLocation msg _) = msg

assertError :: (Show a) => String -> a -> IO ()
assertError errorMsg action = do
  r <- try (evaluate action)
  case r of
    Left e ->
      if (getMsg e == errorMsg)
        then return ()
        else assertFailure $ "Received unexpected error: " ++ (show e) ++ "\ninstead of: " ++ errorMsg
    Right _ -> assertFailure $ "Expected error: " ++ errorMsg

{--BEGINN TESTTREE--}
spec :: TestTree
spec =
  testGroup
    "Test5 Spec"
    [ zahlraum_0_10_tests,
      funktionTests,
      mengeVonIntTests,
      mengeVonZahlraumTests,
      mengeVonFunktionTests,
      mengeVonElemTypTests,
      phElemTypTests
      -- ph_elemtyp'_tests
    ]

zahlraum_0_10_tests :: TestTree
zahlraum_0_10_tests =
  testGroup
    "-Zahlraum_0_10 instance Num Tests----------"
    [ testCase "plus: I + II -> III" $
        I + II @?= III,
      testCase "plus: I + X -> F" $
        I + X @?= F,
      testCase "minus: I - I -> N" $
        I - I @?= N,
      testCase "minus: N - V -> F" $
        N - V @?= F,
      testCase "mal: N * V -> N" $
        N * V @?= N,
      testCase "mal: V * V -> F" $
        V * V @?= F,
      testCase "mal: V * II -> X" $
        V * II @?= X,
      testCase "negate: [N,I..X,F] -> [N,F..F]" $
        [negate n | n <- zahlraum010_liste] @?= [N, F, F, F, F, F, F, F, F, F, F, F],
      testCase "abs: [N,I..X,F] -> [N,I..X,F]" $
        [abs n | n <- zahlraum010_liste] @?= zahlraum010_liste,
      testCase "signum: [N,I..X,F] -> [N,I..I,F]" $
        [signum n | n <- zahlraum010_liste] @?= [N, I, I, I, I, I, I, I, I, I, I, F],
      testCase "fromInteger: [0..11] -> [N,I..X,F]" $
        [fromInteger i | i <- [0 .. 11]] @?= zahlraum010_liste
    ]

funktionTests :: TestTree
funktionTests =
  testGroup
    "-Funktion instance Eq, Show Tests----------"
    [ testCase "abs == absProxy -> True" $
        Fkt abs == Fkt absProxy @?= True,
      testCase "signum == negate -> False" $
        Fkt signum == Fkt negate @?= False,
      testCase "abs /= abs -> False" $
        Fkt abs /= Fkt abs @?= False,
      testCase "show (Fkt signum)" $
        show (Fkt signum) @?= "{(N,N),(I,I),(II,I),(III,I),(IV,I),(V,I),(VI,I),(VII,I),(VIII,I),(IX,I),(X,I),(F,F)}"
    ]

mengeVonIntTests :: TestTree
mengeVonIntTests =
  testGroup
    "-Menge von Int Tests----------"
    [ testCase "vereinige: intMenge_1 intMenge_2" $
        (vereinige intMenge_1 intMenge_2) \\ (union intMenge_2 intMenge_1) @?= [],
      testCase "schneide: intMenge_1 intMenge_2" $
        (schneide intMenge_1 intMenge_2) \\ (nub (intersect intMenge_2 intMenge_1)) @?= [],
      testCase "ziehe_ab: intMenge_1 intMenge_2" $
        (ziehe_ab intMenge_1 intMenge_2) \\ (intMenge_1 \\ intMenge_2) @?= [],
      testCase "ist_obermenge: intMenge_1 intMenge_3" $
        ist_obermenge intMenge_1 intMenge_3 @?= True,
      testCase "ist_obermenge: intMenge_3 intMenge_1" $
        ist_obermenge intMenge_3 intMenge_1 @?= False,
      testCase "anzahl: 2 intMultimenge -> Fehler" $
        assertError "Fehler" (anzahl 2 intMultimenge),
      testCase "anzahl: 9 intMenge_1" $
        anzahl 9 intMenge_1 @?= 0
    ]

mengeVonZahlraumTests :: TestTree
mengeVonZahlraumTests =
  testGroup
    "-Menge von Zahlraum_0_10 Tests----------"
    [ testCase "vereinige: zr010Menge_1 zr010Menge_2" $
        (vereinige zr010Menge_1 zr010Menge_2) \\ (union zr010Menge_2 zr010Menge_1) @?= [],
      testCase "schneide: zr010Menge_1 zr010Menge_2" $
        (schneide zr010Menge_1 zr010Menge_2) \\ (nub (intersect zr010Menge_2 zr010Menge_1)) @?= [],
      testCase "ziehe_ab: zr010Menge_1 zr010Menge_2" $
        (ziehe_ab zr010Menge_1 zr010Menge_2) \\ (zr010Menge_1 \\ zr010Menge_2) @?= [],
      testCase "ist_obermenge: zr010Menge_1 zr010Menge_3" $
        ist_obermenge zr010Menge_1 zr010Menge_3 @?= True,
      testCase "ist_obermenge: zr010Menge_3 zr010Menge_1" $
        ist_obermenge zr010Menge_3 zr010Menge_1 @?= False,
      testCase "anzahl: II zr010Multimenge -> Fehler" $
        assertError "Fehler" (anzahl II zr010Multimenge),
      testCase "anzahl: N zr010Menge_1" $
        anzahl N zr010Menge_1 @?= 1
    ]

mengeVonFunktionTests :: TestTree
mengeVonFunktionTests =
  testGroup
    "-Menge von Funktion Tests----------"
    [ testCase "vereinige: fktMenge_1 fktMenge_2" $
        (vereinige fktMenge_1 fktMenge_2) \\ (union fktMenge_2 fktMenge_1) @?= [],
      testCase "schneide: fktMenge_1 fktMenge_2" $
        (schneide fktMenge_1 fktMenge_2) \\ (nub (intersect fktMenge_2 fktMenge_1)) @?= [],
      testCase "ziehe_ab: fktMenge_1 fktMenge_3" $
        ziehe_ab fktMenge_1 fktMenge_3 @?= [Fkt negate],
      testCase "ist_obermenge: fktMenge_1 fktMenge_3" $
        ist_obermenge fktMenge_1 fktMenge_3 @?= True,
      testCase "ist_obermenge: fktMenge_1 fktMenge_2" $
        ist_obermenge fktMenge_1 fktMenge_2 @?= False,
      testCase "anzahl: abs fktMultimenge" $
        assertError "Fehler" (anzahl (Fkt abs) fktMultimenge),
      testCase "anzahl: abs fktMenge_1" $
        anzahl (Fkt abs) fktMenge_1 @?= 1
    ]

mengeVonElemTypTests :: TestTree
mengeVonElemTypTests =
  testGroup
    "-Menge von ElemTyp Tests----------"
    [ testCase "vereinige ET 010: et010Multimenge_1 et010Multimenge_2" $
        vereinige et010Multimenge_1 et010Multimenge_2 @?= et010Multimenge_1 ++ et010Multimenge_2,
      testCase "vereinige ET Fkt: etFktMultimenge_1 etFktMultimenge_2" $
        vereinige etFktMultimenge_1 etFktMultimenge_2 @?= etFktMultimenge_1 ++ etFktMultimenge_2,
      testCase "schneide ET 010: et010Multimenge_1 et010Multimenge_2" $
        schneide et010Multimenge_1 et010Multimenge_2 @?= [ET I, ET I, ET II, ET V],
      testCase "sind_gleich: (et010MM_1 n et010MM_2) (et010MM_2 n et010MM_1) -> True" $
        sind_gleich (schneide et010Multimenge_1 et010Multimenge_2) (schneide et010Multimenge_2 et010Multimenge_1) @?= True,
      testCase "schneide ET Fkt: etFktMultimenge_1 etFktMultimenge_2" $
        schneide etFktMultimenge_1 etFktMultimenge_2 @?= [ET (Fkt signum), ET (Fkt abs)],
      testCase "ziehe_ab ET Fkt: et010Multimenge_1_2 et010Multimenge_2" $
        ziehe_ab et010Multimenge_1_2 et010Multimenge_2 @?= [ET II, ET VII],
      testCase "ziehe_ab ET Fkt: etFktMultimenge_1 etFktMultimenge_2" $
        ziehe_ab etFktMultimenge_1 etFktMultimenge_2 @?= [ET (Fkt abs)],
      testCase "ist_obermenge ET 010: et010Multimenge_1 et010Multimenge_2" $
        ist_obermenge et010Multimenge_1 et010Multimenge_2 @?= True,
      testCase "ist_obermenge ET Fkt: etFktMultimenge_1 etFktMultimenge_2" $
        ist_obermenge etFktMultimenge_1 etFktMultimenge_2 @?= False,
      testCase "anzahl ET Fkt: abs etFktMultimenge_2" $
        anzahl (ET (Fkt abs)) etFktMultimenge_2 @?= 1,
      testCase "anzahl ET Fkt: abs etFktMultimenge_1" $
        anzahl (ET (Fkt abs)) etFktMultimenge_1 @?= 2
    ]

phElemTypTests :: TestTree
phElemTypTests =
  testGroup
    "-Menge von PH_ElemTyp Tests----------"
    [ testCase "vereinige: phETMenge_1 phETMenge_2" $
        (vereinige phETMenge_1 phETMenge_2) \\ (union phETMenge_1 phETMenge_2) @?= [],
      testCase "schneide: phETMenge_1 phETMenge_2" $
        schneide phETMenge_1 phETMenge_2 @?= [B paar_1]
        -- testCase "sind_gleich: phETMenge_3 phETMenge_4" $
        --   sind_gleich phETMenge_3 phETMenge_4 @?= False,
        -- testCase "ist_obermenge: phETMenge_1 phETMenge_3" $
        --   ist_obermenge phETMenge_1 phETMenge_3 @?= True,
        -- testCase "ist_obermenge: phETMenge_1 phETMenge_4" $
        --   ist_obermenge phETMenge_1 phETMenge_4 @?= False
    ]

-- ph_elemtyp'_tests :: TestTree
-- ph_elemtyp'_tests =
--   testGroup
--     "-Menge von PH_ElemTyp' Tests----------"
--     [ testCase "vereinige: phETMultiMenge_1 phETMultiMenge_2" $
--         vereinige phETMultiMenge_1 phETMultiMenge_2 @?= phETMultiMenge_1 ++ phETMultiMenge_2,
--       testCase "schneide: phETMultiMenge_1 phETMultiMenge_2" $
--         schneide phETMultiMenge_1 phETMultiMenge_2 @?= [R fktMenge_2],
--       testCase "ziehe_ab: phETMultiMenge_1 phETMultiMenge_2" $
--         ziehe_ab phETMultiMenge_1 phETMultiMenge_2 @?= [R fktMenge_1, S X, S X]
--         {-testCase "anzahl: (S X) phETMultiMenge_1" $
--         anzahl (S X) phETMultiMenge_1 @?= 2,
--          testCase "anzahl: (Q phETMenge_1) phETMultiMenge_1" $
--         anzahl (R fktMenge_1) phETMultiMenge_1 @?= 1,
--          testCase "sind_gleich: phETMultiMenge_3 phETMultiMenge_4" $
--         sind_gleich phETMultiMenge_3 phETMultiMenge_4 @?= True-}
--     ]
