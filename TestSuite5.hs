module TestSuite5 where

import Angabe5
import Test.Tasty
import Test.Tasty.HUnit

-- funk :: Funktion
-- funk z = z + II

zero = N :: Zahlraum_0_10

one = I :: Zahlraum_0_10

two = II :: Zahlraum_0_10

three = III :: Zahlraum_0_10

four = IV :: Zahlraum_0_10

seven = VII :: Zahlraum_0_10

ten = X :: Zahlraum_0_10

zahlraum010_liste = [N, I, II, III, IV, V, VI, VII, VIII, IX, X, F] :: [] Zahlraum_0_10

spec :: TestTree
spec = testGroup "Angabe5 Tests" [showTests, numZahlraumTests]

numZahlraumTests :: TestTree
numZahlraumTests =
  testGroup
    "numZahlraum"
    [ testCase "Num Zahlraum 1" $
        zero * one @?= N,
      testCase "Num Zahlraum 2" $
        one * two @?= II,
      testCase "Num Zahlraum 3" $
        ten * two @?= F,
      testCase "Num Zahlraum 4" $
        seven + four @?= F,
      testCase "Num Zahlraum 5" $
        three - ten @?= F,
      testCase "Num Zahlraum 6" $
        ten - three @?= VII,
      testCase "Num Zahlraum 7" $
        ten - ten @?= N,
      testCase "Num Zahlraum 8" $
        four + one @?= V,
      testCase "negate: [N,I..X,F] -> [N,F..F]" $
        [negate n | n <- zahlraum010_liste] @?= [N, F, F, F, F, F, F, F, F, F, F, F],
      testCase "abs: [N,I..X,F] -> [N,I..X,F]" $
        [abs n | n <- zahlraum010_liste] @?= zahlraum010_liste,
      testCase "signum: [N,I..X,F] -> [N,I..I,F]" $
        [signum n | n <- zahlraum010_liste] @?= [N, I, I, I, I, I, I, I, I, I, I, F],
      testCase "fromInteger: [0..11] -> [N,I..X,F]" $
        [fromInteger i | i <- [0 .. 11]] @?= zahlraum010_liste
    ]

showTests :: TestTree
showTests =
  testGroup
    "Show Tests"
    [ testCase "placeholder" $ 1 @?= 1
    -- testCase "Show Funktion" $
    --   show funk @?= "{(N,II),(I,III),(II,IV), (III,V),(IV,VI),(V,VII),(VI,VIII),(VII,IX),(VIII,X),(IX,F),(X,F),(F,F)}"
    ]
