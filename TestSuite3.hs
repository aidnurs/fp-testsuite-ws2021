module TestSuite3 where

import Test.Tasty
import Test.Tasty.HUnit
import Prelude as P
import qualified Control.Exception as E
import qualified Control.Monad as M

import Angabe3

spec :: TestTree
spec =
  testGroup
    "Angabe3 Tests"
    [ 
      -- showTests,
      -- matrixtypTests,
      eqTests,
      utilsTests
    ]

matrix_0 :: Matrix
matrix_0 = M []
matrix_3x0 :: Matrix
matrix_3x0 = M [[],[],[]]
matrix1 :: Matrix
matrix1 = M [[1,2],[3,4]]
matrix2 :: Matrix
matrix2 = M [[1,-2],[5,-4]]
matrix_3x2 :: Matrix
matrix_3x2 = M [[1,2],[3,4],[5,6]]
matrix_3x3 :: Matrix
matrix_3x3 = M [[1,2,3],[3,4,5],[5,6,7]]
matrix_3x3_b :: Matrix
matrix_3x3_b = M [[1,23,7],[3,4,5],[0,6,0]]
matrix_ungueltig :: Matrix
matrix_ungueltig = M [[1,23,7],[3,5],[0]]

m1 = matrix_0 :: Matrix
m2 = matrix_3x0 :: Matrix
m3 = matrix1 :: Matrix
m4 = matrix2 :: Matrix
m5 = matrix_3x2 :: Matrix
m6 = matrix_3x3 :: Matrix
m7 = matrix_3x3_b :: Matrix
m8 = matrix_ungueltig :: Matrix

showTests :: TestTree
showTests =
  testGroup
		"show Tests"
    [ 
      testCase "show 1" $
        show (M [[1,2],[3,4]]) @?= "([1,2] [3,4])",
      testCase "show 2" $
        show (M [[1,2],[3,4],[5,6]]) @?= "([1,2] [3,4] [5,6])",
      testCase "show 3" $
        show (M [[1,2,3],[4,5],[6]]) @?= "([1,2,3] [4,5] [6])",
      testCase "show 4" $
        show (M [[1,2,3],[],[6]]) @?= "([1,2,3] [] [6])",
      testCase "show 5" $
        show (M [[],[],[]]) @?= "([] [] [])",
      testCase "show 6" $
        show (M []) @?= "()"
    ]

matrixtypTests :: TestTree
matrixtypTests = 
  testGroup
  "matrixtyp Tests"
  [ 
    testCase "matrixtyp 1" $
      matrixtyp (M [[1,2],[3,4]]) @?= Mat (2,2),
    testCase "matrixtyp 2" $
      matrixtyp (M [[1,2],[3,4],[5,6]]) @?= Mat (3,2),
    testCase "matrixtyp 3" $
      matrixtyp (M [[1,2,3],[4,5],[6]]) @?= KeineMatrix,
    testCase "matrixtyp 4" $
      matrixtyp (M [[1,2,3],[],[6]]) @?= KeineMatrix,
    testCase "matrixtyp 5" $
      matrixtyp (M [[],[],[]]) @?= KeineMatrix,
    testCase "matrixtyp 6" $
      matrixtyp (M []) @?= KeineMatrix
  ]


eqTests :: TestTree
eqTests = 
  testGroup
  "eq Tests"
  [ 
    testCase "eq 1" $
      matrix1 == matrix1 @?= True,
    testCase "eq 2" $
      matrix1 == matrix2 @?= False,
    testCase "eq 3" $
      (matrix1 == matrix_ungueltig) `expectError` "Gleichheit undefiniert",
    testCase "eq 4" $
      (matrix_ungueltig == matrix2) `expectError` "Gleichheit undefiniert",
    testCase "eq 5" $
      (matrix1 /= matrix_ungueltig) `expectError` "Ungleichheit undefiniert",
    testCase "eq 6" $
      (matrix_ungueltig /= matrix2) `expectError` "Ungleichheit undefiniert"
  ]

numTests :: TestTree
numTests = 
  testGroup
  "num Tests"
  [ 
    testCase "abs 1" $
      abs m1 @?= (M []),
    testCase "abs 2" $
      abs m2 @?= (M []),
    testCase "abs 3" $
      abs m3 @?= (M [[1,2],[3,4]]),
    testCase "abs 4" $
      abs m4 @?= (M [[1,2],[5,4]]),
    testCase "abs 5" $
      abs m5 @?= (M [[1,2],[3,4],[5,6]])
  ]

utilsTests :: TestTree
utilsTests = 
  testGroup
  "utils Tests"
  [ 
    testCase "compareRows 1" $
      compareRows [1,2] [1,2] @?= True,
    testCase "compareRows 2" $
      compareRows [1,3] [1,2] @?= False,
    testCase "compareRows 2" $
      compareRows [] [] @?= True,
    testCase "compareRows 2" $
      compareRows [] [1] @?= False,
    testCase "compareRows 2" $
      compareRows [1,2,3] [] @?= False,
    testCase "compareMatrixValues 2" $
      compareMatrixValues matrix1 matrix2 @?= False,
    testCase "compareMatrixValues 2" $
      compareMatrixValues matrix1 matrix1 @?= True,
    testCase "compareMatrixValues 2" $
      absRow [1,-2] @?= [1,2],
    testCase "compareMatrixValues 2" $
      absRow [1,2] @?= [1,2]
  ]


expectError :: Show a => a -> String -> Assertion
expectError val expectedMsg = do
  err <- E.try (E.evaluate val)
  case err of
    Left (E.ErrorCall actual) ->
      M.unless (expectedMsg P.== actual) $
        assertFailure $
          "expected: " ++ expectedMsg ++ "\n but got: " ++ actual
    Right r -> do
      assertFailure $ "Expected an exception but got: " ++ P.show r