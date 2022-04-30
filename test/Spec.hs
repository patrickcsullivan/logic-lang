import Control.Exception (evaluate)
import Ground (groundTermPermutations, groundTerms)
import qualified Ground
import Parser (parse')
import Syntax.Clausal (Literal (..), clausal)
import Syntax.Constant (FnConst (..), ObjConst (..), RltnConst (..))
import Syntax.Formula (Formula (..))
import Syntax.Term (Term (..))
import Syntax.Variable (Var (..))
import Test.Hspec (context, describe, hspec, it, pending, shouldBe)

main :: IO ()
main = hspec $ do
  formulaPSpec
  clausalSpec
  groundTermsSpec
  groundTermPermutationsSpec

formulaPSpec =
  describe "Parser.formulaP" $ do
    it "works" $ do
      pending

clausalSpec =
  describe "Clausal.clausal" $ do
    let fo1 = "exists Y. X < Y ==> forall U. exists V. X * U < Y * V"
    it ("transforms " ++ show fo1) $ do
      let expected =
            [ [ LNeg
                  (RltnConst {rltnConstName = "<", rltnConstArity = 2})
                  [ TVar (Var {varName = "X"}),
                    TFn (FnConst {fnConstName = "f_Y", fnConstArity = 1}) [TVar (Var {varName = "X"})]
                  ],
                LPos
                  (RltnConst {rltnConstName = "<", rltnConstArity = 2})
                  [ TFn (FnConst {fnConstName = "*", fnConstArity = 2}) [TVar (Var {varName = "X"}), TVar (Var {varName = "U"})],
                    TFn (FnConst {fnConstName = "*", fnConstArity = 2}) [TFn (FnConst {fnConstName = "f_Y", fnConstArity = 1}) [TVar (Var {varName = "X"})], TFn (FnConst {fnConstName = "f_V", fnConstArity = 2}) [TVar (Var {varName = "U"}), TVar (Var {varName = "X"})]]
                  ]
              ]
            ]
      clausal (parse' fo1) `shouldBe` expected
    let fo2 = "forall X. p(X) ==> (exists Y Z. q(Y) or ~(exists Z. p(Z) and q(Z)))"
    it ("transforms " ++ show fo2) $ do
      let expected =
            [ [ LNeg (RltnConst "p" 1) [TVar (Var "X")],
                LPos (RltnConst "q" 1) [TObj (ObjConst "c_Y")],
                LNeg (RltnConst "p" 1) [TVar (Var "Z")],
                LNeg (RltnConst "q" 1) [TVar (Var "Z")]
              ]
            ]
      clausal (parse' fo2) `shouldBe` expected
    let fo3 = "exists X Y. p(X, Y) and q(X, Y)"
    it ("transforms " ++ show fo3) $ do
      let expected =
            [ [ LPos (RltnConst "p" 2) [TObj (ObjConst "c_X"), TObj (ObjConst "c_Y")]
              ],
              [ LPos (RltnConst "q" 2) [TObj (ObjConst "c_X"), TObj (ObjConst "c_Y")]
              ]
            ]
      clausal (parse' fo3) `shouldBe` expected

groundTermsSpec =
  describe "Ground.groundTerms" $ do
    let c = ObjConst "c"
    let d = ObjConst "d"
    let f = FnConst "f" 1
    let g = FnConst "g" 2
    it "generates ground terms of depth 0" $ do
      groundTerms [c, d] [f, g] 0
        `shouldBe` [ TObj (ObjConst {objConstName = "c"}),
                     TObj (ObjConst {objConstName = "d"})
                   ]
    it "generates ground terms of depth 1" $ do
      groundTerms [c, d] [f, g] 1
        `shouldBe` [ TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TObj (ObjConst {objConstName = "c"})],
                     TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TObj (ObjConst {objConstName = "d"})],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "c"}), TObj (ObjConst {objConstName = "c"})],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "c"}), TObj (ObjConst {objConstName = "d"})],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "d"}), TObj (ObjConst {objConstName = "c"})],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "d"}), TObj (ObjConst {objConstName = "d"})]
                   ]
    it "generates ground terms of depth 2" $ do
      groundTerms [c, d] [f, g] 2
        `shouldBe` [ TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TObj (ObjConst {objConstName = "c"})]],
                     TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TObj (ObjConst {objConstName = "d"})]],
                     TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "c"}), TObj (ObjConst {objConstName = "c"})]],
                     TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "c"}), TObj (ObjConst {objConstName = "d"})]],
                     TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "d"}), TObj (ObjConst {objConstName = "c"})]],
                     TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "d"}), TObj (ObjConst {objConstName = "d"})]],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "c"}), TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TObj (ObjConst {objConstName = "c"})]],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "c"}), TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TObj (ObjConst {objConstName = "d"})]],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "c"}), TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "c"}), TObj (ObjConst {objConstName = "c"})]],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "c"}), TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "c"}), TObj (ObjConst {objConstName = "d"})]],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "c"}), TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "d"}), TObj (ObjConst {objConstName = "c"})]],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "c"}), TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "d"}), TObj (ObjConst {objConstName = "d"})]],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "d"}), TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TObj (ObjConst {objConstName = "c"})]],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "d"}), TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TObj (ObjConst {objConstName = "d"})]],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "d"}), TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "c"}), TObj (ObjConst {objConstName = "c"})]],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "d"}), TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "c"}), TObj (ObjConst {objConstName = "d"})]],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "d"}), TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "d"}), TObj (ObjConst {objConstName = "c"})]],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "d"}), TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "d"}), TObj (ObjConst {objConstName = "d"})]],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TObj (ObjConst {objConstName = "c"})], TObj (ObjConst {objConstName = "c"})],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TObj (ObjConst {objConstName = "c"})], TObj (ObjConst {objConstName = "d"})],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TObj (ObjConst {objConstName = "d"})], TObj (ObjConst {objConstName = "c"})],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TObj (ObjConst {objConstName = "d"})], TObj (ObjConst {objConstName = "d"})],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "c"}), TObj (ObjConst {objConstName = "c"})], TObj (ObjConst {objConstName = "c"})],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "c"}), TObj (ObjConst {objConstName = "c"})], TObj (ObjConst {objConstName = "d"})],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "c"}), TObj (ObjConst {objConstName = "d"})], TObj (ObjConst {objConstName = "c"})],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "c"}), TObj (ObjConst {objConstName = "d"})], TObj (ObjConst {objConstName = "d"})],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "d"}), TObj (ObjConst {objConstName = "c"})], TObj (ObjConst {objConstName = "c"})],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "d"}), TObj (ObjConst {objConstName = "c"})], TObj (ObjConst {objConstName = "d"})],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "d"}), TObj (ObjConst {objConstName = "d"})], TObj (ObjConst {objConstName = "c"})],
                     TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TFn (FnConst {fnConstName = "g", fnConstArity = 2}) [TObj (ObjConst {objConstName = "d"}), TObj (ObjConst {objConstName = "d"})], TObj (ObjConst {objConstName = "d"})]
                   ]

groundTermPermutationsSpec =
  describe "Ground.groundTermPermutations" $ do
    let c = ObjConst "c"
    let d = ObjConst "d"
    let f = FnConst "f" 1
    it "generates permutations of length 2 for ground terms of depth 0" $ do
      groundTermPermutations [c, d] [f] 2 0
        `shouldBe` [ [TObj (ObjConst {objConstName = "c"}), TObj (ObjConst {objConstName = "c"})],
                     [TObj (ObjConst {objConstName = "c"}), TObj (ObjConst {objConstName = "d"})],
                     [TObj (ObjConst {objConstName = "d"}), TObj (ObjConst {objConstName = "c"})],
                     [TObj (ObjConst {objConstName = "d"}), TObj (ObjConst {objConstName = "d"})]
                   ]
    it "generates permutations of length 1 for ground terms of depth 2" $ do
      groundTermPermutations [c, d] [f] 1 2
        `shouldBe` [ [TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TObj (ObjConst {objConstName = "c"})]]],
                     [TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TFn (FnConst {fnConstName = "f", fnConstArity = 1}) [TObj (ObjConst {objConstName = "d"})]]]
                   ]
