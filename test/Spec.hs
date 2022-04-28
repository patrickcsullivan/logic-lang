import Ast (FnConst (..), Formula (..), ObjConst (..), RltnConst (..), Term (..), Var (..))
import Clausal (Literal (..), clausal)
import Control.Exception (evaluate)
import Parser (parse')
import Test.Hspec (context, describe, hspec, it, pending, shouldBe)

main :: IO ()
main = hspec $ do
  formulaPSpec
  clausalSpec

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
      (clausal $ parse' fo1) `shouldBe` expected
    let fo2 = "forall X. p(X) ==> (exists Y Z. q(Y) or ~(exists Z. p(Z) and q(Z)))"
    it ("transforms " ++ show fo2) $ do
      let expected =
            [ [ LNeg (RltnConst "p" 1) [TVar (Var "X")],
                LPos (RltnConst "q" 1) [TObj (ObjConst "c_Y")],
                LNeg (RltnConst "p" 1) [TVar (Var "Z")],
                LNeg (RltnConst "q" 1) [TVar (Var "Z")]
              ]
            ]
      (clausal $ parse' fo2) `shouldBe` expected
    let fo3 = "exists X Y. p(X, Y) and q(X, Y)"
    it ("transforms " ++ show fo3) $ do
      let expected =
            [ [ LPos (RltnConst "p" 2) [TObj (ObjConst "c_X"), TObj (ObjConst "c_Y")]
              ],
              [ LPos (RltnConst "q" 2) [TObj (ObjConst "c_X"), TObj (ObjConst "c_Y")]
              ]
            ]
      (clausal $ parse' fo3) `shouldBe` expected
