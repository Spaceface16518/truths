import Lib (Expr (..), vars)
import Test.HUnit

main :: IO Counts
main = runTestTT $ TestList [varsTests]

-- test variable extraction
varsTests = TestList [TestLabel "vars Var" varsVar, TestLabel "vars Not" varsNot, TestLabel "vars And" varsAnd, TestLabel "vars Or" varsOr, TestLabel "vars Cond" varsCond, TestLabel "vars Bicond" varsBicond]

varsVar = TestCase (assertEqual "for p" "p" (vars $ Var 'p'))

varsNot = TestCase (assertEqual "for ¬p" "p" (vars $ Not (Var 'p')))

varsAnd = TestCase (assertEqual "for p ∧ q" "pq" (vars $ And (Var 'p') (Var 'q')))

varsOr = TestCase (assertEqual "for p ∨ q" "pq" (vars $ Or (Var 'p') (Var 'q')))

varsCond = TestCase (assertEqual "for p → q" "pq" (vars $ Cond (Var 'p') (Var 'q')))

varsBicond = TestCase (assertEqual "for p ↔ q" "pq" (vars $ Bicond (Var 'p') (Var 'q')))
