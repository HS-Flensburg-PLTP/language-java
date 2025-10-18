module BinOpTests where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Language.Java.Parser (ParserMode (ParseFull), ParserState (ParserState), exp, parserWithState)
import Language.Java.Pretty (Pretty (pretty))
import Language.Java.SourceSpan
import Language.Java.Syntax
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (HasCallStack, assertFailure, testCaseInfo, (@?))
import Prelude hiding (exp)

ident :: String -> Exp Parsed
ident name = ExpName (Name dummySourceSpan (Ident dummySourceSpan name :| []))

binOp :: Op -> Exp Parsed -> Exp Parsed -> Exp Parsed
binOp = flip (BinOp dummySourceSpan)

testRoundTrip :: (HasCallStack) => Exp Parsed -> TestTree
testRoundTrip e = testCaseInfo (prettyExplicit e) $
  case parserWithState (ParserState ParseFull False) exp "" (show (pretty e)) of
    Left err -> assertFailure (show err)
    Right (e', _) -> do
      eq IgnoreSourceSpan e e' @? "pretty-printed (" ++ show (pretty e) ++ ") reparsed as " ++ prettyExplicit e'
      pure ("pretty-printed: " ++ show (pretty e))

prettyExplicit :: Exp Parsed -> String
prettyExplicit (BinOp _ a op b) = "(" ++ prettyExplicit a ++ " " ++ show (pretty op) ++ " " ++ prettyExplicit b ++ ")"
prettyExplicit e = show (pretty e)

testBinOp :: [TestTree]
testBinOp =
  [ testGroup
      "3-operand add/sub"
      [ testRoundTrip ((a +: b) *: c),
        testRoundTrip (a +: (b *: c)),
        testRoundTrip ((a -: b) -: c),
        testRoundTrip (a -: (b -: c))
      ],
    testGroup
      "5-operand add/sub/mul/div"
      [ testRoundTrip ((((a -: b) *: c) +: d) /: e),
        testRoundTrip ((a -: (b *: c)) +: (d /: e)),
        testRoundTrip (a -: ((b *: c) +: (d /: e))),
        testRoundTrip (((a *: b) +: (c /: d)) -: e),
        testRoundTrip ((a *: b) +: ((c /: d) -: e)),
        testRoundTrip (((a -: b) *: (c +: d)) /: e),
        testRoundTrip ((a -: b) *: ((c +: d) /: e)),
        testRoundTrip ((((a -: b) -: c) -: d) -: e),
        testRoundTrip (a -: (b -: (c -: (d -: e))))
      ],
    testGroup
      "comparisons"
      [ testRoundTrip ((a +: b) <: c),
        testRoundTrip (a >: (b +: c)),
        testRoundTrip ((a +: b) <: (c +: d))
      ],
    testGroup
      "logical operators"
      [ testRoundTrip (((a ==: b) ||: (b ==: c)) &&: ((c ==: d) ||: (d ==: e))),
        testRoundTrip (((a ==: b) &&: (b ==: c)) ||: ((c ==: d) &&: (d ==: e)))
      ]
  ]
  where
    (+:) = binOp Add
    (-:) = binOp Sub
    (*:) = binOp Mult
    (/:) = binOp Div
    (<:) = binOp LThan
    (>:) = binOp GThan
    (==:) = binOp Equal
    (&&:) = binOp CAnd
    (||:) = binOp COr
    a = ident "a"
    b = ident "b"
    c = ident "c"
    d = ident "d"
    e = ident "e"
