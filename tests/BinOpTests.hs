module BinOpTests where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Language.Java.Parser (ParserMode (ParseFull), ParserState (ParserState), exp, parserWithState)
import Language.Java.Pretty (Pretty (pretty))
import Language.Java.SourceSpan
import Language.Java.Syntax
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (HasCallStack, assertFailure, testCase, (@?))
import Prelude hiding (div, exp)

ident :: String -> Exp Parsed
ident name = ExpName (Name dummySourceSpan (Ident dummySourceSpan name :| []))

binOp :: Op -> Exp Parsed -> Exp Parsed -> Exp Parsed
binOp = flip (BinOp dummySourceSpan)

testRoundTrip :: (HasCallStack) => Exp Parsed -> TestTree
testRoundTrip e = testCase (prettyExplicit e) $
  case parserWithState (ParserState ParseFull False) exp "" (show (pretty e)) of
    Left err -> assertFailure (show err)
    Right (e', _) -> eq IgnoreSourceSpan e e' @? "pretty-printed (" ++ show (pretty e) ++ ") reparsed as " ++ prettyExplicit e'

prettyExplicit :: Exp Parsed -> String
prettyExplicit (BinOp _ a op b) = "(" ++ prettyExplicit a ++ " " ++ show (pretty op) ++ " " ++ prettyExplicit b ++ ")"
prettyExplicit e = show (pretty e)

testBinOp :: [TestTree]
testBinOp =
  [ testRoundTrip ((a +: b) *: c),
    testRoundTrip (a +: (b *: c)),
    testRoundTrip ((a -: b) -: c),
    testRoundTrip (a -: (b -: c)),
    testRoundTrip ((((a -: b) *: c) +: d) /: e),
    testRoundTrip ((a -: (b *: c)) +: (d /: e)),
    testRoundTrip (a -: ((b *: c) +: (d /: e))),
    testRoundTrip (((a -: b) *: (c +: d)) /: e),
    testRoundTrip ((a -: b) *: ((c +: d) /: e)),
    testRoundTrip ((((a -: b) -: c) -: d) -: e),
    testRoundTrip (a -: (b -: (c -: (d -: e))))
  ]
  where
    (+:) = binOp Add
    (-:) = binOp Sub
    (*:) = binOp Mult
    (/:) = binOp Div
    a = ident "a"
    b = ident "b"
    c = ident "c"
    d = ident "d"
    e = ident "e"
