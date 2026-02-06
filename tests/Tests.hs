{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import BinOpTests (testBinOp)
import qualified Control.Exception as E
import Control.Monad
import Data.List (isSuffixOf)
import qualified Data.List.NonEmpty as NonEmpty
import Language.Java.Parser
import Language.Java.Pretty
import Language.Java.SourceSpan
import Language.Java.Syntax
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import TransformerTests (allTransformerTests)
import Prelude hiding (exp)

instance Arbitrary (CompilationUnit p) where
  arbitrary = CompilationUnit <$> arbitrary <*> arbitrary <*> ((: []) <$> arbitrary)

instance Arbitrary PackageDecl where
  arbitrary = PackageDecl <$> arbitrary

instance Arbitrary ImportDecl where
  arbitrary = ImportDecl dummySourceSpan <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (TypeDecl p) where
  arbitrary = ClassTypeDecl <$> arbitrary

instance Arbitrary (ClassDecl p) where
  arbitrary = (\x y -> ClassDecl dummySourceSpan [] x [] Nothing [] y) <$> arbitrary <*> arbitrary

instance Arbitrary (ClassBody p) where
  arbitrary = pure (ClassBody [])

instance Arbitrary Name where
  arbitrary = Name dummySourceSpan <$> (choose (1, 3) >>= \len -> NonEmpty.fromList <$> replicateM len arbitrary)

instance Arbitrary ClassifiedName where
  arbitrary = Unknown <$> arbitrary

class (Arbitrary (XNameClassification x)) => ArbitraryExtension x

instance ArbitraryExtension Parsed

instance ArbitraryExtension Analyzed

instance Arbitrary Ident where
  arbitrary = Ident dummySourceSpan . unkeyword <$> (choose (1, 15) >>= \len -> replicateM len (elements (['a' .. 'z'] ++ ['A' .. 'Z'])))
    where
      unkeyword k
        | k
            `elem` [ "abstract",
                     "assert",
                     "boolean",
                     "break",
                     "byte",
                     "case",
                     "catch",
                     "char",
                     "class",
                     "const",
                     "continue",
                     "default",
                     "do",
                     "double",
                     "else",
                     "enum",
                     "extends",
                     "final",
                     "finally",
                     "float",
                     "for",
                     "goto",
                     "if",
                     "implements",
                     "import",
                     "instanceof",
                     "int",
                     "interface",
                     "long",
                     "native",
                     "new",
                     "package",
                     "private",
                     "protected",
                     "public",
                     "record",
                     "return",
                     "short",
                     "static",
                     "strictfp",
                     "super",
                     "switch",
                     "synchronized",
                     "this",
                     "throw",
                     "throws",
                     "transient",
                     "try",
                     "void",
                     "volatile",
                     "while"
                   ] =
            "x" ++ k
        | otherwise = k

instance (ArbitraryExtension p) => Arbitrary (Exp p) where
  arbitrary = sized exp'
    where
      exp' n
        | n <= 0 = pure (Lit (Null dummySourceSpan))
        | otherwise =
            scale (`div` 2) $
              oneof
                [ ExpName <$> arbitrary,
                  PrePlus dummySourceSpan <$> arbitrary,
                  PreMinus dummySourceSpan <$> arbitrary,
                  PreBitCompl dummySourceSpan <$> arbitrary,
                  PreNot dummySourceSpan <$> arbitrary,
                  BinOp dummySourceSpan <$> arbitrary <*> arbitrary <*> arbitrary
                ]

instance (ArbitraryExtension p) => Arbitrary (ArrayIndex p) where
  arbitrary = ArrayIndex dummySourceSpan <$> arbitrary <*> arbitrary

instance Arbitrary Op where
  arbitrary =
    elements
      [ Mult,
        Div,
        Rem,
        Add,
        Sub,
        LShift,
        RShift,
        RRShift,
        LThan,
        GThan,
        LThanE,
        GThanE,
        Equal,
        NotEq,
        And,
        Or,
        Xor,
        CAnd,
        COr
      ]

----------------------------------------------------------
testJavaDirectory :: FilePath
testJavaDirectory = "tests" </> "java"

isJavaFile :: FilePath -> Bool
isJavaFile f = ".java" `isSuffixOf` f

toTestCase :: ParserMode -> Bool -> FilePath -> TestTree
toTestCase mode expected jFile = testCase (takeBaseName jFile) doTest
  where
    doTest = do
      r <- E.try parseOne
      case r of
        Left (e :: E.SomeException) -> assertBool ("failure exception: " ++ show e) (not expected)
        Right (Left perr) -> assertBool ("failure parse error: " ++ show perr) (not expected)
        Right (Right p) -> assertBool ("success: " ++ show p) expected
    parseOne = parserWithMode mode compilationUnit jFile <$> readFile jFile

getAllJavaPaths :: FilePath -> IO [FilePath]
getAllJavaPaths path = map (path </>) . filter isJavaFile <$> getDirectoryContents path

main :: IO ()
main = do
  exists <- doesDirectoryExist testJavaDirectory
  unless exists $ error "cannot find tests files java directory"

  allGoodJavas <- getAllJavaPaths (testJavaDirectory </> "good")
  allBadJavas <- getAllJavaPaths (testJavaDirectory </> "bad")
  let -- the bad tests that work with shallow parsing
      shallowGoodJavas = [testJavaDirectory </> "bad" </> "DiamondIncorrectPlacement.java"]
      shallowBadJavas = filter (`notElem` shallowGoodJavas) allBadJavas

  defaultMain $
    testGroup
      "java"
      [ testGroup "parsing unit good" (map (toTestCase ParseFull True) allGoodJavas),
        testGroup
          "parsing shallow unit good"
          (map (toTestCase ParseShallow True) (allGoodJavas ++ shallowGoodJavas)),
        testGroup "parsing unit bad" (map (toTestCase ParseFull False) allBadJavas),
        testGroup "parsing shallow unit bad" (map (toTestCase ParseShallow False) shallowBadJavas),
        testProperty
          "parsing.generating==id (compilationUnit)"
          ( \g -> case parserWithState (ParserState ParseFull False) compilationUnit "<input>" (show $ pretty g) of
              Right g' -> eq IgnoreSourceSpan g g'
              Left perr -> error (show (pretty g) ++ show perr)
          ),
        testProperty
          "parsing.generating==id (exp)"
          ( \g -> case parserWithState (ParserState ParseFull False) (fst <$> exp) "<input>" (show $ pretty g) of
              Right g' -> counterexample (show $ pretty g) $ counterexample (show g') $ eq IgnoreSourceSpan g g'
              Left perr -> error (show (pretty g) ++ show perr)
          ),
        testGroup "binary operators" testBinOp,
        testGroup "transformer tests" allTransformerTests
      ]
