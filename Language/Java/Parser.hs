{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Java.Parser
  ( JavaParser,
    parser,
    compilationUnit,
    packageDeclaration,
    importDeclaration,
    typeDeclaration,
    classDecl,
    interfaceDecl,
    memberDecl,
    fieldDecl,
    methodDecl,
    constrDecl,
    interfaceMemberDecl,
    absMethodDecl,
    formalParams,
    formalParam,
    modifier,
    varDecls,
    varDecl,
    block,
    blockStmt,
    stmt,
    stmtExp,
    exp,
    primary,
    literal,
    ttype,
    primType,
    refType,
    classType,
    resultType,
    lambdaExp,
    methodRef,
    typeParameters,
    typeParameter,
    name,
    ident,
    manyNonEmptyPos,
    sepByNonEmptyPos,
    sepEndByPos,
    trySepByNonEmptyPos,
    optionMaybePos,
    comma,
    semiColon,
    period,
    colon,
  )
where

import Control.Monad (guard, when)
import qualified Data.Bifunctor as Bifunctor
import Data.Foldable (Foldable (foldl'))
import Data.Function ((&))
import Data.Functor (void, ($>), (<&>))
import Data.List (unsnoc)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Language.Java.Lexer (L1 (..), Token (..), lexer)
import Language.Java.SourceSpan (posEof)
import Language.Java.Syntax
import Text.Parsec
  ( Parsec,
    State (stateInput, statePos),
    chainl1,
    eof,
    getParserState,
    many,
    many1,
    option,
    optionMaybe,
    parse,
    sepBy,
    sepEndBy,
    skipMany1,
    token,
    try,
    (<?>),
    (<|>),
  )
import Text.Parsec.Extra (manyMaybes, optionBool, optionConst)
import qualified Text.Parsec.NonEmpty as NonEmpty
import Text.Parsec.Pos
import Text.ParserCombinators.Parsec.Error
import Prelude hiding (exp, (>>), (>>=))
import qualified Prelude as P ((>>), (>>=))

type JavaParser = Parsec [L1 Token] ()

type P = JavaParser

noPos :: P (a, SourcePos) -> P a
noPos = fmap fst

getPosition :: P SourcePos
getPosition =
  getParserState <&> \case
    (stateInput -> []) -> posEof
    state -> statePos state

getEndPos :: P SourcePos
getEndPos =
  getParserState <&> \case
    state@(stateInput -> L1 (line, column) len _ : _) ->
      let file = sourceName (statePos state)
       in newPos file line (column + len)
    _ -> posEof

-- A trick to allow >> and >>=, normally infixr 1, to be
-- used inside branches of <|>, which is declared as infixl 1.
-- There are no clashes with other operators of precedence 2.
(>>) :: P a -> P b -> P b
(>>) = (P.>>)

(>>=) :: P a -> (a -> P b) -> P b
(>>=) = (P.>>=)

infixr 2 >>, >>=

-- Note also when reading that <$> is infixl 4 and thus has
-- lower precedence than all the others (>>, >>=, and <|>).

----------------------------------------------------------------------------
-- Top-level parsing

parser :: P a -> FilePath -> String -> Either ParseError a
parser p srcName src =
  case lexer src of
    Left (line, col, err) ->
      let msg = Message err
          pos = newPos srcName line col
       in Left (newErrorMessage msg pos)
    Right tokens ->
      parse p srcName tokens

----------------------------------------------------------------------------
-- Packages and compilation units

compilationUnit :: P (CompilationUnit Parsed)
compilationUnit =
  CompilationUnit
    <$> optionMaybe packageDeclaration
    <*> many importDeclaration
    <*> manyMaybes typeDeclaration
    <* eof

packageDeclaration :: P (PackageDecl Parsed)
packageDeclaration =
  PackageDecl
    <$> many (Annotation <$> annotation)
    <* tok KW_Package
    <*> noPos name
    <* semiColon

importDeclaration :: P ImportDecl
importDeclaration = do
  startPos <- getPosition
  tok KW_Import
  st <- optionBool $ tok KW_Static
  n <- noPos name
  star <- optionBool (period >> tok Op_Star)
  endPos <- semiColon
  return (ImportDecl (startPos, endPos) st n star)

typeDeclaration :: P (Maybe (TypeDecl Parsed))
typeDeclaration =
  Just <$> classOrInterfaceDecl
    <|> Nothing <$ semiColon

----------------------------------------------------------------------------
-- Declarations

-- Class declarations

classOrInterfaceDecl :: P (TypeDecl Parsed)
classOrInterfaceDecl = do
  startPos <- getPosition
  ms <- many modifier
  do
    cd <- noPos classDecl
    return (ClassTypeDecl (cd startPos ms))
    <|> do
      iDecl <- annInterfaceDecl <|> interfaceDecl
      return (InterfaceTypeDecl (iDecl startPos ms))

classDecl :: P (Mod (ClassDecl Parsed), SourcePos)
classDecl =
  normalClassDeclaration
    <|> recordDeclaration
    <|> enumDeclaration

normalClassDeclaration :: P (Mod (ClassDecl Parsed), SourcePos)
normalClassDeclaration = do
  tok KW_Class
  i <- noPos ident
  tps <- typeParameters
  mex <- extends1
  imp <- implements
  (bod, endPos) <- classBody
  return (\pos ms -> ClassDecl (pos, endPos) ms i tps mex imp bod, endPos)

extends1 :: P (Maybe RefType)
extends1 = optionMaybe (tok KW_Extends >> noPos refType)

extends :: P [RefType]
extends = NonEmpty.neoptList (tok KW_Extends >> refTypeList)

permits :: P [RefType]
permits = NonEmpty.neoptList (fixedIdent "permits" >> refTypeList)

implements :: P [RefType]
implements = NonEmpty.neoptList (tok KW_Implements >> refTypeList)

enumDeclaration :: P (Mod (ClassDecl Parsed), SourcePos)
enumDeclaration = do
  tok KW_Enum
  i <- noPos ident
  imp <- implements
  (bod, endPos) <- enumBody
  return (\pos ms -> EnumDecl (pos, endPos) ms i imp bod, endPos)

-- Since Java 16
recordDeclaration :: P (Mod (ClassDecl Parsed), SourcePos)
recordDeclaration = do
  tok KW_Record
  i <- noPos ident
  tps <- typeParameters
  fields <- noPos $ parens $ sepBy recordField comma
  imp <- implements
  (bod, endPos) <- classBody
  return (\pos ms -> RecordDecl (pos, endPos) ms i tps fields imp bod, endPos)
  where
    recordField = RecordFieldDecl <$> ttype <*> noPos ident

classBody :: P (ClassBody Parsed, SourcePos)
classBody = Bifunctor.first ClassBody <$> braces classBodyStatements

enumBody :: P (EnumBody Parsed, SourcePos)
enumBody =
  braces $
    EnumBody
      <$> sepEndBy enumConst comma
      <*> option [] enumBodyDecls

enumConst :: P (EnumConstant Parsed)
enumConst =
  EnumConstant
    <$> noPos ident
    <*> option [] (noPos args)
    <*> optionMaybe (noPos classBody)

enumBodyDecls :: P [Decl Parsed]
enumBodyDecls = semiColon >> classBodyStatements

classBodyStatements :: P [Decl Parsed]
classBodyStatements = manyMaybes classBodyStatement

-- Interface declarations

annInterfaceDecl :: P (Mod (InterfaceDecl Parsed))
annInterfaceDecl = do
  tok KW_AnnInterface
  i <- noPos ident
  tps <- typeParameters
  exs <- extends
  ps <- permits
  (bod, endPos) <- interfaceBody
  return \pos ms -> InterfaceDecl (pos, endPos) InterfaceAnnotation ms i tps exs ps bod

interfaceDecl :: P (Mod (InterfaceDecl Parsed))
interfaceDecl = do
  tok KW_Interface
  i <- noPos ident
  tps <- typeParameters
  exs <- extends
  ps <- permits
  (bod, endPos) <- interfaceBody
  return \pos ms -> InterfaceDecl (pos, endPos) InterfaceNormal ms i tps exs ps bod

interfaceBody :: P (InterfaceBody Parsed, SourcePos)
interfaceBody = braces (InterfaceBody <$> manyMaybes interfaceBodyDecl)

-- Declarations

classBodyStatement :: P (Maybe (Decl Parsed))
classBodyStatement =
  skipMany1 semiColon $> Nothing
    <|> try do
      startPos <- getPosition
      mst <- optionBool (tok KW_Static)
      (bl, endPos) <- block
      return (Just (InitDecl (startPos, endPos) mst bl))
    <|> do
      pos <- getPosition
      ms <- many modifier
      dec <- memberDecl
      return (Just (MemberDecl (dec pos ms)))

memberDecl :: P (Mod (MemberDecl Parsed))
memberDecl =
  try do
    cd <- noPos classDecl
    return \pos ms -> MemberClassDecl (cd pos ms)
    <|> try do
      iDecl <- try annInterfaceDecl <|> try interfaceDecl
      return \pos ms -> MemberInterfaceDecl (iDecl pos ms)
    <|> try fieldDecl
    <|> try methodDecl
    <|> constrDecl

fieldDecl :: P (Mod (MemberDecl Parsed))
fieldDecl = do
  typ <- ttype
  vds <- varDecls
  endPos <- semiColon
  return \pos ms -> FieldDecl (pos, endPos) ms typ vds

methodDecl :: P (Mod (MemberDecl Parsed))
methodDecl = do
  tps <- typeParameters
  rt <- resultType
  i <- noPos ident
  fps <- formalParams
  thr <- throws
  (bod, endPos) <- methodBody
  return \pos ms -> MethodDecl (pos, endPos) ms tps rt i fps thr Nothing bod

methodBody :: P (MethodBody Parsed, SourcePos)
methodBody = onlySemi <|> fullBody
  where
    onlySemi = attrTokWithEndPos SemiColon (MethodBody Nothing)
    fullBody = Bifunctor.first (MethodBody . Just) <$> block

constrDecl :: P (Mod (MemberDecl Parsed))
constrDecl = do
  tps <- typeParameters
  i <- noPos ident
  fps <- option [] formalParams -- record constructors omit the argument list
  thr <- throws
  (bod, endPos) <- constrBody
  return \pos ms -> ConstructorDecl (pos, endPos) ms tps i fps thr bod

constrBody :: P (ConstructorBody Parsed, SourcePos)
constrBody =
  braces $
    ConstructorBody
      <$> optionMaybe (try explConstrInv)
      <*> many (noPos blockStmt)

explConstrInv :: P (ExplConstrInv Parsed)
explConstrInv =
  noPos . endSemi $
    try
      ( ThisInvoke
          <$> NonEmpty.neoptList refTypeArgs
          <* tok KW_This
          <*> noPos args
      )
      <|> try
        ( SuperInvoke
            <$> NonEmpty.neoptList refTypeArgs
            <* tok KW_Super
            <*> noPos args
        )
      <|> PrimarySuperInvoke
        <$> noPos primary
        <* period
        <*> NonEmpty.neoptList refTypeArgs
        <* tok KW_Super
        <*> noPos args

-- TODO: This should be parsed like class bodies, and post-checked.
--       That would give far better error messages.
interfaceBodyDecl :: P (Maybe (MemberDecl Parsed))
interfaceBodyDecl =
  semiColon $> Nothing
    <|> do
      pos <- getPosition
      ms <- many modifier
      imd <- interfaceMemberDecl
      return $ Just (imd pos ms)

interfaceMemberDecl :: P (Mod (MemberDecl Parsed))
interfaceMemberDecl =
  do
    cd <- noPos classDecl
    return \pos ms -> MemberClassDecl (cd pos ms)
    <|> do
      iDecl <- try annInterfaceDecl <|> try interfaceDecl
      return \pos ms -> MemberInterfaceDecl (iDecl pos ms)
    <|> try fieldDecl
    <|> absMethodDecl

absMethodDecl :: P (Mod (MemberDecl Parsed))
absMethodDecl = do
  tps <- typeParameters
  rt <- resultType
  i <- noPos ident
  fps <- formalParams
  thr <- throws
  def <- optionMaybe defaultValue
  endPos <- semiColon
  return \pos ms -> MethodDecl (pos, endPos) ms tps rt i fps thr def (MethodBody Nothing)

defaultValue :: P (Exp Parsed)
defaultValue = tok KW_Default >> noPos exp

throws :: P [RefType]
throws = NonEmpty.neoptList (tok KW_Throws >> refTypeList)

-- Formal parameters

formalParams :: P [FormalParam Parsed]
formalParams = noPos $ parens do
  fps <- sepBy formalParam comma
  fps <$ guard (null fps || all nonVarParam (init fps))
    <|> fail "Only the last formal parameter may be of variable arity"
  where
    nonVarParam :: FormalParam Parsed -> Bool
    nonVarParam (FormalParam _ _ _ b _) = not b

formalParam :: P (FormalParam Parsed)
formalParam = do
  startPos <- getPosition
  ms <- many modifier
  typ <- ttype
  var <- optionBool ellipsis
  (vdi, pos) <- varDeclId
  return (FormalParam (startPos, pos) ms typ var vdi)

catchFormalParam :: P (CatchFormalParam Parsed)
catchFormalParam = do
  startPos <- getPosition
  typs <- NonEmpty.sepBy ((,) <$> many modifier <*> ttype) (tok Op_Or)
  (vdi, pos) <- varDeclId
  return (CatchFormalParam (startPos, pos) typs vdi)

ellipsis :: P SourcePos
ellipsis = period >> period >> period

-- Modifiers

-- TODO: Enforce modifier rules.
modifier :: P (Modifier Parsed)
modifier =
  Annotation <$> annotation
    <|> do
      startPos <- getPosition
      (constr, endPos) <-
        attrTokWithEndPos KW_Public Public
          <|> attrTokWithEndPos KW_Private Private
          <|> attrTokWithEndPos KW_Protected Protected
          <|> attrTokWithEndPos KW_Abstract Abstract
          <|> attrTokWithEndPos KW_Final Final
          <|> attrTokWithEndPos KW_Static Static
          <|> attrTokWithEndPos KW_Strictfp StrictFP
          <|> attrTokWithEndPos KW_Transient Transient
          <|> attrTokWithEndPos KW_Volatile Volatile
          <|> attrTokWithEndPos KW_Native Native
          <|> attrTokWithEndPos KW_Synchronized Synchronized_
          <|> attrFixedIdent "sealed" Sealed
      return (constr (startPos, endPos))

annotation :: P (Annotation Parsed)
annotation = do
  startPos <- getPosition
  tok Op_AtSign
  (n, nPos) <- name
  try do
    (elist, endPos) <- parens evlist
    return (NormalAnnotation (startPos, endPos) n elist)
    <|> try do
      (e, endPos) <- parens elementValue
      return (SingleElementAnnotation (startPos, endPos) n e)
    <|> return (MarkerAnnotation (startPos, nPos) n)

evlist :: P (NonEmpty (Ident, ElementValue Parsed))
evlist = NonEmpty.sepBy elementValuePair comma

elementValuePair :: P (Ident, ElementValue Parsed)
elementValuePair = (,) <$> noPos ident <* tok Op_Equal <*> elementValue

elementValue :: P (ElementValue Parsed)
elementValue =
  EVVal
    <$> ( InitArray <$> noPos arrayInit
            <|> InitExp <$> noPos condExp
        )
    <|> EVAnn
      <$> annotation

----------------------------------------------------------------------------
-- Variable declarations

varDecls :: P (NonEmpty (VarDecl Parsed))
varDecls = NonEmpty.sepBy varDecl comma

varDecl :: P (VarDecl Parsed)
varDecl = do
  startPos <- getPosition
  (vid, vidPos) <- varDeclId
  (mvi, endPos) <- optionMaybePos vidPos (tok Op_Equal >> varInit)
  return (VarDecl (startPos, endPos) vid mvi)

varDeclId :: P (VarDeclId, SourcePos)
varDeclId = do
  startPos <- getPosition
  (i, noArrPos) <- ident
  ebs <- many emptyBrackets
  let endPos = case ebs of
        [] -> noArrPos
        _ -> last ebs
  return (foldl' (\f pos -> VarDeclArray (startPos, pos) . f) VarId ebs i, endPos)

localVarDecl :: P (SourcePos, [Modifier Parsed], Type, NonEmpty (VarDecl Parsed))
localVarDecl =
  (,,,)
    <$> getPosition
    <*> many modifier
    <*> ttype
    <*> varDecls

varInit :: P (VarInit Parsed, SourcePos)
varInit =
  Bifunctor.first InitArray <$> arrayInit
    <|> Bifunctor.first InitExp <$> exp

arrayInit :: P (ArrayInit Parsed, SourcePos)
arrayInit = braces do
  startPos <- getPosition
  (vis, visPos) <- sepEndByPos varInit comma
  return (ArrayInit (startPos, visPos) vis)

----------------------------------------------------------------------------
-- Statements

block :: P (Block Parsed, SourcePos)
block = do
  startPos <- getPosition
  (bs, pos) <- braces (many (noPos blockStmt))
  return (Block (startPos, pos) bs, pos)

blockStmt :: P (BlockStmt Parsed, SourcePos)
blockStmt =
  try do
    pos <- getPosition
    ms <- many modifier
    (cd, endPos) <- classDecl
    return (LocalClass (cd pos ms), endPos)
    <|> try do
      ((startPos, m, t, vds), endPos) <- endSemi localVarDecl
      return (LocalVars (startPos, endPos) m t vds, endPos)
    <|> Bifunctor.first BlockStmt <$> stmt

stmt :: P (Stmt Parsed, SourcePos)
stmt =
  ifStmt
    <|> whileStmt
    <|> forStmt
    <|> labeledStmt
    <|> stmtNoTrail
  where
    ifStmt = do
      startPos <- getPosition
      tok KW_If
      e <- noPos $ parens (noPos exp)
      try do
        th <- noPos stmtNSI
        tok KW_Else
        (el, endPos) <- stmt
        return (IfThenElse (startPos, endPos) e th el, endPos)
        <|> do
          (th, endPos) <- stmt
          return (IfThen (startPos, endPos) e th, endPos)
    whileStmt = do
      startPos <- getPosition
      tok KW_While
      e <- noPos $ parens (noPos exp)
      (s, endPos) <- stmt
      return (While (startPos, endPos) e s, endPos)
    forStmt = do
      startPos <- getPosition
      tok KW_For
      f <-
        noPos $
          parens $
            try do
              fi <- optionMaybe forInit
              void semiColon
              e <- optionMaybe (noPos exp)
              void semiColon
              fu <- forUp
              return \pos -> BasicFor (startPos, pos) fi e fu
              <|> do
                ms <- many modifier
                t <- ttype
                i <- noPos ident
                void colon
                e <- noPos exp
                return \pos -> EnhancedFor (startPos, pos) ms t i e
      (s, endPos) <- stmt
      return (f endPos s, endPos)
    labeledStmt = try do
      startPos <- getPosition
      lbl <- noPos ident
      void colon
      (s, endPos) <- stmt
      return (Labeled (startPos, endPos) lbl s, endPos)

stmtNSI :: P (Stmt Parsed, SourcePos)
stmtNSI =
  ifStmt
    <|> whileStmt
    <|> forStmt
    <|> labeledStmt
    <|> stmtNoTrail
  where
    ifStmt = do
      startPos <- getPosition
      tok KW_If
      e <- noPos $ parens (noPos exp)
      th <- noPos stmtNSI
      tok KW_Else
      (el, endPos) <- stmtNSI
      return (IfThenElse (startPos, endPos) e th el, endPos)
    whileStmt = do
      startPos <- getPosition
      tok KW_While
      e <- noPos $ parens (noPos exp)
      (s, endPos) <- stmtNSI
      return (While (startPos, endPos) e s, endPos)
    forStmt = do
      startPos <- getPosition
      tok KW_For
      f <-
        noPos $
          parens $
            try do
              fi <- optionMaybe forInit
              void semiColon
              e <- optionMaybe (noPos exp)
              void semiColon
              fu <- forUp
              return \pos -> BasicFor (startPos, pos) fi e fu
              <|> do
                ms <- many modifier
                t <- ttype
                i <- noPos ident
                void colon
                e <- noPos exp
                return \pos -> EnhancedFor (startPos, pos) ms t i e
      (s, endPos) <- stmtNSI
      return (f endPos s, endPos)
    labeledStmt = try do
      startPos <- getPosition
      i <- noPos ident
      void colon
      (s, endPos) <- stmtNSI
      return (Labeled (startPos, endPos) i s, endPos)

stmtNoTrail :: P (Stmt Parsed, SourcePos)
stmtNoTrail =
  getPosition >>= \startPos ->
    -- empty statement
    do
      pos <- semiColon
      return (Empty (startPos, pos), pos)
      <|>
      -- inner block
      Bifunctor.first StmtBlock <$> block
      <|>
      -- assertions
      do
        tok KW_Assert
        e <- noPos exp
        me2 <- optionMaybe $ colon >> noPos exp
        pos <- semiColon
        return (Assert (startPos, pos) e me2, pos)
      <|>
      -- switch stmts
      do
        tok KW_Switch
        e <- noPos $ parens (noPos exp)
        ((style, sb), endPos) <- switchBlock
        return (Switch (startPos, endPos) style e sb, endPos)
      <|>
      -- do-while loops
      do
        tok KW_Do
        s <- noPos stmt
        tok KW_While
        e <- noPos $ parens (noPos exp)
        endPos <- semiColon
        return (Do (startPos, endPos) s e, endPos)
      <|>
      -- break
      do
        tok KW_Break
        mi <- optionMaybe (noPos ident)
        endPos <- semiColon
        return (Break (startPos, endPos) mi, endPos)
      <|>
      -- continue
      do
        tok KW_Continue
        mi <- optionMaybe (noPos ident)
        pos <- semiColon
        return (Continue (startPos, pos) mi, pos)
      <|>
      -- return
      do
        tok KW_Return
        me <- optionMaybe (noPos exp)
        endPos <- semiColon
        return (Return (startPos, endPos) me, endPos)
      <|>
      -- synchronized
      do
        tok KW_Synchronized
        e <- noPos $ parens (noPos exp)
        (b, endPos) <- block
        return (Synchronized (startPos, endPos) e b, endPos)
      <|>
      -- throw
      do
        tok KW_Throw
        e <- noPos exp
        pos <- semiColon
        return (Throw (startPos, pos) e, pos)
      <|>
      -- try-catch, both with and without a finally clause
      do
        tok KW_Try
        resources <- tryResourceList
        (b, blockEndPos) <- block
        cs <- many catch
        let cEndPos = maybe blockEndPos (snd . snd) (unsnoc cs)
        (mf, endPos) <- optionMaybePos cEndPos (tok KW_Finally >> block)
        when (null resources && null cs && null mf) $
          fail "try needs at least one catch or finally clause"
        return
          ( Try (startPos, endPos) resources b (map fst cs) mf,
            endPos
          )
      <|>
      -- expressions as stmts
      do
        (s, endPos) <- endSemi stmtExp
        return (ExpStmt (startPos, endPos) s, endPos)

-- For loops

forInit :: P (ForInit Parsed)
forInit =
  try do
    (_, m, t, vds) <- localVarDecl
    return (ForLocalVars m t vds)
    <|> ForInitExps <$> NonEmpty.sepBy stmtExp comma

forUp :: P [Exp Parsed]
forUp = sepBy stmtExp comma

-- Switches

switchBlock :: P ((SwitchStyle, [SwitchBlock Parsed]), SourcePos)
switchBlock = braces (try old <|> new)
  where
    old = (SwitchOldStyle,) <$> many switchStmtOld
    new = (SwitchNewStyle,) <$> many switchStmtNew

switchStmtOld :: P (SwitchBlock Parsed)
switchStmtOld = do
  startPos <- getPosition
  (lbl, lblPos) <- switchLabelOld
  bswithLocs <- many blockStmt
  let bss = map fst bswithLocs
      pos = maybe lblPos (snd . snd) (unsnoc bswithLocs)
  return (SwitchBlock (startPos, pos) lbl bss)

switchLabelOld :: P (SwitchLabel Parsed, SourcePos)
switchLabelOld =
  tok KW_Default >> attrTokWithEndPos Op_Colon Default
    <|> SwitchCase
      <$ tok KW_Case
      <*> NonEmpty.sepBy (noPos condExp) comma
        >>= attrTokWithEndPos Op_Colon

switchStmtNew :: P (SwitchBlock Parsed)
switchStmtNew = do
  startPos <- getPosition
  lbl <- switchLabelNew
  (bss, pos) <- braces (many (noPos blockStmt)) <|> (blockStmt <&> \s -> ([fst s], snd s))
  return (SwitchBlock (startPos, pos) lbl bss)

switchLabelNew :: P (SwitchLabel Parsed)
switchLabelNew =
  SwitchCase
    <$ tok KW_Case
    <*> NonEmpty.sepBy (noPos condExp) comma
    <* tok LambdaArrow
    <|> Default <$ tok KW_Default <* tok LambdaArrow

switchExp :: P (Exp Parsed, SourcePos)
switchExp = do
  startPos <- getPosition
  tok KW_Switch
  e <- noPos $ parens (noPos exp)
  (branches, pos) <- braces switchExpBody
  return (SwitchExp (startPos, pos) e branches, pos)
  where
    switchExpBody = NonEmpty.fromList <$> many1 switchExpBodyBranch
    switchExpBodyBranch = do
      lbl <- switchLabelNew
      body <-
        SwitchExpBranchBlock <$> noPos (braces (many (noPos blockStmt)))
          <|> SwitchExpBranchBlock <$> try (blockStmt <&> pure . fst)
          <|> SwitchExpBranchExp <$> branchExp
      return (SwitchExpBranch lbl body)
    branchExp = noPos exp <* semiColon

-- Try-catch clauses

catch :: P (Catch Parsed, SourcePos)
catch = do
  tok KW_Catch
  fp <- Catch <$> noPos (parens catchFormalParam)
  Bifunctor.first fp <$> block

tryResourceList :: P [TryResource Parsed]
tryResourceList = do
  option [] $ noPos $ parens $ sepEndBy tryResource semiColon
  where
    tryResource =
      TryResourceVarDecl <$> try resourceDecl
        <|> TryResourceQualAccess <$> try fieldAccess
        <|> TryResourceVarAccess <$> noPos ident

resourceDecl :: P (ResourceDecl Parsed)
resourceDecl =
  ResourceDecl
    <$> many modifier
    <*> ttype
    <*> noPos varDeclId
    <* tok Op_Equal
    <*> noPos varInit

----------------------------------------------------------------------------
-- Expressions

stmtExp :: P (Exp Parsed)
stmtExp =
  try (noPos preIncDec)
    <|> try postIncDec
    <|> try (noPos assignment)
    -- There are sharing gains to be made by unifying these two
    <|> try methodInvocationExp
    <|> try (noPos lambdaExp)
    <|> try (noPos methodRef)
    <|> instanceCreation

preIncDec :: P (Exp Parsed, SourcePos)
preIncDec = do
  op <- noPos preIncDecOp
  (e, ePos) <- unaryExp
  return (op ePos e, ePos)

postIncDec :: P (Exp Parsed)
postIncDec = do
  startPos <- getPosition
  e <- noPos postfixExpNES
  ops <- NonEmpty.many (noPos postfixOp)
  return (foldl' (\a s -> s startPos a) e ops)

assignment :: P (Exp Parsed, SourcePos)
assignment = do
  startPos <- getPosition
  lh <- lhs
  op <- assignOp
  (e, pos) <- assignExp
  return (Assign (startPos, pos) lh op e, pos)

lhs :: P (Lhs Parsed)
lhs =
  FieldLhs <$> try fieldAccess
    <|> ArrayLhs <$> try (noPos arrayAccess)
    <|> NameLhs <$> noPos name

exp :: P (Exp Parsed, SourcePos)
exp = assignExp

assignExp :: P (Exp Parsed, SourcePos)
assignExp =
  try switchExp
    <|> try methodRef
    <|> try lambdaExp
    <|> try assignment
    <|> condExp

condExp :: P (Exp Parsed, SourcePos)
condExp = do
  startPos <- getPosition
  condOrExp |>> condExpSuffix startPos

condExpSuffix :: SourcePos -> P (Exp Parsed -> Exp Parsed, SourcePos)
condExpSuffix startPos = do
  tok Op_Query
  th <- noPos exp
  void colon
  (el, endPos) <- condExp
  return (\ce -> Cond (startPos, endPos) ce th el, endPos)

infixlExpList :: P (Exp Parsed, SourcePos) -> P Op -> P (Exp Parsed, SourcePos)
infixlExpList p sep = do
  startPos <- getPosition
  p `chainl1` do
    op <- sep
    return \(e1, _) (e2, l2) -> (BinOp (startPos, l2) e1 op e2, l2)

condOrExp :: P (Exp Parsed, SourcePos)
condOrExp = infixlExpList condAndExp $ attrTok Op_OOr COr

condAndExp :: P (Exp Parsed, SourcePos)
condAndExp = infixlExpList orExp $ attrTok Op_AAnd CAnd

orExp :: P (Exp Parsed, SourcePos)
orExp = infixlExpList xorExp $ attrTok Op_Or Or

xorExp :: P (Exp Parsed, SourcePos)
xorExp = infixlExpList andExp $ attrTok Op_Caret Xor

andExp :: P (Exp Parsed, SourcePos)
andExp = infixlExpList eqExp $ attrTok Op_And And

eqExp :: P (Exp Parsed, SourcePos)
eqExp =
  infixlExpList relExp $
    attrTok Op_Equals Equal
      <|> attrTok Op_BangE NotEq

relExp :: P (Exp Parsed, SourcePos)
relExp = do
  startPos <- getPosition
  (e, endPos) <-
    infixlExpList shiftExp $
      attrTok Op_LThan LThan
        <|> attrTok Op_GThan GThan
        <|> attrTok Op_LThanE LThanE
        <|> attrTok Op_GThanE GThanE
  option (e, endPos) do
    tok KW_Instanceof
    (t, refPos) <- refType
    (mName, pos) <- optionMaybePos refPos name
    return (InstanceOf (startPos, pos) e t mName, pos)

shiftExp :: P (Exp Parsed, SourcePos)
shiftExp =
  infixlExpList addExp $
    attrTok Op_LShift LShift
      <|> try (tok Op_GThan >> tok Op_GThan >> tok Op_GThan $> RRShift)
      <|> try (tok Op_GThan >> tok Op_GThan $> RShift)

addExp :: P (Exp Parsed, SourcePos)
addExp =
  infixlExpList mulExp $
    attrTok Op_Plus Add
      <|> attrTok Op_Minus Sub

mulExp :: P (Exp Parsed, SourcePos)
mulExp =
  infixlExpList unaryExp $
    attrTok Op_Star Mult
      <|> attrTok Op_Slash Div
      <|> attrTok Op_Percent Rem

unaryExp :: P (Exp Parsed, SourcePos)
unaryExp =
  try preIncDec
    <|> try do
      op <- noPos prefixOp
      (e, ePos) <- unaryExp
      return (op ePos e, ePos)
    <|> try do
      startPos <- getPosition
      t <- noPos (parens ttype)
      (e, pos) <- unaryExp
      return (Cast (startPos, pos) t e, pos)
    <|> postfixExp

postfixExpNES :: P (Exp Parsed, SourcePos)
postfixExpNES =
  try primary
    <|> Bifunctor.first ExpName <$> name

postfixExp :: P (Exp Parsed, SourcePos)
postfixExp = do
  startPos <- getPosition
  postfixExpNES |>> (Bifunctor.first (\op -> op startPos) <$> postfixOp)

primary :: P (Exp Parsed, SourcePos)
primary = primaryNPS |>> primarySuffix

primaryNPS :: P (Exp Parsed, SourcePos)
primaryNPS =
  try arrayCreation
    <|> primaryNoNewArrayNPS

primaryNoNewArrayNPS :: P (Exp Parsed, SourcePos)
primaryNoNewArrayNPS = do
  startPos <- getPosition
  Bifunctor.first Lit <$> literal
    <|> do
      pos <- tokWithEndPos KW_This
      return (This (startPos, pos), pos)
    <|> parens (noPos exp)
    <|> try do
      rt <- resultType
      void period
      pos <- tokWithEndPos KW_Class
      return (ClassLit (startPos, pos) rt, pos)
    <|> try do
      n <- noPos name
      void period
      pos <- tokWithEndPos KW_This
      return (ThisClass (startPos, pos) n, pos)
    <|> try instanceCreationNPS
    <|> Bifunctor.first MethodInv <$> try methodInvocationNPS
    <|> Bifunctor.first FieldAccess <$> try fieldAccessNPS
    <|> Bifunctor.first ArrayAccess <$> arrayAccessNPS

primarySuffix :: P (Exp Parsed -> Exp Parsed, SourcePos)
primarySuffix =
  try instanceCreationSuffix
    <|> Bifunctor.first (ArrayAccess .) <$> try arrayAccessSuffix
    <|> Bifunctor.first (MethodInv .) <$> try methodInvocationSuffix
    <|> Bifunctor.first (FieldAccess .) <$> fieldAccessSuffix

instanceCreationNPS :: P (Exp Parsed, SourcePos)
instanceCreationNPS = do
  startPos <- getPosition
  tok KW_New
  tas <- NonEmpty.neoptList (noPos typeArgs)
  tds <- typeDeclSpecifier
  (as, argsPos) <- args
  (mcb, pos) <- optionMaybePos argsPos classBody
  return (InstanceCreation (startPos, pos) tas tds as mcb, pos)

typeDeclSpecifier :: P TypeDeclSpecifier
typeDeclSpecifier =
  try
    ( TypeDeclSpecifierWithDiamond
        <$> noPos classType
        <* period
        <*> noPos ident
        <* tok Op_LThan
        <* tok Op_GThan
    )
    <|> try
      ( TypeDeclSpecifierUnqualifiedWithDiamond
          <$> noPos ident
          <* tok Op_LThan
          <* tok Op_GThan
      )
    <|> TypeDeclSpecifier <$> noPos classType

instanceCreationSuffix :: P (Exp Parsed -> Exp Parsed, SourcePos)
instanceCreationSuffix = do
  startPos <- getPosition
  period >> tok KW_New
  tas <- NonEmpty.neoptList (noPos typeArgs)
  i <- noPos ident
  (as, argsPos) <- args
  (mcb, pos) <- optionMaybePos argsPos classBody
  return (\p -> QualInstanceCreation (startPos, pos) p tas i as mcb, pos)

instanceCreation :: P (Exp Parsed)
instanceCreation =
  try (noPos instanceCreationNPS)
    <|> foldl' (&)
      <$> noPos primaryNPS
      <*> many (noPos primarySuffix)
        >>= \case
          icp@QualInstanceCreation {} -> return icp
          _ -> fail ""

lambdaParams :: P (LambdaParams Parsed)
lambdaParams =
  LambdaSingleParam <$> noPos ident
    <|> noPos
      ( parens $
          LambdaFormalParams <$> try (sepBy formalParam comma)
            <|> LambdaInferredParams <$> sepBy (noPos ident) comma
      )

lambdaExp :: P (Exp Parsed, SourcePos)
lambdaExp = do
  startPos <- getPosition
  params <- lambdaParams
  tok LambdaArrow
  (e, pos) <-
    Bifunctor.first LambdaBlock <$> try block
      <|> Bifunctor.first LambdaExpression <$> exp
  return (Lambda (startPos, pos) params e, pos)

methodRef :: P (Exp Parsed, SourcePos)
methodRef = do
  startPos <- getPosition
  n <- noPos name
  tok MethodRefSep
  (mrt, pos) <-
    attrTokWithEndPos KW_New MethodRefConstructor
      <|> Bifunctor.first MethodRefIdent <$> ident
  return (MethodRef (startPos, pos) n mrt, pos)

fieldAccessNPS :: P (FieldAccess Parsed, SourcePos)
fieldAccessNPS = do
  startPos <- getPosition
  do
    tok KW_Super >> void period
    (i, pos) <- ident
    return (SuperFieldAccess (startPos, pos) i, pos)
    <|> do
      n <- noPos name
      period >> tok KW_Super >> void period
      (i, pos) <- ident
      return (ClassFieldAccess (startPos, pos) n i, pos)

fieldAccessSuffix :: P (Exp Parsed -> FieldAccess Parsed, SourcePos)
fieldAccessSuffix = do
  startPos <- getPosition
  void period
  (i, pos) <- ident
  return (\p -> PrimaryFieldAccess (startPos, pos) p i, pos)

fieldAccess :: P (FieldAccess Parsed)
fieldAccess =
  try (noPos fieldAccessNPS)
    <|> foldl' (&)
      <$> noPos primaryNPS
      <*> many (noPos primarySuffix)
        >>= \case
          FieldAccess fa -> return fa
          _ -> fail ""

methodInvocationNPS :: P (MethodInvocation Parsed, SourcePos)
methodInvocationNPS = do
  startPos <- getPosition
  do
    tok KW_Super >> void period
    rts <- NonEmpty.neoptList refTypeArgs
    i <- noPos ident
    (as, pos) <- args
    return (SuperMethodCall (startPos, pos) rts i as, pos)
    <|> do
      (mn, rts, i) <- qualifiedMethodName
      (as, pos) <- args
      return (MethodCall (startPos, pos) mn rts i as, pos)
    <|> do
      n <- noPos name
      void period
      constr <- optionConst TypeMethodCall ClassMethodCall (tok KW_Super >> period)
      rts <- NonEmpty.neoptList refTypeArgs
      i <- noPos ident
      (as, pos) <- args
      return (constr (startPos, pos) n rts i as, pos)

qualifiedMethodName :: P (Maybe Name, [RefType], Ident)
qualifiedMethodName = do
  startPos <- getPosition
  (mn, rts, i) <- go startPos []
  let mn' = case mn of
        ([], _) -> Nothing
        (is, endPos) -> Just (Name (startPos, endPos) (NonEmpty.fromList (reverse is)))
  return (mn', rts, i)
  where
    go :: SourcePos -> [Ident] -> P (([Ident], SourcePos), [RefType], Ident)
    go endPos nids = do
      (i, pos) <- ident
      ((i : nids, pos),,)
        <$> try (period >> refTypeArgs <&> NonEmpty.toList)
        <*> noPos ident
        <|> try (period >> go pos (i : nids))
        <|> return ((nids, endPos), [], i)

methodInvocationSuffix :: P (Exp Parsed -> MethodInvocation Parsed, SourcePos)
methodInvocationSuffix = do
  startPos <- getPosition
  void period
  rts <- NonEmpty.neoptList refTypeArgs
  i <- noPos ident
  (as, pos) <- args
  return (\p -> PrimaryMethodCall (startPos, pos) p rts i as, pos)

methodInvocationExp :: P (Exp Parsed)
methodInvocationExp =
  foldl' (&)
    <$> try (noPos primaryNPS)
    <*> many (noPos primarySuffix)
      >>= \case
        mip@MethodInv {} -> return mip
        _ -> fail ""
    <|> MethodInv <$> noPos methodInvocationNPS

args :: P ([Argument Parsed], SourcePos)
args = parens (sepBy (noPos exp) comma)

-- Arrays

arrayAccessNPS :: P (ArrayIndex Parsed, SourcePos)
arrayAccessNPS = do
  startPos <- getPosition
  n <- noPos name
  (e, pos) <- manyNonEmptyPos (brackets (noPos exp))
  return (ArrayIndex (startPos, pos) (ExpName n) e, pos)

arrayAccessSuffix :: P (Exp Parsed -> ArrayIndex Parsed, SourcePos)
arrayAccessSuffix = do
  startPos <- getPosition
  (e, pos) <- manyNonEmptyPos (brackets (noPos exp))
  return (\ref -> ArrayIndex (startPos, pos) ref e, pos)

arrayAccess :: P (ArrayIndex Parsed, SourcePos)
arrayAccess =
  try arrayAccessNPS
    <|> primaryNoNewArrayNPS |>> primarySuffix >>= \case
      (ArrayAccess ain, pos) -> return (ain, pos)
      _ -> fail ""

arrayCreation :: P (Exp Parsed, SourcePos)
arrayCreation = do
  startPos <- getPosition
  tok KW_New
  t <- nonArrayType
  try do
    ds <- NonEmpty.many emptyBrackets
    (ai, pos) <- arrayInit
    return (ArrayCreateInit (startPos, pos) t (length ds) ai, pos)
    <|> do
      (des, desPos) <- manyNonEmptyPos $ try $ brackets (noPos exp)
      ds <- many emptyBrackets
      let len = length ds
          pos =
            if len < 1
              then desPos
              else last ds
      return (ArrayCreate (startPos, pos) t des len, pos)

literal :: P (Literal, SourcePos)
literal = do
  startPos <- getPosition
  endPos <- getEndPos
  let sp = (startPos, endPos)
  (,endPos) <$> javaToken \case
    IntTok i -> Just (Int sp i)
    LongTok l -> Just (Word sp l)
    DoubleTok d -> Just (Double sp d)
    FloatTok f -> Just (Float sp f)
    CharTok c -> Just (Char sp c)
    StringTok s -> Just (String sp s)
    BoolTok b -> Just (Boolean sp b)
    NullTok -> Just (Null sp)
    _ -> Nothing

-- Operators

preIncDecOp, prefixOp, postfixOp :: P (SourcePos -> Exp Parsed -> Exp Parsed, SourcePos)
preIncDecOp = do
  startPos <- getPosition
  (constr, endPos) <-
    attrTokWithEndPos Op_PPlus PreIncrement
      <|> attrTokWithEndPos Op_MMinus PreDecrement
  return (\expEndPos -> constr (startPos, expEndPos), endPos)
prefixOp = do
  startPos <- getPosition
  (constr, endPos) <-
    attrTokWithEndPos Op_Bang PreNot
      <|> attrTokWithEndPos Op_Tilde PreBitCompl
      <|> attrTokWithEndPos Op_Plus PrePlus
      <|> attrTokWithEndPos Op_Minus PreMinus
  return (\expEndPos -> constr (startPos, expEndPos), endPos)
postfixOp = do
  (constr, endPos) <-
    attrTokWithEndPos Op_PPlus PostIncrement
      <|> attrTokWithEndPos Op_MMinus PostDecrement
  return (\expStartPos -> constr (expStartPos, endPos), endPos)

assignOp :: P AssignOp
assignOp =
  attrTok Op_Equal EqualA
    <|> attrTok Op_StarE MultA
    <|> attrTok Op_SlashE DivA
    <|> attrTok Op_PercentE RemA
    <|> attrTok Op_PlusE AddA
    <|> attrTok Op_MinusE SubA
    <|> attrTok Op_LShiftE LShiftA
    <|> attrTok Op_RShiftE RShiftA
    <|> attrTok Op_RRShiftE RRShiftA
    <|> attrTok Op_AndE AndA
    <|> attrTok Op_CaretE XorA
    <|> attrTok Op_OrE OrA

----------------------------------------------------------------------------
-- Types

ttype :: P Type
ttype =
  RefType <$> try (noPos refType)
    <|> PrimType <$> primType

primType :: P PrimType
primType = do
  startPos <- getPosition
  (constr, endPos) <-
    attrTokWithEndPos KW_Boolean BooleanT
      <|> attrTokWithEndPos KW_Byte ByteT
      <|> attrTokWithEndPos KW_Short ShortT
      <|> attrTokWithEndPos KW_Int IntT
      <|> attrTokWithEndPos KW_Long LongT
      <|> attrTokWithEndPos KW_Char CharT
      <|> attrTokWithEndPos KW_Float FloatT
      <|> attrTokWithEndPos KW_Double DoubleT
  return (constr (startPos, endPos))

refType :: P (RefType, SourcePos)
refType = do
  startPos <- getPosition
  do
    pt <- primType
    pos :| bs <- NonEmpty.many emptyBrackets
    return
      ( Bifunctor.first
          ($ pt)
          ( foldl'
              (f startPos)
              (ArrayType (startPos, pos) . PrimType, pos)
              bs
          )
      )
    <|> do
      (ct, ctPos) <- classType
      bs <- many emptyBrackets
      return
        ( Bifunctor.first
            ($ ct)
            ( foldl'
                (f startPos)
                (ClassRefType, ctPos)
                bs
            )
        )
    <?> "refType"
  where
    f s (c, _) pos = (ArrayType (s, pos) . RefType . c, pos)

nonArrayType :: P Type
nonArrayType =
  PrimType <$> primType
    <|> RefType . ClassRefType <$> noPos classType

classType :: P (ClassType, SourcePos)
classType = do
  startPos <- getPosition
  (cts, pos) <- trySepByNonEmptyPos classTypeSpec period
  return (ClassType (startPos, pos) cts, pos)

classTypeSpec :: P ((Ident, [TypeArgument]), SourcePos)
classTypeSpec = do
  (i, iPos) <- ident
  (tas, pos) <- option ([], iPos) (Bifunctor.first NonEmpty.toList <$> typeArgs)
  return ((i, tas), pos)

resultType :: P (Maybe Type)
resultType =
  attrTok KW_Void Nothing
    <|> Just <$> ttype
    <?> "resultType"

refTypeList :: P (NonEmpty RefType)
refTypeList = NonEmpty.sepBy (noPos refType) comma

----------------------------------------------------------------------------
-- Type parameters and arguments

typeParameters :: P [TypeParam Parsed]
typeParameters = NonEmpty.neoptList $ noPos $ angles $ NonEmpty.sepBy typeParameter comma

typeParameter :: P (TypeParam Parsed)
typeParameter =
  TypeParam
    <$> many (Annotation <$> annotation)
    <*> noPos ident
    <*> NonEmpty.neoptList bounds

bounds :: P (NonEmpty RefType)
bounds = tok KW_Extends >> NonEmpty.sepBy (noPos refType) (tok Op_And)

typeArgs :: P (NonEmpty TypeArgument, SourcePos)
typeArgs = angles $ NonEmpty.sepBy typeArg comma

typeArg :: P TypeArgument
typeArg =
  Wildcard <$ tok Op_Query <*> optionMaybe wildcardBound
    <|> ActualType <$> noPos refType

wildcardBound :: P WildcardBound
wildcardBound =
  ExtendsBound <$ tok KW_Extends <*> noPos refType
    <|> SuperBound <$ tok KW_Super <*> noPos refType

refTypeArgs :: P (NonEmpty RefType)
refTypeArgs = noPos $ angles refTypeList

----------------------------------------------------------------------------
-- Names

name :: P (Name, SourcePos)
name = do
  startPos <- getPosition
  (idents, endPos) <- trySepByNonEmptyPos ident period
  return (Name (startPos, endPos) idents, endPos)

ident :: P (Ident, SourcePos)
ident = do
  startPos <- getPosition
  pos <- getEndPos
  i <- javaToken \case
    IdentTok s -> Just (Ident (startPos, pos) s)
    _ -> Nothing
  return (i, pos)

fixedIdent :: String -> P ()
fixedIdent = tok . IdentTok

attrFixedIdent :: String -> a -> P (a, SourcePos)
attrFixedIdent fixed result = (result,) <$> getEndPos <* fixedIdent fixed

------------------------------------------------------------

-- | @optionMaybePos pos p@ tries to apply parser @p@.
-- If @p@ fails without consuming input, it returns @('Nothing', pos)@,
-- otherwise it returns 'Data.Bifunctor.first' 'Just' the value returned by @p@.
optionMaybePos :: SourcePos -> P (a, SourcePos) -> P (Maybe a, SourcePos)
optionMaybePos pos p = option (Nothing, pos) (Bifunctor.first Just <$> p)

manyNonEmptyPos :: P (a, SourcePos) -> P (NonEmpty a, SourcePos)
manyNonEmptyPos p =
  NonEmpty.many p
    <&> \ll -> (NonEmpty.map fst ll, snd (NonEmpty.last ll))

sepByNonEmptyPos :: P (a, SourcePos) -> P sep -> P (NonEmpty a, SourcePos)
sepByNonEmptyPos p sep = NonEmpty.sepBy p sep <&> \l -> (NonEmpty.map fst l, snd (NonEmpty.last l))

sepEndByPos :: P (a, SourcePos) -> P SourcePos -> P ([a], SourcePos)
sepEndByPos p sep = sepEndBy1Pos <|> ([],) <$> getPosition
  where
    sepEndBy1Pos = do
      (x, pos) <- p
      do
        pos' <- sep
        Bifunctor.first (x :) <$> sepEndBy1Pos
          <|> return ([x], pos')
        <|> return ([x], pos)

trySepByNonEmptyPos :: P (a, SourcePos) -> P sep -> P (NonEmpty a, SourcePos)
trySepByNonEmptyPos p sep = NonEmpty.trySepBy p sep <&> \l -> (NonEmpty.map fst l, snd (NonEmpty.last l))

(|>>) :: P (a, SourcePos) -> P (a -> a, SourcePos) -> P (a, SourcePos)
start |>> suffix =
  foldl' (\(a, _) (s, pos) -> (s a, pos))
    <$> start
    <*> many suffix

------------------------------------------------------------

javaToken :: (Token -> Maybe a) -> P a
javaToken test = token showT posT testT
  where
    showT (L1 _ _ t) = show t
    posT (L1 p _ _) = pos2sourcePos p
    testT (L1 _ _ t) = test t

tok :: Token -> P ()
tok t = javaToken \r -> if r == t then Just () else Nothing

attrTok :: Token -> b -> P b
attrTok t constr = tok t $> constr

attrTokWithEndPos :: Token -> b -> P (b, SourcePos)
attrTokWithEndPos t constr = (constr,) <$> tokWithEndPos t

tokWithEndPos :: Token -> P SourcePos
tokWithEndPos t = getEndPos <* tok t

pos2sourcePos :: (Int, Int) -> SourcePos
pos2sourcePos = uncurry (newPos "")

type Mod a = SourcePos -> [Modifier Parsed] -> a

angles, braces, brackets, parens :: P a -> P (a, SourcePos)
angles = betweenWithEndPos Op_LThan Op_GThan
braces = betweenWithEndPos OpenCurly CloseCurly
brackets = betweenWithEndPos OpenSquare CloseSquare
parens = betweenWithEndPos OpenParen CloseParen

betweenWithEndPos :: Token -> Token -> P a -> P (a, SourcePos)
betweenWithEndPos open close p =
  (,)
    <$ tok open
    <*> p
    <*> tokWithEndPos close

emptyBrackets :: P SourcePos
emptyBrackets = tok OpenSquare >> tokWithEndPos CloseSquare

endSemi :: P a -> P (a, SourcePos)
endSemi p = (,) <$> p <*> semiColon

comma, colon, semiColon, period :: P SourcePos
comma = tokWithEndPos Comma
colon = tokWithEndPos Op_Colon
semiColon = tokWithEndPos SemiColon
period = tokWithEndPos Period
