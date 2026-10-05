{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Java.Parser
  ( JavaParser,
    parser,
    parserWithMode,
    ParserMode (..),
    parserWithState,
    ParserState (..),
    getLocation,
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
    typeParams,
    typeParam,
    name,
    ident,
    manyNonEmptyLoc,
    sepByNonEmptyLoc,
    sepEndByLoc,
    trySepByNonEmptyLoc,
    optionMaybeLoc,
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
import Language.Java.SourceSpan (dummyLocation, locationEof)
import Language.Java.Syntax
import Text.Parsec
  ( Parsec,
    State (stateInput, statePos, stateUser),
    chainl1,
    eof,
    getParserState,
    getState,
    many,
    many1,
    option,
    optionMaybe,
    runParser,
    sepBy,
    sepEndBy,
    skipMany1,
    token,
    try,
    (<?>),
    (<|>),
  )
import Text.Parsec.Extra (manyMaybes, optionBool, optionConst)
import Text.Parsec.NonEmpty (manyNonEmpty, neoptList, sepByNonEmpty, trySepByNonEmpty)
import Text.Parsec.Pos
import Text.ParserCombinators.Parsec.Error
import Prelude hiding (exp, (>>), (>>=))
import qualified Prelude as P ((>>), (>>=))

data ParserState = ParserState
  { ps_mode :: ParserMode,
    ps_locations :: Bool
  }
  deriving (Eq, Show)

data ParserMode
  = ParseFull -- the default
  | ParseShallow -- do not parse methods bodies
  deriving (Eq, Show)

defaultParserState :: ParserState
defaultParserState = ParserState ParseFull True

type JavaParser = Parsec [L1 Token] ParserState

type P = JavaParser

noLoc :: P (a, SourcePos) -> P a
noLoc = fmap fst

getLocation :: P SourcePos
getLocation =
  getParserState <&> \case
    (ps_locations . stateUser -> False) -> dummyLocation
    (stateInput -> []) -> locationEof
    state -> statePos state

getEndLoc :: P SourcePos
getEndLoc =
  getParserState <&> \case
    (ps_locations . stateUser -> False) -> dummyLocation
    state@(stateInput -> L1 (line, column) len _ : _) ->
      let file = sourceName (statePos state)
       in newPos file line (column + len)
    _ -> locationEof

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
parser = parserWithState defaultParserState

parserWithMode :: ParserMode -> P a -> FilePath -> String -> Either ParseError a
parserWithMode mode = parserWithState (ParserState mode True)

parserWithState :: ParserState -> P a -> FilePath -> String -> Either ParseError a
parserWithState state p srcName src =
  case lexer src of
    Left (line, col, err) ->
      let msg = Message err
          pos = newPos srcName line col
       in Left (newErrorMessage msg pos)
    Right tokens ->
      runParser p state srcName tokens

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
    <*> noLoc name
    <* semiColon

importDeclaration :: P ImportDecl
importDeclaration = do
  startLoc <- getLocation
  tok KW_Import
  st <- optionBool $ tok KW_Static
  n <- noLoc name
  star <- optionBool (period >> tok Op_Star)
  endLoc <- semiColon
  return (ImportDecl (startLoc, endLoc) st n star)

typeDeclaration :: P (Maybe (TypeDecl Parsed))
typeDeclaration =
  Just <$> classOrInterfaceDecl
    <|> Nothing <$ semiColon

----------------------------------------------------------------------------
-- Declarations

-- Class declarations

classOrInterfaceDecl :: P (TypeDecl Parsed)
classOrInterfaceDecl = do
  startLoc <- getLocation
  ms <- many modifier
  do
    cd <- noLoc classDecl
    return (ClassTypeDecl (cd startLoc ms))
    <|> do
      iDecl <- annInterfaceDecl <|> interfaceDecl
      return (InterfaceTypeDecl (iDecl startLoc ms))

classDecl :: P (Mod (ClassDecl Parsed), SourcePos)
classDecl =
  normalClassDecl
    <|> recordClassDecl
    <|> enumClassDecl

normalClassDecl :: P (Mod (ClassDecl Parsed), SourcePos)
normalClassDecl = do
  tok KW_Class
  i <- noLoc ident
  tps <- typeParams
  mex <- extends1
  imp <- implements
  (bod, endLoc) <- classBody
  return (\loc ms -> ClassDecl (loc, endLoc) ms i tps mex imp bod, endLoc)

extends1 :: P (Maybe RefType)
extends1 = optionMaybe (tok KW_Extends >> noLoc refType)

extends :: P [RefType]
extends = neoptList (tok KW_Extends >> refTypeList)

permits :: P [RefType]
permits = neoptList (fixedIdent "permits" >> refTypeList)

implements :: P [RefType]
implements = neoptList (tok KW_Implements >> refTypeList)

enumClassDecl :: P (Mod (ClassDecl Parsed), SourcePos)
enumClassDecl = do
  tok KW_Enum
  i <- noLoc ident
  imp <- implements
  (bod, endLoc) <- enumBody
  return (\loc ms -> EnumDecl (loc, endLoc) ms i imp bod, endLoc)

recordClassDecl :: P (Mod (ClassDecl Parsed), SourcePos)
recordClassDecl = do
  tok KW_Record
  i <- noLoc ident
  tps <- typeParams
  fields <- noLoc $ parens $ sepBy recordField comma
  imp <- implements
  (bod, endLoc) <- classBody
  return (\loc ms -> RecordDecl (loc, endLoc) ms i tps fields imp bod, endLoc)
  where
    recordField = RecordFieldDecl <$> ttype <*> noLoc ident

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
    <$> noLoc ident
    <*> option [] (noLoc args)
    <*> optionMaybe (noLoc classBody)

enumBodyDecls :: P [Decl Parsed]
enumBodyDecls = semiColon >> classBodyStatements

classBodyStatements :: P [Decl Parsed]
classBodyStatements = manyMaybes classBodyStatement

-- Interface declarations

annInterfaceDecl :: P (Mod (InterfaceDecl Parsed))
annInterfaceDecl = do
  tok KW_AnnInterface
  i <- noLoc ident
  tps <- typeParams
  exs <- extends
  ps <- permits
  (bod, endLoc) <- interfaceBody
  return \loc ms -> InterfaceDecl (loc, endLoc) InterfaceAnnotation ms i tps exs ps bod

interfaceDecl :: P (Mod (InterfaceDecl Parsed))
interfaceDecl = do
  tok KW_Interface
  i <- noLoc ident
  tps <- typeParams
  exs <- extends
  ps <- permits
  (bod, endLoc) <- interfaceBody
  return \loc ms -> InterfaceDecl (loc, endLoc) InterfaceNormal ms i tps exs ps bod

interfaceBody :: P (InterfaceBody Parsed, SourcePos)
interfaceBody = braces (InterfaceBody <$> manyMaybes interfaceBodyDecl)

-- Declarations

classBodyStatement :: P (Maybe (Decl Parsed))
classBodyStatement =
  skipMany1 semiColon $> Nothing
    <|> try do
      startLoc <- getLocation
      mst <- optionBool (tok KW_Static)
      (bl, endLoc) <- block
      return (Just (InitDecl (startLoc, endLoc) mst bl))
    <|> do
      loc <- getLocation
      ms <- many modifier
      dec <- memberDecl
      return (Just (MemberDecl (dec loc ms)))

memberDecl :: P (Mod (MemberDecl Parsed))
memberDecl =
  try do
    cd <- noLoc classDecl
    return \loc ms -> MemberClassDecl (cd loc ms)
    <|> try do
      iDecl <- try annInterfaceDecl <|> try interfaceDecl
      return \loc ms -> MemberInterfaceDecl (iDecl loc ms)
    <|> try fieldDecl
    <|> try methodDecl
    <|> constrDecl

fieldDecl :: P (Mod (MemberDecl Parsed))
fieldDecl = do
  typ <- ttype
  vds <- varDecls
  endLoc <- semiColon
  return \loc ms -> FieldDecl (loc, endLoc) ms typ vds

methodDecl :: P (Mod (MemberDecl Parsed))
methodDecl = do
  tps <- typeParams
  rt <- resultType
  i <- noLoc ident
  fps <- formalParams
  thr <- throws
  (bod, endLoc) <- methodBody
  return \loc ms -> MethodDecl (loc, endLoc) ms tps rt i fps thr Nothing bod

methodBody :: P (MethodBody Parsed, SourcePos)
methodBody = onlySemi <|> fullBody
  where
    onlySemi = attrTokWithEndLoc SemiColon (MethodBody Nothing)
    fullBody = Bifunctor.first (MethodBody . Just) <$> block

constrDecl :: P (Mod (MemberDecl Parsed))
constrDecl = do
  tps <- typeParams
  i <- noLoc ident
  fps <- option [] formalParams -- record constructors omit the argument list
  thr <- throws
  (bod, endLoc) <- constrBody
  return \loc ms -> ConstructorDecl (loc, endLoc) ms tps i fps thr bod

constrBody :: P (ConstructorBody Parsed, SourcePos)
constrBody =
  braces $
    ConstructorBody
      <$> optionMaybe (try explConstrInv)
      <*> many (noLoc blockStmt)

explConstrInv :: P (ExplConstrInv Parsed)
explConstrInv =
  noLoc . endSemi $
    try
      ( ThisInvoke
          <$> neoptList refTypeArgs
          <* tok KW_This
          <*> noLoc args
      )
      <|> try
        ( SuperInvoke
            <$> neoptList refTypeArgs
            <* tok KW_Super
            <*> noLoc args
        )
      <|> PrimarySuperInvoke
        <$> noLoc primary
        <* period
        <*> neoptList refTypeArgs
        <* tok KW_Super
        <*> noLoc args

-- TODO: This should be parsed like class bodies, and post-checked.
--       That would give far better error messages.
interfaceBodyDecl :: P (Maybe (MemberDecl Parsed))
interfaceBodyDecl =
  semiColon $> Nothing
    <|> do
      loc <- getLocation
      ms <- many modifier
      imd <- interfaceMemberDecl
      return $ Just (imd loc ms)

interfaceMemberDecl :: P (Mod (MemberDecl Parsed))
interfaceMemberDecl =
  do
    cd <- noLoc classDecl
    return \loc ms -> MemberClassDecl (cd loc ms)
    <|> do
      iDecl <- try annInterfaceDecl <|> try interfaceDecl
      return \loc ms -> MemberInterfaceDecl (iDecl loc ms)
    <|> try fieldDecl
    <|> absMethodDecl

absMethodDecl :: P (Mod (MemberDecl Parsed))
absMethodDecl = do
  tps <- typeParams
  rt <- resultType
  i <- noLoc ident
  fps <- formalParams
  thr <- throws
  def <- optionMaybe defaultValue
  endLoc <- semiColon
  return \loc ms -> MethodDecl (loc, endLoc) ms tps rt i fps thr def (MethodBody Nothing)

defaultValue :: P (Exp Parsed)
defaultValue = tok KW_Default >> noLoc exp

throws :: P [RefType]
throws = neoptList (tok KW_Throws >> refTypeList)

-- Formal parameters

formalParams :: P [FormalParam Parsed]
formalParams = noLoc $ parens do
  fps <- sepBy formalParam comma
  fps <$ guard (null fps || all nonVarParam (init fps))
    <|> fail "Only the last formal parameter may be of variable arity"
  where
    nonVarParam :: FormalParam Parsed -> Bool
    nonVarParam (FormalParam _ _ _ b _) = not b

formalParam :: P (FormalParam Parsed)
formalParam = do
  startLoc <- getLocation
  ms <- many modifier
  typ <- ttype
  var <- optionBool ellipsis
  (vdi, loc) <- varDeclId
  return (FormalParam (startLoc, loc) ms typ var vdi)

catchFormalParam :: P (CatchFormalParam Parsed)
catchFormalParam = do
  startLoc <- getLocation
  typs <- sepByNonEmpty ((,) <$> many modifier <*> ttype) (tok Op_Or)
  (vdi, loc) <- varDeclId
  return (CatchFormalParam (startLoc, loc) typs vdi)

ellipsis :: P SourcePos
ellipsis = period >> period >> period

-- Modifiers

modifier :: P (Modifier Parsed)
modifier =
  Annotation <$> annotation
    <|> do
      startLoc <- getLocation
      (constr, endLoc) <-
        attrTokWithEndLoc KW_Public Public
          <|> attrTokWithEndLoc KW_Private Private
          <|> attrTokWithEndLoc KW_Protected Protected
          <|> attrTokWithEndLoc KW_Abstract Abstract
          <|> attrTokWithEndLoc KW_Final Final
          <|> attrTokWithEndLoc KW_Static Static
          <|> attrTokWithEndLoc KW_Strictfp StrictFP
          <|> attrTokWithEndLoc KW_Transient Transient
          <|> attrTokWithEndLoc KW_Volatile Volatile
          <|> attrTokWithEndLoc KW_Native Native
          <|> attrTokWithEndLoc KW_Synchronized Synchronized_
          <|> attrFixedIdent "sealed" Sealed
      return (constr (startLoc, endLoc))

annotation :: P (Annotation Parsed)
annotation = do
  startLoc <- getLocation
  tok Op_AtSign
  (n, nLoc) <- name
  try do
    (elist, endLoc) <- parens evlist
    return (NormalAnnotation (startLoc, endLoc) n elist)
    <|> try do
      (e, endLoc) <- parens elementValue
      return (SingleElementAnnotation (startLoc, endLoc) n e)
    <|> return (MarkerAnnotation (startLoc, nLoc) n)

evlist :: P (NonEmpty (Ident, ElementValue Parsed))
evlist = sepByNonEmpty elementValuePair comma

elementValuePair :: P (Ident, ElementValue Parsed)
elementValuePair = (,) <$> noLoc ident <* tok Op_Equal <*> elementValue

elementValue :: P (ElementValue Parsed)
elementValue =
  EVVal
    <$> ( InitArray <$> noLoc arrayInit
            <|> InitExp <$> noLoc condExp
        )
    <|> EVAnn
      <$> annotation

----------------------------------------------------------------------------
-- Variable declarations

varDecls :: P (NonEmpty (VarDecl Parsed))
varDecls = sepByNonEmpty varDecl comma

varDecl :: P (VarDecl Parsed)
varDecl = do
  startLoc <- getLocation
  (vid, vidLoc) <- varDeclId
  (mvi, endLoc) <- optionMaybeLoc vidLoc (tok Op_Equal >> varInit)
  return (VarDecl (startLoc, endLoc) vid mvi)

varDeclId :: P (VarDeclId, SourcePos)
varDeclId = do
  startLoc <- getLocation
  (i, noArrLoc) <- ident
  ebs <- many emptyBrackets
  let endLoc = case ebs of
        [] -> noArrLoc
        _ -> last ebs
  return (foldl' (\f loc -> VarDeclArray (startLoc, loc) . f) VarId ebs i, endLoc)

localVarDecl :: P (SourcePos, [Modifier Parsed], Type, NonEmpty (VarDecl Parsed))
localVarDecl =
  (,,,)
    <$> getLocation
    <*> many modifier
    <*> ttype
    <*> varDecls

varInit :: P (VarInit Parsed, SourcePos)
varInit =
  Bifunctor.first InitArray <$> arrayInit
    <|> Bifunctor.first InitExp <$> exp

arrayInit :: P (ArrayInit Parsed, SourcePos)
arrayInit = braces do
  startLoc <- getLocation
  (vis, visLoc) <- sepEndByLoc varInit comma
  return (ArrayInit (startLoc, visLoc) vis)

----------------------------------------------------------------------------
-- Statements

block :: P (Block Parsed, SourcePos)
block = do
  startLoc <- getLocation
  state <- getState
  case ps_mode state of
    -- ParseFull -> braces $ Block <$> many (noLoc blockStmt)
    ParseFull -> do
      (bs, loc) <- braces (many (noLoc blockStmt))
      return (Block (startLoc, loc) bs, loc)
    ParseShallow -> shallowP startLoc
  where
    shallowP start = do
      loc <- parseNestedCurly (-1)
      return (Block (start, loc) [], loc)

-- | Parses anything between properly balance curly brackets.
-- level must initially be -1
parseNestedCurly :: Int -> P SourcePos
parseNestedCurly level = do
  loc <- getEndLoc
  newLevel <- javaToken \case
    OpenCurly -> Just (level + 1)
    _ | level < 0 -> Nothing -- need to start with {
    CloseCurly -> Just (level - 1)
    _ -> Just level
  if newLevel < 0 then return loc else parseNestedCurly newLevel

blockStmt :: P (BlockStmt Parsed, SourcePos)
blockStmt =
  try do
    loc <- getLocation
    ms <- many modifier
    (cd, endLoc) <- classDecl
    return (LocalClass (cd loc ms), endLoc)
    <|> try do
      ((startLoc, m, t, vds), endLoc) <- endSemi localVarDecl
      return (LocalVars (startLoc, endLoc) m t vds, endLoc)
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
      startLoc <- getLocation
      tok KW_If
      e <- noLoc $ parens (noLoc exp)
      try do
        th <- noLoc stmtNSI
        tok KW_Else
        (el, endLoc) <- stmt
        return (IfThenElse (startLoc, endLoc) e th el, endLoc)
        <|> do
          (th, endLoc) <- stmt
          return (IfThen (startLoc, endLoc) e th, endLoc)
    whileStmt = do
      startLoc <- getLocation
      tok KW_While
      e <- noLoc $ parens (noLoc exp)
      (s, endLoc) <- stmt
      return (While (startLoc, endLoc) e s, endLoc)
    forStmt = do
      startLoc <- getLocation
      tok KW_For
      f <-
        noLoc $
          parens $
            try do
              fi <- optionMaybe forInit
              void semiColon
              e <- optionMaybe (noLoc exp)
              void semiColon
              fu <- forUp
              return \loc -> BasicFor (startLoc, loc) fi e fu
              <|> do
                ms <- many modifier
                t <- ttype
                i <- noLoc ident
                void colon
                e <- noLoc exp
                return \loc -> EnhancedFor (startLoc, loc) ms t i e
      (s, endLoc) <- stmt
      return (f endLoc s, endLoc)
    labeledStmt = try do
      startLoc <- getLocation
      lbl <- noLoc ident
      void colon
      (s, endLoc) <- stmt
      return (Labeled (startLoc, endLoc) lbl s, endLoc)

stmtNSI :: P (Stmt Parsed, SourcePos)
stmtNSI =
  ifStmt
    <|> whileStmt
    <|> forStmt
    <|> labeledStmt
    <|> stmtNoTrail
  where
    ifStmt = do
      startLoc <- getLocation
      tok KW_If
      e <- noLoc $ parens (noLoc exp)
      th <- noLoc stmtNSI
      tok KW_Else
      (el, endLoc) <- stmtNSI
      return (IfThenElse (startLoc, endLoc) e th el, endLoc)
    whileStmt = do
      startLoc <- getLocation
      tok KW_While
      e <- noLoc $ parens (noLoc exp)
      (s, endLoc) <- stmtNSI
      return (While (startLoc, endLoc) e s, endLoc)
    forStmt = do
      startLoc <- getLocation
      tok KW_For
      f <-
        noLoc $
          parens $
            try do
              fi <- optionMaybe forInit
              void semiColon
              e <- optionMaybe (noLoc exp)
              void semiColon
              fu <- forUp
              return \loc -> BasicFor (startLoc, loc) fi e fu
              <|> do
                ms <- many modifier
                t <- ttype
                i <- noLoc ident
                void colon
                e <- noLoc exp
                return \loc -> EnhancedFor (startLoc, loc) ms t i e
      (s, endLoc) <- stmtNSI
      return (f endLoc s, endLoc)
    labeledStmt = try do
      startLoc <- getLocation
      i <- noLoc ident
      void colon
      (s, endLoc) <- stmtNSI
      return (Labeled (startLoc, endLoc) i s, endLoc)

stmtNoTrail :: P (Stmt Parsed, SourcePos)
stmtNoTrail =
  getLocation >>= \startLoc ->
    -- empty statement
    do
      loc <- semiColon
      return (Empty (startLoc, loc), loc)
      <|>
      -- inner block
      Bifunctor.first StmtBlock <$> block
      <|>
      -- assertions
      do
        tok KW_Assert
        e <- noLoc exp
        me2 <- optionMaybe $ colon >> noLoc exp
        loc <- semiColon
        return (Assert (startLoc, loc) e me2, loc)
      <|>
      -- switch stmts
      do
        tok KW_Switch
        e <- noLoc $ parens (noLoc exp)
        ((style, sb), endLoc) <- switchBlock
        return (Switch (startLoc, endLoc) style e sb, endLoc)
      <|>
      -- do-while loops
      do
        tok KW_Do
        s <- noLoc stmt
        tok KW_While
        e <- noLoc $ parens (noLoc exp)
        endLoc <- semiColon
        return (Do (startLoc, endLoc) s e, endLoc)
      <|>
      -- break
      do
        tok KW_Break
        mi <- optionMaybe (noLoc ident)
        endLoc <- semiColon
        return (Break (startLoc, endLoc) mi, endLoc)
      <|>
      -- continue
      do
        tok KW_Continue
        mi <- optionMaybe (noLoc ident)
        loc <- semiColon
        return (Continue (startLoc, loc) mi, loc)
      <|>
      -- return
      do
        tok KW_Return
        me <- optionMaybe (noLoc exp)
        endLoc <- semiColon
        return (Return (startLoc, endLoc) me, endLoc)
      <|>
      -- synchronized
      do
        tok KW_Synchronized
        e <- noLoc $ parens (noLoc exp)
        (b, endLoc) <- block
        return (Synchronized (startLoc, endLoc) e b, endLoc)
      <|>
      -- throw
      do
        tok KW_Throw
        e <- noLoc exp
        loc <- semiColon
        return (Throw (startLoc, loc) e, loc)
      <|>
      -- try-catch, both with and without a finally clause
      do
        tok KW_Try
        resources <- tryResourceList
        (b, blockEndLoc) <- block
        cs <- many catch
        let cEndLoc = maybe blockEndLoc (snd . snd) (unsnoc cs)
        (mf, endLoc) <- optionMaybeLoc cEndLoc (tok KW_Finally >> block)
        when (null resources && null cs && null mf) $
          fail "try needs at least one catch or finally clause"
        return
          ( Try (startLoc, endLoc) resources b (map fst cs) mf,
            endLoc
          )
      <|>
      -- expressions as stmts
      do
        (s, endLoc) <- endSemi stmtExp
        return (ExpStmt (startLoc, endLoc) s, endLoc)

-- For loops

forInit :: P (ForInit Parsed)
forInit =
  try do
    (_, m, t, vds) <- localVarDecl
    return (ForLocalVars m t vds)
    <|> ForInitExps <$> sepByNonEmpty stmtExp comma

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
  startLoc <- getLocation
  (lbl, lblLoc) <- switchLabelOld
  bswithLocs <- many blockStmt
  let bss = map fst bswithLocs
      loc = maybe lblLoc (snd . snd) (unsnoc bswithLocs)
  return (SwitchBlock (startLoc, loc) lbl bss)

switchLabelOld :: P (SwitchLabel Parsed, SourcePos)
switchLabelOld =
  tok KW_Default >> attrTokWithEndLoc Op_Colon Default
    <|> SwitchCase
      <$ tok KW_Case
      <*> sepByNonEmpty (noLoc condExp) comma
        >>= attrTokWithEndLoc Op_Colon

switchStmtNew :: P (SwitchBlock Parsed)
switchStmtNew = do
  startLoc <- getLocation
  lbl <- switchLabelNew
  (bss, loc) <- braces (many (noLoc blockStmt)) <|> (blockStmt <&> \s -> ([fst s], snd s))
  return (SwitchBlock (startLoc, loc) lbl bss)

switchLabelNew :: P (SwitchLabel Parsed)
switchLabelNew =
  SwitchCase
    <$ tok KW_Case
    <*> sepByNonEmpty (noLoc condExp) comma
    <* tok LambdaArrow
    <|> Default <$ tok KW_Default <* tok LambdaArrow

switchExp :: P (Exp Parsed, SourcePos)
switchExp = do
  startLoc <- getLocation
  tok KW_Switch
  e <- noLoc $ parens (noLoc exp)
  (branches, loc) <- braces switchExpBody
  return (SwitchExp (startLoc, loc) e branches, loc)
  where
    switchExpBody = NonEmpty.fromList <$> many1 switchExpBodyBranch
    switchExpBodyBranch = do
      lbl <- switchLabelNew
      body <-
        SwitchExpBranchBlock <$> noLoc (braces (many (noLoc blockStmt)))
          <|> SwitchExpBranchBlock <$> try (blockStmt <&> pure . fst)
          <|> SwitchExpBranchExp <$> branchExp
      return (SwitchExpBranch lbl body)
    branchExp = noLoc exp <* semiColon

-- Try-catch clauses

catch :: P (Catch Parsed, SourcePos)
catch = do
  tok KW_Catch
  fp <- Catch <$> noLoc (parens catchFormalParam)
  Bifunctor.first fp <$> block

tryResourceList :: P [TryResource Parsed]
tryResourceList = do
  option [] $ noLoc $ parens $ sepEndBy tryResource semiColon
  where
    tryResource =
      TryResourceVarDecl <$> try resourceDecl
        <|> TryResourceQualAccess <$> try fieldAccess
        <|> TryResourceVarAccess <$> noLoc ident

resourceDecl :: P (ResourceDecl Parsed)
resourceDecl =
  ResourceDecl
    <$> many modifier
    <*> ttype
    <*> noLoc varDeclId
    <* tok Op_Equal
    <*> noLoc varInit

----------------------------------------------------------------------------
-- Expressions

stmtExp :: P (Exp Parsed)
stmtExp =
  try (noLoc preIncDec)
    <|> try postIncDec
    <|> try (noLoc assignment)
    -- There are sharing gains to be made by unifying these two
    <|> try methodInvocationExp
    <|> try (noLoc lambdaExp)
    <|> try (noLoc methodRef)
    <|> instanceCreation

preIncDec :: P (Exp Parsed, SourcePos)
preIncDec = do
  op <- noLoc preIncDecOp
  (e, eLoc) <- unaryExp
  return (op eLoc e, eLoc)

postIncDec :: P (Exp Parsed)
postIncDec = do
  startLoc <- getLocation
  e <- noLoc postfixExpNES
  ops <- manyNonEmpty (noLoc postfixOp)
  return (foldl' (\a s -> s startLoc a) e ops)

assignment :: P (Exp Parsed, SourcePos)
assignment = do
  startLoc <- getLocation
  lh <- lhs
  op <- assignOp
  (e, loc) <- assignExp
  return (Assign (startLoc, loc) lh op e, loc)

lhs :: P (Lhs Parsed)
lhs =
  FieldLhs <$> try fieldAccess
    <|> ArrayLhs <$> try (noLoc arrayAccess)
    <|> NameLhs <$> noLoc name

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
  startLoc <- getLocation
  condOrExp |>> condExpSuffix startLoc

condExpSuffix :: SourcePos -> P (Exp Parsed -> Exp Parsed, SourcePos)
condExpSuffix startLoc = do
  tok Op_Query
  th <- noLoc exp
  void colon
  (el, endLoc) <- condExp
  return (\ce -> Cond (startLoc, endLoc) ce th el, endLoc)

infixlExpList :: P (Exp Parsed, SourcePos) -> P Op -> P (Exp Parsed, SourcePos)
infixlExpList p sep = do
  startLoc <- getLocation
  p `chainl1` do
    op <- sep
    return \(e1, _) (e2, l2) -> (BinOp (startLoc, l2) e1 op e2, l2)

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
  startLoc <- getLocation
  (e, endLoc) <-
    infixlExpList shiftExp $
      attrTok Op_LThan LThan
        <|> attrTok Op_GThan GThan
        <|> attrTok Op_LThanE LThanE
        <|> attrTok Op_GThanE GThanE
  option (e, endLoc) do
    tok KW_Instanceof
    (t, refLoc) <- refType
    (mName, loc) <- optionMaybeLoc refLoc name
    return (InstanceOf (startLoc, loc) e t mName, loc)

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
      op <- noLoc prefixOp
      (e, eLoc) <- unaryExp
      return (op eLoc e, eLoc)
    <|> try do
      startLoc <- getLocation
      t <- noLoc (parens ttype)
      (e, loc) <- unaryExp
      return (Cast (startLoc, loc) t e, loc)
    <|> postfixExp

postfixExpNES :: P (Exp Parsed, SourcePos)
postfixExpNES =
  try primary
    <|> Bifunctor.first ExpName <$> name

postfixExp :: P (Exp Parsed, SourcePos)
postfixExp = do
  startLoc <- getLocation
  postfixExpNES |>> (Bifunctor.first (\op -> op startLoc) <$> postfixOp)

primary :: P (Exp Parsed, SourcePos)
primary = primaryNPS |>> primarySuffix

primaryNPS :: P (Exp Parsed, SourcePos)
primaryNPS =
  try arrayCreation
    <|> primaryNoNewArrayNPS

primaryNoNewArrayNPS :: P (Exp Parsed, SourcePos)
primaryNoNewArrayNPS = do
  startLoc <- getLocation
  Bifunctor.first Lit <$> literal
    <|> do
      loc <- tokWithEndLoc KW_This
      return (This (startLoc, loc), loc)
    <|> parens (noLoc exp)
    <|> try do
      rt <- resultType
      void period
      loc <- tokWithEndLoc KW_Class
      return (ClassLit (startLoc, loc) rt, loc)
    <|> try do
      n <- noLoc name
      void period
      loc <- tokWithEndLoc KW_This
      return (ThisClass (startLoc, loc) n, loc)
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
  startLoc <- getLocation
  tok KW_New
  tas <- neoptList (noLoc typeArgs)
  tds <- typeDeclSpecifier
  (as, argsLoc) <- args
  (mcb, loc) <- optionMaybeLoc argsLoc classBody
  return (InstanceCreation (startLoc, loc) tas tds as mcb, loc)

typeDeclSpecifier :: P TypeDeclSpecifier
typeDeclSpecifier =
  try
    ( TypeDeclSpecifierWithDiamond
        <$> noLoc classType
        <* period
        <*> noLoc ident
        <* tok Op_LThan
        <* tok Op_GThan
    )
    <|> try
      ( TypeDeclSpecifierUnqualifiedWithDiamond
          <$> noLoc ident
          <* tok Op_LThan
          <* tok Op_GThan
      )
    <|> TypeDeclSpecifier <$> noLoc classType

instanceCreationSuffix :: P (Exp Parsed -> Exp Parsed, SourcePos)
instanceCreationSuffix = do
  startLoc <- getLocation
  period >> tok KW_New
  tas <- neoptList (noLoc typeArgs)
  i <- noLoc ident
  (as, argsLoc) <- args
  (mcb, loc) <- optionMaybeLoc argsLoc classBody
  return (\p -> QualInstanceCreation (startLoc, loc) p tas i as mcb, loc)

instanceCreation :: P (Exp Parsed)
instanceCreation =
  try (noLoc instanceCreationNPS)
    <|> foldl' (&)
      <$> noLoc primaryNPS
      <*> many (noLoc primarySuffix)
        >>= \case
          icp@QualInstanceCreation {} -> return icp
          _ -> fail ""

lambdaParams :: P (LambdaParams Parsed)
lambdaParams =
  LambdaSingleParam <$> noLoc ident
    <|> noLoc
      ( parens $
          LambdaFormalParams <$> try (sepBy formalParam comma)
            <|> LambdaInferredParams <$> sepBy (noLoc ident) comma
      )

lambdaExp :: P (Exp Parsed, SourcePos)
lambdaExp = do
  startLoc <- getLocation
  params <- lambdaParams
  tok LambdaArrow
  (e, loc) <-
    Bifunctor.first LambdaBlock <$> try block
      <|> Bifunctor.first LambdaExpression <$> exp
  return (Lambda (startLoc, loc) params e, loc)

methodRef :: P (Exp Parsed, SourcePos)
methodRef = do
  startLoc <- getLocation
  n <- noLoc name
  tok MethodRefSep
  (mrt, loc) <-
    attrTokWithEndLoc KW_New MethodRefConstructor
      <|> Bifunctor.first MethodRefIdent <$> ident
  return (MethodRef (startLoc, loc) n mrt, loc)

fieldAccessNPS :: P (FieldAccess Parsed, SourcePos)
fieldAccessNPS = do
  startLoc <- getLocation
  do
    tok KW_Super >> void period
    (i, loc) <- ident
    return (SuperFieldAccess (startLoc, loc) i, loc)
    <|> do
      n <- noLoc name
      period >> tok KW_Super >> void period
      (i, loc) <- ident
      return (ClassFieldAccess (startLoc, loc) n i, loc)

fieldAccessSuffix :: P (Exp Parsed -> FieldAccess Parsed, SourcePos)
fieldAccessSuffix = do
  startLoc <- getLocation
  void period
  (i, loc) <- ident
  return (\p -> PrimaryFieldAccess (startLoc, loc) p i, loc)

fieldAccess :: P (FieldAccess Parsed)
fieldAccess =
  try (noLoc fieldAccessNPS)
    <|> foldl' (&)
      <$> noLoc primaryNPS
      <*> many (noLoc primarySuffix)
        >>= \case
          FieldAccess fa -> return fa
          _ -> fail ""

methodInvocationNPS :: P (MethodInvocation Parsed, SourcePos)
methodInvocationNPS = do
  startLoc <- getLocation
  do
    tok KW_Super >> void period
    rts <- neoptList refTypeArgs
    i <- noLoc ident
    (as, loc) <- args
    return (SuperMethodCall (startLoc, loc) rts i as, loc)
    <|> do
      (mn, rts, i) <- qualifiedMethodName
      (as, loc) <- args
      return (MethodCall (startLoc, loc) mn rts i as, loc)
    <|> do
      n <- noLoc name
      void period
      constr <- optionConst TypeMethodCall ClassMethodCall (tok KW_Super >> period)
      rts <- neoptList refTypeArgs
      i <- noLoc ident
      (as, loc) <- args
      return (constr (startLoc, loc) n rts i as, loc)

qualifiedMethodName :: P (Maybe Name, [RefType], Ident)
qualifiedMethodName = do
  startLoc <- getLocation
  (mn, rts, i) <- go startLoc []
  let mn' = case mn of
        ([], _) -> Nothing
        (is, endLoc) -> Just (Name (startLoc, endLoc) (NonEmpty.fromList (reverse is)))
  return (mn', rts, i)
  where
    go :: SourcePos -> [Ident] -> P (([Ident], SourcePos), [RefType], Ident)
    go endLoc nids = do
      (i, loc) <- ident
      ((i : nids, loc),,)
        <$> try (period >> refTypeArgs <&> NonEmpty.toList)
        <*> noLoc ident
        <|> try (period >> go loc (i : nids))
        <|> return ((nids, endLoc), [], i)

methodInvocationSuffix :: P (Exp Parsed -> MethodInvocation Parsed, SourcePos)
methodInvocationSuffix = do
  startLoc <- getLocation
  void period
  rts <- neoptList refTypeArgs
  i <- noLoc ident
  (as, loc) <- args
  return (\p -> PrimaryMethodCall (startLoc, loc) p rts i as, loc)

methodInvocationExp :: P (Exp Parsed)
methodInvocationExp =
  foldl' (&)
    <$> try (noLoc primaryNPS)
    <*> many (noLoc primarySuffix)
      >>= \case
        mip@MethodInv {} -> return mip
        _ -> fail ""
    <|> MethodInv <$> noLoc methodInvocationNPS

args :: P ([Argument Parsed], SourcePos)
args = parens (sepBy (noLoc exp) comma)

-- Arrays

arrayAccessNPS :: P (ArrayIndex Parsed, SourcePos)
arrayAccessNPS = do
  startLoc <- getLocation
  n <- noLoc name
  (e, loc) <- manyNonEmptyLoc (brackets (noLoc exp))
  return (ArrayIndex (startLoc, loc) (ExpName n) e, loc)

arrayAccessSuffix :: P (Exp Parsed -> ArrayIndex Parsed, SourcePos)
arrayAccessSuffix = do
  startLoc <- getLocation
  (e, loc) <- manyNonEmptyLoc (brackets (noLoc exp))
  return (\ref -> ArrayIndex (startLoc, loc) ref e, loc)

arrayAccess :: P (ArrayIndex Parsed, SourcePos)
arrayAccess =
  try arrayAccessNPS
    <|> primaryNoNewArrayNPS |>> primarySuffix >>= \case
      (ArrayAccess ain, loc) -> return (ain, loc)
      _ -> fail ""

arrayCreation :: P (Exp Parsed, SourcePos)
arrayCreation = do
  startLoc <- getLocation
  tok KW_New
  t <- nonArrayType
  try do
    ds <- manyNonEmpty emptyBrackets
    (ai, loc) <- arrayInit
    return (ArrayCreateInit (startLoc, loc) t (length ds) ai, loc)
    <|> do
      (des, desLoc) <- manyNonEmptyLoc $ try $ brackets (noLoc exp)
      ds <- many emptyBrackets
      let len = length ds
          loc =
            if len < 1
              then desLoc
              else last ds
      return (ArrayCreate (startLoc, loc) t des len, loc)

literal :: P (Literal, SourcePos)
literal = do
  startLoc <- getLocation
  endLoc <- getEndLoc
  let sp = (startLoc, endLoc)
  (,endLoc) <$> javaToken \case
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
  startLoc <- getLocation
  (constr, endLoc) <-
    attrTokWithEndLoc Op_PPlus PreIncrement
      <|> attrTokWithEndLoc Op_MMinus PreDecrement
  return (\expEndLoc -> constr (startLoc, expEndLoc), endLoc)
prefixOp = do
  startLoc <- getLocation
  (constr, endLoc) <-
    attrTokWithEndLoc Op_Bang PreNot
      <|> attrTokWithEndLoc Op_Tilde PreBitCompl
      <|> attrTokWithEndLoc Op_Plus PrePlus
      <|> attrTokWithEndLoc Op_Minus PreMinus
  return (\expEndLoc -> constr (startLoc, expEndLoc), endLoc)
postfixOp = do
  (constr, endLoc) <-
    attrTokWithEndLoc Op_PPlus PostIncrement
      <|> attrTokWithEndLoc Op_MMinus PostDecrement
  return (\expStartLoc -> constr (expStartLoc, endLoc), endLoc)

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
  RefType <$> try (noLoc refType)
    <|> PrimType <$> primType

primType :: P PrimType
primType = do
  startLoc <- getLocation
  (constr, endLoc) <-
    attrTokWithEndLoc KW_Boolean BooleanT
      <|> attrTokWithEndLoc KW_Byte ByteT
      <|> attrTokWithEndLoc KW_Short ShortT
      <|> attrTokWithEndLoc KW_Int IntT
      <|> attrTokWithEndLoc KW_Long LongT
      <|> attrTokWithEndLoc KW_Char CharT
      <|> attrTokWithEndLoc KW_Float FloatT
      <|> attrTokWithEndLoc KW_Double DoubleT
  return (constr (startLoc, endLoc))

refType :: P (RefType, SourcePos)
refType = do
  startLoc <- getLocation
  do
    pt <- primType
    loc :| bs <- manyNonEmpty emptyBrackets
    return
      ( Bifunctor.first
          ($ pt)
          ( foldl'
              (f startLoc)
              (ArrayType (startLoc, loc) . PrimType, loc)
              bs
          )
      )
    <|> do
      (ct, ctLoc) <- classType
      bs <- many emptyBrackets
      return
        ( Bifunctor.first
            ($ ct)
            ( foldl'
                (f startLoc)
                (ClassRefType, ctLoc)
                bs
            )
        )
    <?> "refType"
  where
    f s (c, _) loc = (ArrayType (s, loc) . RefType . c, loc)

nonArrayType :: P Type
nonArrayType =
  PrimType <$> primType
    <|> RefType . ClassRefType <$> noLoc classType

classType :: P (ClassType, SourcePos)
classType = do
  startLoc <- getLocation
  (cts, loc) <- trySepByNonEmptyLoc classTypeSpec period
  return (ClassType (startLoc, loc) cts, loc)

classTypeSpec :: P ((Ident, [TypeArgument]), SourcePos)
classTypeSpec = do
  (i, iLoc) <- ident
  (tas, loc) <- option ([], iLoc) (Bifunctor.first NonEmpty.toList <$> typeArgs)
  return ((i, tas), loc)

resultType :: P (Maybe Type)
resultType =
  attrTok KW_Void Nothing
    <|> Just <$> ttype
    <?> "resultType"

refTypeList :: P (NonEmpty RefType)
refTypeList = sepByNonEmpty (noLoc refType) comma

----------------------------------------------------------------------------
-- Type parameters and arguments

typeParams :: P [TypeParam]
typeParams = neoptList $ noLoc $ angles $ sepByNonEmpty typeParam comma

typeParam :: P TypeParam
typeParam = TypeParam <$> noLoc ident <*> neoptList bounds

bounds :: P (NonEmpty RefType)
bounds = tok KW_Extends >> sepByNonEmpty (noLoc refType) (tok Op_And)

typeArgs :: P (NonEmpty TypeArgument, SourcePos)
typeArgs = angles $ sepByNonEmpty typeArg comma

typeArg :: P TypeArgument
typeArg =
  Wildcard <$ tok Op_Query <*> optionMaybe wildcardBound
    <|> ActualType <$> noLoc refType

wildcardBound :: P WildcardBound
wildcardBound =
  ExtendsBound <$ tok KW_Extends <*> noLoc refType
    <|> SuperBound <$ tok KW_Super <*> noLoc refType

refTypeArgs :: P (NonEmpty RefType)
refTypeArgs = noLoc $ angles refTypeList

----------------------------------------------------------------------------
-- Names

name :: P (Name, SourcePos)
name = do
  startLoc <- getLocation
  (idents, endLoc) <- trySepByNonEmptyLoc ident period
  return (Name (startLoc, endLoc) idents, endLoc)

ident :: P (Ident, SourcePos)
ident = do
  startLoc <- getLocation
  loc <- getEndLoc
  i <- javaToken \case
    IdentTok s -> Just (Ident (startLoc, loc) s)
    _ -> Nothing
  return (i, loc)

fixedIdent :: String -> P ()
fixedIdent = tok . IdentTok

attrFixedIdent :: String -> a -> P (a, SourcePos)
attrFixedIdent fixed result = (result,) <$> getEndLoc <* fixedIdent fixed

------------------------------------------------------------

-- | @optionMaybeLoc loc p@ tries to apply parser @p@.
-- If @p@ fails without consuming input, it returns @('Nothing', loc)@,
-- otherwise it returns 'Data.Bifunctor.first' 'Just' the value returned by @p@.
optionMaybeLoc :: SourcePos -> P (a, SourcePos) -> P (Maybe a, SourcePos)
optionMaybeLoc loc p = option (Nothing, loc) (Bifunctor.first Just <$> p)

manyNonEmptyLoc :: P (a, SourcePos) -> P (NonEmpty a, SourcePos)
manyNonEmptyLoc p =
  manyNonEmpty p
    <&> \ll -> (NonEmpty.map fst ll, snd (NonEmpty.last ll))

sepByNonEmptyLoc :: P (a, SourcePos) -> P sep -> P (NonEmpty a, SourcePos)
sepByNonEmptyLoc p sep = sepByNonEmpty p sep <&> \l -> (NonEmpty.map fst l, snd (NonEmpty.last l))

sepEndByLoc :: P (a, SourcePos) -> P SourcePos -> P ([a], SourcePos)
sepEndByLoc p sep = sepEndBy1Loc <|> ([],) <$> getLocation
  where
    sepEndBy1Loc = do
      (x, loc) <- p
      do
        loc' <- sep
        Bifunctor.first (x :) <$> sepEndBy1Loc
          <|> return ([x], loc')
        <|> return ([x], loc)

trySepByNonEmptyLoc :: P (a, SourcePos) -> P sep -> P (NonEmpty a, SourcePos)
trySepByNonEmptyLoc p sep = trySepByNonEmpty p sep <&> \l -> (NonEmpty.map fst l, snd (NonEmpty.last l))

(|>>) :: P (a, SourcePos) -> P (a -> a, SourcePos) -> P (a, SourcePos)
start |>> suffix =
  foldl' (\(a, _) (s, loc) -> (s a, loc))
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

attrTokWithEndLoc :: Token -> b -> P (b, SourcePos)
attrTokWithEndLoc t constr = (constr,) <$> tokWithEndLoc t

tokWithEndLoc :: Token -> P SourcePos
tokWithEndLoc t = getEndLoc <* tok t

pos2sourcePos :: (Int, Int) -> SourcePos
pos2sourcePos = uncurry (newPos "")

type Mod a = SourcePos -> [Modifier Parsed] -> a

angles, braces, brackets, parens :: P a -> P (a, SourcePos)
angles = betweenWithEndLoc Op_LThan Op_GThan
braces = betweenWithEndLoc OpenCurly CloseCurly
brackets = betweenWithEndLoc OpenSquare CloseSquare
parens = betweenWithEndLoc OpenParen CloseParen

betweenWithEndLoc :: Token -> Token -> P a -> P (a, SourcePos)
betweenWithEndLoc open close p =
  (,)
    <$ tok open
    <*> p
    <*> tokWithEndLoc close

emptyBrackets :: P SourcePos
emptyBrackets = tok OpenSquare >> tokWithEndLoc CloseSquare

endSemi :: P a -> P (a, SourcePos)
endSemi p = (,) <$> p <*> semiColon

comma, colon, semiColon, period :: P SourcePos
comma = tokWithEndLoc Comma
colon = tokWithEndLoc Op_Colon
semiColon = tokWithEndLoc SemiColon
period = tokWithEndLoc Period
