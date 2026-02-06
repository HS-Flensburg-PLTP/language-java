{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.Java.Parser
  ( JavaParser,
    parser,
    parserWithMode,
    ParserMode (..),
    parserWithState,
    ParserState (..),
    getLocation,
    compilationUnit,
    packageDecl,
    importDecl,
    typeDecl,
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
    empty,
    list,
    list1,
    seplist,
    seplist1,
    opt,
    bopt,
    lopt,
    comma,
    semiColon,
    period,
    colon,
  )
where

import Data.Functor (($>), (<&>))
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Language.Java.Lexer (L1 (..), Token (..), lexer)
import Language.Java.SourceSpan (Location (..), dummyLocation, locationEof)
import Language.Java.Syntax
import Text.Parsec
  ( Parsec,
    State (stateInput, statePos),
    chainl1,
    eof,
    getParserState,
    getState,
    many1,
    option,
    optionMaybe,
    optional,
    runParser,
    token,
    try,
    (<?>),
    (<|>),
  )
import Text.Parsec.Pos
import Text.ParserCombinators.Parsec.Error
import Prelude hiding (exp, (>>), (>>=))
import qualified Prelude as P ((>>), (>>=))

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

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

noLoc :: P (a, Location) -> P a
noLoc = fmap fst

getLocation :: P Location
getLocation = do
  myState <- getState
  if ps_locations myState
    then do
      state <- getParserState
      case stateInput state of
        [] -> return locationEof
        _ ->
          let p = statePos state
           in return $ Location {loc_file = sourceName p, loc_line = sourceLine p, loc_column = sourceColumn p}
    else return dummyLocation

getEndLoc :: P Location
getEndLoc = do
  state <- getState
  if ps_locations state
    then do
      parserState <- getParserState
      case stateInput parserState of
        [] -> return locationEof
        L1 (line, column) len _ : _ ->
          let file = sourceName (statePos parserState)
           in return (Location {loc_file = file, loc_line = line, loc_column = column + len})
    else return dummyLocation

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
compilationUnit = do
  mpd <- opt packageDecl
  ids <- list importDecl
  tds <- list typeDecl
  eof
  return $ CompilationUnit mpd ids (catMaybes tds)

packageDecl :: P PackageDecl
packageDecl = do
  tok KW_Package
  n <- noLoc name
  semiColon
  return $ PackageDecl n

importDecl :: P ImportDecl
importDecl = do
  startLoc <- getLocation
  tok KW_Import
  st <- bopt $ tok KW_Static
  n <- noLoc name
  star <- bopt (period >> tok Op_Star)
  (_, endLoc) <- tokWithEndLoc SemiColon
  return (ImportDecl (startLoc, endLoc) st n star)

typeDecl :: P (Maybe (TypeDecl Parsed))
typeDecl =
  Just <$> classOrInterfaceDecl
    <|> Nothing <$ semiColon

----------------------------------------------------------------------------
-- Declarations

-- Class declarations

classOrInterfaceDecl :: P (TypeDecl Parsed)
classOrInterfaceDecl = do
  startLoc <- getLocation
  ms <- list modifier
  ( do
      cd <- noLoc classDecl
      return (ClassTypeDecl (cd startLoc ms))
    )
    <|> ( do
            iDecl <- annInterfaceDecl <|> interfaceDecl
            return (InterfaceTypeDecl (iDecl startLoc ms))
        )

classDecl :: P (Mod (ClassDecl Parsed), Location)
classDecl = normalClassDecl <|> recordClassDecl <|> enumClassDecl

normalClassDecl :: P (Mod (ClassDecl Parsed), Location)
normalClassDecl = do
  tok KW_Class
  i <- noLoc ident
  tps <- neoptList typeParams
  mex <- opt extends
  imp <- neoptList implements
  (bod, endLoc) <- classBody
  return (\loc ms -> ClassDecl (loc, endLoc) ms i tps (fmap NonEmpty.head mex) imp bod, endLoc)

extends :: P (NonEmpty RefType)
extends = tok KW_Extends >> refTypeList

permits :: P (NonEmpty RefType)
permits = fixedIdent "permits" () >> refTypeList

implements :: P (NonEmpty RefType)
implements = tok KW_Implements >> refTypeList

enumClassDecl :: P (Mod (ClassDecl Parsed), Location)
enumClassDecl = do
  tok KW_Enum
  i <- noLoc ident
  imp <- neoptList implements
  (bod, endLoc) <- enumBody
  return (\loc ms -> EnumDecl (loc, endLoc) ms i imp bod, endLoc)

recordClassDecl :: P (Mod (ClassDecl Parsed), Location)
recordClassDecl = do
  tok KW_Record
  i <- noLoc ident
  tps <- neoptList typeParams
  fields <- noLoc $ parens (seplistNoLoc recordField comma)
  imp <- neoptList implements
  (bod, endLoc) <- classBody
  return (\loc ms -> RecordDecl (loc, endLoc) ms i tps fields imp bod, endLoc)
  where
    recordField = do
      typ <- ttype
      i <- noLoc ident
      return $ RecordFieldDecl typ i

classBody :: P (ClassBody Parsed, Location)
classBody = do
  (b, loc) <- braces classBodyStatements
  return (ClassBody b, loc)

enumBody :: P (EnumBody Parsed, Location)
enumBody = braces $ do
  ecs <- seplistNoLoc enumConst comma
  optional comma
  eds <- lopt enumBodyDecls
  return $ EnumBody ecs eds

enumConst :: P (EnumConstant Parsed)
enumConst = do
  i <- noLoc ident
  as <- lopt (noLoc args)
  mcb <- opt (noLoc classBody)
  return $ EnumConstant i as mcb

enumBodyDecls :: P [Decl Parsed]
enumBodyDecls = semiColon >> classBodyStatements

classBodyStatements :: P [Decl Parsed]
classBodyStatements = catMaybes <$> list classBodyStatement

-- Interface declarations

annInterfaceDecl :: P (Mod (InterfaceDecl Parsed))
annInterfaceDecl = do
  tok KW_AnnInterface
  i <- noLoc ident
  tps <- neoptList typeParams
  exs <- neoptList extends
  ps <- neoptList permits
  (bod, endLoc) <- interfaceBody
  return $ \loc ms -> InterfaceDecl (loc, endLoc) InterfaceAnnotation ms i tps exs ps bod

interfaceDecl :: P (Mod (InterfaceDecl Parsed))
interfaceDecl = do
  tok KW_Interface
  i <- noLoc ident
  tps <- neoptList typeParams
  exs <- neoptList extends
  ps <- neoptList permits
  (bod, endLoc) <- interfaceBody
  return $ \loc ms -> InterfaceDecl (loc, endLoc) InterfaceNormal ms i tps exs ps bod

interfaceBody :: P (InterfaceBody Parsed, Location)
interfaceBody = braces (InterfaceBody . catMaybes <$> list interfaceBodyDecl)

-- Declarations

classBodyStatement :: P (Maybe (Decl Parsed))
classBodyStatement =
  try
    ( do
        _ <- list1 (tokWithEndLoc SemiColon)
        return Nothing
    )
    <|> try
      ( do
          startLoc <- getLocation
          mst <- bopt (tok KW_Static)
          (bl, endLoc) <- block
          return (Just (InitDecl (startLoc, endLoc) mst bl))
      )
    <|> ( do
            loc <- getLocation
            ms <- list modifier
            dec <- memberDecl
            return (Just (MemberDecl (dec loc ms)))
        )

memberDecl :: P (Mod (MemberDecl Parsed))
memberDecl =
  try
    ( do
        cd <- noLoc classDecl
        return (\loc ms -> MemberClassDecl (cd loc ms))
    )
    <|> try
      ( do
          iDecl <- try annInterfaceDecl <|> try interfaceDecl
          return (\loc ms -> MemberInterfaceDecl (iDecl loc ms))
      )
    <|> try fieldDecl
    <|> try methodDecl
    <|> constrDecl

fieldDecl :: P (Mod (MemberDecl Parsed))
fieldDecl = do
  typ <- ttype
  vds <- varDecls
  endLoc <- getEndLoc
  semiColon
  return $ \loc ms -> FieldDecl (loc, endLoc) ms typ vds

methodDecl :: P (Mod (MemberDecl Parsed))
methodDecl = do
  tps <- neoptList typeParams
  rt <- resultType
  i <- noLoc ident
  fps <- formalParams
  thr <- neoptList throws
  (bod, endLoc) <- methodBody
  return $ \loc ms -> MethodDecl (loc, endLoc) ms tps rt i fps thr Nothing bod

methodBody :: P (MethodBody Parsed, Location)
methodBody = onlySemi <|> fullBody
  where
    onlySemi = attrTok SemiColon (MethodBody Nothing)
    fullBody = do
      (b, loc) <- block
      return (MethodBody (Just b), loc)

constrDecl :: P (Mod (MemberDecl Parsed))
constrDecl = do
  tps <- neoptList typeParams
  i <- noLoc ident
  fps <- lopt formalParams -- record constructors omit the argument list
  thr <- neoptList throws
  (bod, endLoc) <- constrBody
  return $ \loc ms -> ConstructorDecl (loc, endLoc) ms tps i fps thr bod

constrBody :: P (ConstructorBody Parsed, Location)
constrBody =
  braces
    ( do
        mec <- opt (try explConstrInv)
        bss <- list (noLoc blockStmt)
        return (ConstructorBody mec bss)
    )

explConstrInv :: P (ExplConstrInv Parsed)
explConstrInv =
  noLoc . endSemi $
    try
      ( do
          tas <- neoptList refTypeArgs
          tok KW_This
          ThisInvoke tas <$> noLoc args
      )
      <|> try
        ( do
            tas <- neoptList refTypeArgs
            tok KW_Super
            SuperInvoke tas <$> noLoc args
        )
      <|> ( do
              pri <- noLoc primary
              period
              tas <- neoptList refTypeArgs
              tok KW_Super
              PrimarySuperInvoke pri tas <$> noLoc args
          )

-- TODO: This should be parsed like class bodies, and post-checked.
--       That would give far better error messages.
interfaceBodyDecl :: P (Maybe (MemberDecl Parsed))
interfaceBodyDecl =
  semiColon
    >> return Nothing
    <|> do
      loc <- getLocation
      ms <- list modifier
      imd <- interfaceMemberDecl
      return $ Just (imd loc ms)

interfaceMemberDecl :: P (Mod (MemberDecl Parsed))
interfaceMemberDecl =
  ( do
      cd <- noLoc classDecl
      return (\loc ms -> MemberClassDecl (cd loc ms))
  )
    <|> ( do
            iDecl <- try annInterfaceDecl <|> try interfaceDecl
            return (\loc ms -> MemberInterfaceDecl (iDecl loc ms))
        )
    <|> try fieldDecl
    <|> absMethodDecl

absMethodDecl :: P (Mod (MemberDecl Parsed))
absMethodDecl = do
  tps <- neoptList typeParams
  rt <- resultType
  i <- noLoc ident
  fps <- formalParams
  thr <- neoptList throws
  def <- opt defaultValue
  endLoc <- getEndLoc
  semiColon
  return $ \loc ms -> MethodDecl (loc, endLoc) ms tps rt i fps thr def (MethodBody Nothing)

defaultValue :: P (Exp Parsed)
defaultValue = tok KW_Default >> noLoc exp

throws :: P (NonEmpty RefType)
throws = tok KW_Throws >> refTypeList

-- Formal parameters

formalParams :: P [FormalParam Parsed]
formalParams = noLoc $ parens $ do
  fps <- seplistNoLoc formalParam comma
  if validateFPs fps
    then return fps
    else fail "Only the last formal parameter may be of variable arity"
  where
    validateFPs :: [FormalParam Parsed] -> Bool
    validateFPs [] = True
    validateFPs [_] = True
    validateFPs (FormalParam _ _ _ b _ : _) = not b

formalParam :: P (FormalParam Parsed)
formalParam = do
  startLoc <- getLocation
  ms <- list modifier
  typ <- ttype
  var <- bopt ellipsis
  (vdi, loc) <- varDeclId
  return (FormalParam (startLoc, loc) ms typ var vdi)

ellipsis :: P ()
ellipsis = period >> period >> period

-- Modifiers

modifier :: P (Modifier Parsed)
modifier =
  Annotation <$> annotation
    <|> ( do
            startLoc <- getLocation
            (constr, endLoc) <-
              attrTok KW_Public Public
                <|> attrTok KW_Private Private
                <|> attrTok KW_Protected Protected
                <|> attrTok KW_Abstract Abstract
                <|> attrTok KW_Final Final
                <|> attrTok KW_Static Static
                <|> attrTok KW_Strictfp StrictFP
                <|> attrTok KW_Transient Transient
                <|> attrTok KW_Volatile Volatile
                <|> attrTok KW_Native Native
                <|> attrTok KW_Synchronized Synchronized_
                <|> attrFixedIdent "sealed" Sealed
            return (constr (startLoc, endLoc))
        )

annotation :: P (Annotation Parsed)
annotation = do
  startLoc <- getLocation
  tok Op_AtSign
  (n, nLoc) <- name
  try
    ( do
        (elist, endLoc) <- parens evlist
        return (NormalAnnotation (startLoc, endLoc) n elist)
    )
    <|> try
      ( do
          (e, endLoc) <- parens elementValue
          return (SingleElementAnnotation (startLoc, endLoc) n e)
      )
    <|> return (MarkerAnnotation (startLoc, nLoc) n)

evlist :: P (NonEmpty (Ident, ElementValue Parsed))
evlist = seplist1NoLoc elementValuePair comma

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
varDecls = seplist1NoLoc varDecl comma

varDecl :: P (VarDecl Parsed)
varDecl = do
  startLoc <- getLocation
  (vid, vidLoc) <- varDeclId
  mviLoc <- opt (tok Op_Equal >> varInit)
  let (mvi, endLoc) = case mviLoc of
        Nothing -> (Nothing, vidLoc)
        Just (vi, loc) -> (Just vi, loc)
  return (VarDecl (startLoc, endLoc) vid mvi)

varDeclId :: P (VarDeclId, Location)
varDeclId = do
  startLoc <- getLocation
  (i, noArrLoc) <- ident
  ebs <- list emptyBrackets
  let endLoc = case ebs of
        [] -> noArrLoc
        _ -> snd (last ebs)
  return (foldl (\f (_, loc) -> VarDeclArray (startLoc, loc) . f) VarId ebs i, endLoc)

localVarDecl :: P (Location, [Modifier Parsed], Type, NonEmpty (VarDecl Parsed))
localVarDecl = do
  startLoc <- getLocation
  ms <- list modifier
  typ <- ttype
  vds <- varDecls
  return (startLoc, ms, typ, vds)

varInit :: P (VarInit Parsed, Location)
varInit =
  mapFst InitArray <$> arrayInit
    <|> mapFst InitExp <$> exp

arrayInit :: P (ArrayInit Parsed, Location)
arrayInit = braces $ do
  startLoc <- getLocation
  (vis, visLoc) <- seplist varInit comma
  mtrailing <- opt (tokWithEndLoc Comma)
  return (ArrayInit (startLoc, maybe visLoc snd mtrailing) vis)

----------------------------------------------------------------------------
-- Statements

block :: P (Block Parsed, Location)
block = do
  startLoc <- getLocation
  state <- getState
  case ps_mode state of
    -- ParseFull -> braces $ Block <$> list (noLoc blockStmt)
    ParseFull -> do
      (bs, loc) <- braces (list (noLoc blockStmt))
      return (Block (startLoc, loc) bs, loc)
    ParseShallow -> shallowP startLoc
  where
    shallowP start = do
      loc <- parseNestedCurly (-1)
      return (Block (start, loc) [], loc)

-- | Parses anything between properly balance curly brackets.
-- level must initially be -1
parseNestedCurly :: Int -> P Location
parseNestedCurly level = do
  loc <- getEndLoc
  newLevel <-
    javaToken $ \case
      OpenCurly -> Just (level + 1)
      _ | level < 0 -> Nothing -- need to start with {
      CloseCurly -> Just (level - 1)
      _ -> Just level
  if newLevel < 0 then return loc else parseNestedCurly newLevel

blockStmt :: P (BlockStmt Parsed, Location)
blockStmt =
  try
    ( do
        loc <- getLocation
        ms <- list modifier
        (cd, endLoc) <- classDecl
        return (LocalClass (cd loc ms), endLoc)
    )
    <|> try
      ( do
          ((startLoc, m, t, vds), endLoc) <- endSemi localVarDecl
          return (LocalVars (startLoc, endLoc) m t vds, endLoc)
      )
    <|> do
      mapFst BlockStmt <$> stmt

stmt :: P (Stmt Parsed, Location)
stmt = ifStmt <|> whileStmt <|> forStmt <|> labeledStmt <|> stmtNoTrail
  where
    ifStmt = do
      startLoc <- getLocation
      tok KW_If
      e <- noLoc $ parens (noLoc exp)
      try
        ( do
            th <- noLoc stmtNSI
            tok KW_Else
            (el, endLoc) <- stmt
            return (IfThenElse (startLoc, endLoc) e th el, endLoc)
        )
        <|> ( do
                (th, endLoc) <- stmt
                return (IfThen (startLoc, endLoc) e th, endLoc)
            )
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
            try
              ( do
                  fi <- opt forInit
                  semiColon
                  e <- opt (noLoc exp)
                  semiColon
                  fu <- forUp
                  return (\loc -> BasicFor (startLoc, loc) fi e fu)
              )
              <|> ( do
                      ms <- list modifier
                      t <- ttype
                      i <- noLoc ident
                      colon
                      e <- noLoc exp
                      return (\loc -> EnhancedFor (startLoc, loc) ms t i e)
                  )
      (s, endLoc) <- stmt
      return (f endLoc s, endLoc)
    labeledStmt = try $ do
      startLoc <- getLocation
      lbl <- noLoc ident
      colon
      (s, endLoc) <- stmt
      return (Labeled (startLoc, endLoc) lbl s, endLoc)

stmtNSI :: P (Stmt Parsed, Location)
stmtNSI = ifStmt <|> whileStmt <|> forStmt <|> labeledStmt <|> stmtNoTrail
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
            try
              ( do
                  fi <- opt forInit
                  semiColon
                  e <- opt (noLoc exp)
                  semiColon
                  fu <- forUp
                  return (\loc -> BasicFor (startLoc, loc) fi e fu)
              )
              <|> ( do
                      ms <- list modifier
                      t <- ttype
                      i <- noLoc ident
                      colon
                      e <- noLoc exp
                      return (\loc -> EnhancedFor (startLoc, loc) ms t i e)
                  )
      (s, endLoc) <- stmtNSI
      return (f endLoc s, endLoc)
    labeledStmt = try $ do
      startLoc <- getLocation
      i <- noLoc ident
      colon
      (s, endLoc) <- stmtNSI
      return (Labeled (startLoc, endLoc) i s, endLoc)

stmtNoTrail :: P (Stmt Parsed, Location)
stmtNoTrail =
  -- empty statement
  ( do
      startLoc <- getLocation
      (_, loc) <- tokWithEndLoc SemiColon
      return (Empty (startLoc, loc), loc)
  )
    <|>
    -- inner block
    mapFst StmtBlock <$> block
    <|>
    -- assertions
    ( do
        startLoc <- getLocation
        tok KW_Assert
        e <- noLoc exp
        me2 <- opt $ colon >> noLoc exp
        (_, loc) <- tokWithEndLoc SemiColon
        return (Assert (startLoc, loc) e me2, loc)
    )
    <|>
    -- switch stmts
    ( do
        startLoc <- getLocation
        tok KW_Switch
        e <- noLoc $ parens (noLoc exp)
        ((style, sb), endLoc) <- switchBlock
        return (Switch (startLoc, endLoc) style e sb, endLoc)
    )
    <|>
    -- do-while loops
    ( do
        startLoc <- getLocation
        tok KW_Do
        s <- noLoc stmt
        tok KW_While
        e <- noLoc $ parens (noLoc exp)
        (_, endLoc) <- tokWithEndLoc SemiColon
        return (Do (startLoc, endLoc) s e, endLoc)
    )
    <|>
    -- break
    ( do
        startLoc <- getLocation
        tok KW_Break
        mi <- opt (noLoc ident)
        (_, endLoc) <- tokWithEndLoc SemiColon
        return (Break (startLoc, endLoc) mi, endLoc)
    )
    <|>
    -- continue
    ( do
        startLoc <- getLocation
        tok KW_Continue
        mi <- opt (noLoc ident)
        (_, loc) <- tokWithEndLoc SemiColon
        return (Continue (startLoc, loc) mi, loc)
    )
    <|>
    -- return
    ( do
        startLoc <- getLocation
        tok KW_Return
        me <- opt (noLoc exp)
        (_, endLoc) <- tokWithEndLoc SemiColon
        return (Return (startLoc, endLoc) me, endLoc)
    )
    <|>
    -- synchronized
    ( do
        startLoc <- getLocation
        tok KW_Synchronized
        e <- noLoc $ parens (noLoc exp)
        (b, endLoc) <- block
        return (Synchronized (startLoc, endLoc) e b, endLoc)
    )
    <|>
    -- throw
    ( do
        startLoc <- getLocation
        tok KW_Throw
        e <- noLoc exp
        (_, loc) <- tokWithEndLoc SemiColon
        return (Throw (startLoc, loc) e, loc)
    )
    <|>
    -- try-catch, both with and without a finally clause
    ( do
        startLoc <- getLocation
        tok KW_Try
        resources <- tryResourceList
        (b, blockEndLoc) <- block
        cs <- list catch
        mfLoc <- opt (tok KW_Finally >> block)
        -- TODO: here we should check that there exists at
        -- least one catch or finally clause
        let (mf, endLoc) = case (mfLoc, cs) of
              (Nothing, []) -> (Nothing, blockEndLoc)
              (Nothing, c) -> (Nothing, snd (last c))
              (Just (fb, l), _) -> (Just fb, l)
        return
          ( Try (startLoc, endLoc) resources b (map fst cs) mf,
            endLoc
          )
    )
    <|>
    -- expressions as stmts
    do
      startLoc <- getLocation
      (s, endLoc) <- endSemi stmtExp
      return (ExpStmt (startLoc, endLoc) s, endLoc)

-- For loops

forInit :: P (ForInit Parsed)
forInit =
  ( do
      try
        ( do
            (_, m, t, vds) <- localVarDecl
            return (ForLocalVars m t vds)
        )
  )
    <|> (seplist1NoLoc stmtExp comma <&> ForInitExps)

forUp :: P [Exp Parsed]
forUp = seplistNoLoc stmtExp comma

-- Switches

switchBlock :: P ((SwitchStyle, [SwitchBlock Parsed]), Location)
switchBlock = braces (try old <|> new)
  where
    old = do
      x <- list switchStmtOld
      return (SwitchOldStyle, x)
    new = do
      x <- list switchStmtNew
      return (SwitchNewStyle, x)

switchStmtOld :: P (SwitchBlock Parsed)
switchStmtOld = do
  startLoc <- getLocation
  (lbl, lblLoc) <- switchLabelOld
  bswithLocs <- list blockStmt
  let bss = map fst bswithLocs
      loc = case bswithLocs of
        [] -> lblLoc
        _ -> snd (last bswithLocs)
  return (SwitchBlock (startLoc, loc) lbl bss)

switchLabelOld :: P (SwitchLabel Parsed, Location)
switchLabelOld =
  (tok KW_Default >> attrTok Op_Colon Default)
    <|> ( do
            tok KW_Case
            es <- noLoc (seplist1 condExp comma)
            attrTok Op_Colon (SwitchCase es)
        )

switchStmtNew :: P (SwitchBlock Parsed)
switchStmtNew = do
  startLoc <- getLocation
  lbl <- switchLabelNew
  (bss, loc) <- braces (list (noLoc blockStmt)) <|> blockStmt >>= \s -> return ([fst s], snd s)
  return (SwitchBlock (startLoc, loc) lbl bss)

switchLabelNew :: P (SwitchLabel Parsed)
switchLabelNew =
  (tok KW_Default >> tok LambdaArrow >> return Default)
    <|> ( do
            tok KW_Case
            es <- noLoc (seplist1 condExp comma)
            tok LambdaArrow
            return $ SwitchCase es
        )

switchExp :: P (Exp Parsed, Location)
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
        (SwitchExpBranchBlock <$> noLoc (braces (list (noLoc blockStmt))))
          <|> (SwitchExpBranchBlock <$> try (blockStmt >>= \s -> return [fst s]))
          <|> (SwitchExpBranchExp <$> branchExp)
      return (SwitchExpBranch lbl body)
    branchExp = do
      e <- noLoc exp
      semiColon
      return e

-- Try-catch clauses

catch :: P (Catch Parsed, Location)
catch = do
  tok KW_Catch
  fp <- noLoc (parens formalParam)
  (b, loc) <- block
  return (Catch fp b, loc)

tryResourceList :: P [TryResource Parsed]
tryResourceList = do
  l <-
    opt
      ( noLoc
          ( parens $ do
              l <- seplistNoLoc tryResource semiColon
              _ <- opt semiColon
              return l
          )
      )
  case l of
    Just xs -> return xs
    Nothing -> return []
  where
    tryResource =
      (TryResourceVarDecl <$> try resourceDecl)
        <|> (TryResourceQualAccess <$> try fieldAccess)
        <|> (TryResourceVarAccess <$> noLoc ident)

resourceDecl :: P (ResourceDecl Parsed)
resourceDecl = do
  ms <- list modifier
  typ <- ttype
  vid <- noLoc varDeclId
  tok Op_Equal
  ResourceDecl ms typ vid <$> noLoc varInit

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

preIncDec :: P (Exp Parsed, Location)
preIncDec = do
  op <- noLoc preIncDecOp
  (e, eLoc) <- unaryExp
  return (op eLoc e, eLoc)

postIncDec :: P (Exp Parsed)
postIncDec = do
  startLoc <- getLocation
  e <- noLoc postfixExpNES
  ops <- noLoc $ list1 postfixOp
  return (foldl (\a s -> s startLoc a) e ops)

assignment :: P (Exp Parsed, Location)
assignment = do
  startLoc <- getLocation
  lh <- lhs
  op <- assignOp
  (e, loc) <- assignExp
  return (Assign (startLoc, loc) lh op e, loc)

lhs :: P (Lhs Parsed)
lhs =
  try (FieldLhs <$> fieldAccess)
    <|> try (ArrayLhs <$> noLoc arrayAccess)
    <|> NameLhs <$> noLoc name

exp :: P (Exp Parsed, Location)
exp = assignExp

assignExp :: P (Exp Parsed, Location)
assignExp = try switchExp <|> try methodRef <|> try lambdaExp <|> try assignment <|> condExp

condExp :: P (Exp Parsed, Location)
condExp = do
  startLoc <- getLocation
  condOrExp |>> condExpSuffix startLoc

condExpSuffix :: Location -> P (Exp Parsed -> Exp Parsed, Location)
condExpSuffix startLoc = do
  tok Op_Query
  th <- noLoc exp
  colon
  (el, endLoc) <- condExp
  return (\ce -> Cond (startLoc, endLoc) ce th el, endLoc)

infixlExpList :: P (Exp Parsed, Location) -> P Op -> P (Exp Parsed, Location)
infixlExpList p sep = do
  startLoc <- getLocation
  p `chainl1` do
    op <- sep
    return (\(e1, _) (e2, l2) -> (BinOp (startLoc, l2) e1 op e2, l2))

condOrExp, condAndExp, orExp, xorExp, andExp, eqExp, relExp, shiftExp, addExp, mulExp :: P (Exp Parsed, Location)
condOrExp = infixlExpList condAndExp (tok Op_OOr $> COr)
condAndExp = infixlExpList orExp (tok Op_AAnd $> CAnd)
orExp = infixlExpList xorExp (tok Op_Or $> Or)
xorExp = infixlExpList andExp (tok Op_Caret $> Xor)
andExp = infixlExpList eqExp (tok Op_And $> And)
eqExp = infixlExpList relExp (tok Op_Equals $> Equal <|> tok Op_BangE $> NotEq)
relExp = do
  startLoc <- getLocation
  (e, endLoc) <- infixlExpList shiftExp (tok Op_LThan $> LThan <|> tok Op_GThan $> GThan <|> tok Op_LThanE $> LThanE <|> tok Op_GThanE $> GThanE)
  fromMaybe (e, endLoc)
    <$> opt
      ( do
          tok KW_Instanceof
          (t, refLoc) <- refType
          mNameLoc <- opt name
          let (mName, loc) =
                case mNameLoc of
                  Just (n, l) -> (Just n, l)
                  Nothing -> (Nothing, refLoc)
          return (InstanceOf (startLoc, loc) e t mName, loc)
      )
shiftExp = infixlExpList addExp (tok Op_LShift $> LShift <|> try (tok Op_GThan >> tok Op_GThan >> tok Op_GThan $> RRShift) <|> try (tok Op_GThan >> tok Op_GThan $> RShift))
addExp = infixlExpList mulExp (tok Op_Plus $> Add <|> tok Op_Minus $> Sub)
mulExp = infixlExpList unaryExp (tok Op_Star $> Mult <|> tok Op_Slash $> Div <|> tok Op_Percent $> Rem)

unaryExp :: P (Exp Parsed, Location)
unaryExp =
  try preIncDec
    <|> try
      ( do
          op <- noLoc prefixOp
          (e, eLoc) <- unaryExp
          return (op eLoc e, eLoc)
      )
    <|> try
      ( do
          startLoc <- getLocation
          t <- noLoc (parens ttype)
          (e, loc) <- unaryExp
          return (Cast (startLoc, loc) t e, loc)
      )
    <|> postfixExp

postfixExpNES :: P (Exp Parsed, Location)
postfixExpNES =
  -- try postIncDec <|>
  try primary
    <|> mapFst ExpName <$> name

postfixExp :: P (Exp Parsed, Location)
postfixExp = do
  startLoc <- getLocation
  postfixExpNES |>> (mapFst (\op -> op startLoc) <$> postfixOp)

primary :: P (Exp Parsed, Location)
primary = primaryNPS |>> primarySuffix

primaryNPS :: P (Exp Parsed, Location)
primaryNPS = try arrayCreation <|> primaryNoNewArrayNPS

-- unused
-- primaryNoNewArray = startSuff primaryNoNewArrayNPS primarySuffix

primaryNoNewArrayNPS :: P (Exp Parsed, Location)
primaryNoNewArrayNPS = do
  startLoc <- getLocation
  mapFst Lit <$> literal
    <|> ( do
            (_, loc) <- tokWithEndLoc KW_This
            return (This (startLoc, loc), loc)
        )
    <|> parens (noLoc exp)
    <|>
    -- TODO: These two following should probably be merged more
    try
      ( do
          rt <- resultType
          _ <- period
          (_, loc) <- tokWithEndLoc KW_Class
          return (ClassLit (startLoc, loc) rt, loc)
      )
    <|> try
      ( do
          n <- noLoc name
          _ <- period
          (_, loc) <- tokWithEndLoc KW_This
          return (ThisClass (startLoc, loc) n, loc)
      )
    <|> try instanceCreationNPS
    <|> try (mapFst MethodInv <$> methodInvocationNPS)
    <|> try (mapFst FieldAccess <$> fieldAccessNPS)
    <|> mapFst ArrayAccess <$> arrayAccessNPS

primarySuffix :: P (Exp Parsed -> Exp Parsed, Location)
primarySuffix =
  try instanceCreationSuffix
    <|> try (mapFst (ArrayAccess .) <$> arrayAccessSuffix)
    <|> try (mapFst (MethodInv .) <$> methodInvocationSuffix)
    <|> mapFst (FieldAccess .) <$> fieldAccessSuffix

instanceCreationNPS :: P (Exp Parsed, Location)
instanceCreationNPS =
  do
    startLoc <- getLocation
    tok KW_New
    tas <- neoptList (noLoc typeArgs)
    tds <- typeDeclSpecifier
    (as, argsLoc) <- args
    mcbLoc <- opt classBody
    let (mcb, loc) =
          case mcbLoc of
            Just (cb, l) -> (Just cb, l)
            Nothing -> (Nothing, argsLoc)
    return (InstanceCreation (startLoc, loc) tas tds as mcb, loc)

typeDeclSpecifier :: P TypeDeclSpecifier
typeDeclSpecifier =
  try
    ( do
        ct <- noLoc classType
        period
        i <- noLoc ident
        tok Op_LThan
        tok Op_GThan
        return $ TypeDeclSpecifierWithDiamond ct i Diamond
    )
    <|> try
      ( do
          i <- noLoc ident
          tok Op_LThan
          tok Op_GThan
          return $ TypeDeclSpecifierUnqualifiedWithDiamond i Diamond
      )
    <|> TypeDeclSpecifier <$> noLoc classType

instanceCreationSuffix :: P (Exp Parsed -> Exp Parsed, Location)
instanceCreationSuffix =
  do
    startLoc <- getLocation
    period >> tok KW_New
    tas <- neoptList (noLoc typeArgs)
    i <- noLoc ident
    (as, argsLoc) <- args
    mcbLoc <- opt classBody
    let (mcb, loc) =
          case mcbLoc of
            Just (cb, l) -> (Just cb, l)
            Nothing -> (Nothing, argsLoc)
    return (\p -> QualInstanceCreation (startLoc, loc) p tas i as mcb, loc)

instanceCreation :: P (Exp Parsed)
instanceCreation =
  try
    ( noLoc instanceCreationNPS
    )
    <|> do
      p <- noLoc primaryNPS
      ss <- list (noLoc primarySuffix)
      let icp = foldl (\a s -> s a) p ss
      case icp of
        QualInstanceCreation {} -> return icp
        _ -> fail ""

lambdaParams :: P (LambdaParams Parsed)
lambdaParams =
  try (LambdaSingleParam <$> noLoc ident)
    <|> try (noLoc (parens (LambdaFormalParams <$> seplistNoLoc formalParam comma)))
    <|> noLoc (parens (LambdaInferredParams <$> noLoc (seplist ident comma)))

lambdaExp :: P (Exp Parsed, Location)
lambdaExp = do
  startLoc <- getLocation
  params <- lambdaParams
  tok LambdaArrow
  (e, loc) <-
    ( do
        (b, loc) <- try block
        return (LambdaBlock b, loc)
      )
      <|> ( do
              (e, loc) <- exp
              return (LambdaExpression e, loc)
          )
  return (Lambda (startLoc, loc) params e, loc)

methodRef :: P (Exp Parsed, Location)
methodRef = do
  startLoc <- getLocation
  n <- noLoc name
  tok MethodRefSep
  (mrt, loc) <- attrTok KW_New MethodRefConstructor <|> mapFst MethodRefIdent <$> ident
  return (MethodRef (startLoc, loc) n mrt, loc)

{-
instanceCreation =
    (do tok KW_New
        tas <- lopt typeArgs
        ct  <- classType
        as  <- args
        mcb <- opt classBody
        return $ InstanceCreation tas ct as mcb) <|>
    (do p   <- primary
        period >> tok KW_New
        tas <- lopt typeArgs
        i   <- ident
        as  <- args
        mcb <- opt classBody
        return $ QualInstanceCreation p tas i as mcb)
-}

fieldAccessNPS :: P (FieldAccess Parsed, Location)
fieldAccessNPS = do
  startLoc <- getLocation
  ( do
      tok KW_Super >> period
      (i, loc) <- ident
      return (SuperFieldAccess (startLoc, loc) i, loc)
    )
    <|> ( do
            n <- noLoc name
            period >> tok KW_Super >> period
            (i, loc) <- ident
            return (ClassFieldAccess (startLoc, loc) n i, loc)
        )

fieldAccessSuffix :: P (Exp Parsed -> FieldAccess Parsed, Location)
fieldAccessSuffix = do
  startLoc <- getLocation
  period
  (i, loc) <- ident
  return (\p -> PrimaryFieldAccess (startLoc, loc) p i, loc)

fieldAccess :: P (FieldAccess Parsed)
fieldAccess =
  try (noLoc fieldAccessNPS)
    <|> do
      p <- noLoc primaryNPS
      ss <- list (noLoc primarySuffix)
      let fap = foldl (\a s -> s a) p ss
      case fap of
        FieldAccess fa -> return fa
        _ -> fail ""

{-
fieldAccess :: P FieldAccess
fieldAccess = try fieldAccessNPS <|> do
    p <- primary
    fs <- fieldAccessSuffix
    return (fs p)
-}

{-
fieldAccess :: P FieldAccess
fieldAccess =
    (do tok KW_Super >> period
        i <- ident
        return $ SuperFieldAccess i) <|>
    (try $ do
        n <- name
        period >> tok KW_Super >> period
        i <- ident
        return $ ClassFieldAccess n i) <|>
    (do p <- primary
        period
        i <- ident
        return $ PrimaryFieldAccess p i)
-}

methodInvocationNPS :: P (MethodInvocation Parsed, Location)
methodInvocationNPS = do
  startLoc <- getLocation
  ( do
      tok KW_Super >> period
      rts <- neoptList refTypeArgs
      i <- noLoc ident
      (as, loc) <- args
      return (SuperMethodCall (startLoc, loc) rts i as, loc)
    )
    <|> ( do
            (mn, i) <- qualifiedMethodName
            (as, loc) <- args
            return (MethodCall (startLoc, loc) mn i as, loc)
        )
    <|> ( do
            n <- noLoc name
            period
            msp <- opt (tok KW_Super >> period)
            rts <- neoptList refTypeArgs
            i <- noLoc ident
            (as, loc) <- args
            let constr = maybe TypeMethodCall (const ClassMethodCall) msp
            return (constr (startLoc, loc) n rts i as, loc)
        )

qualifiedMethodName :: P (Maybe Name, Ident)
qualifiedMethodName = do
  startLoc <- getLocation
  mapFst
    ( \case
        ([], _) -> Nothing
        (is, endLoc) -> Just (Name (startLoc, endLoc) (NonEmpty.fromList (reverse is)))
    )
    <$> go startLoc []
  where
    go :: Location -> [Ident] -> P (([Ident], Location), Ident)
    go endLoc nids = do
      (i, loc) <- ident
      try
        ( do
            period
            go loc (i : nids)
        )
        <|> return ((nids, endLoc), i)

methodInvocationSuffix :: P (Exp Parsed -> MethodInvocation Parsed, Location)
methodInvocationSuffix = do
  startLoc <- getLocation
  period
  _ <- neoptList refTypeArgs
  i <- noLoc ident
  (as, loc) <- args
  return (\p -> PrimaryMethodCall (startLoc, loc) p [] i as, loc)

methodInvocationExp :: P (Exp Parsed)
methodInvocationExp =
  try
    ( do
        p <- noLoc primaryNPS
        ss <- list (noLoc primarySuffix)
        let mip = foldl (\a s -> s a) p ss
        case mip of
          MethodInv _ -> return mip
          _ -> fail ""
    )
    <|> (MethodInv <$> noLoc methodInvocationNPS)

{-
methodInvocation :: P MethodInvocation
methodInvocation =
    (do tok KW_Super >> period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ SuperMethodCall rts i as) <|>
    (do p <- primary
        period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ PrimaryMethodCall p rts i as) <|>
    (do n <- name
        f <- (do as <- args
                 return $ \n -> MethodCall n as) <|>
             (period >> do
                msp <- opt (tok KW_Super >> period)
                rts <- lopt refTypeArgs
                i   <- ident
                as  <- args
                let mc = maybe TypeMethodCall (const ClassMethodCall) msp
                return $ \n -> mc n rts i as)
        return $ f n)
-}

args :: P ([Argument Parsed], Location)
args = parens (noLoc (seplist exp comma))

-- Arrays

arrayAccessNPS :: P (ArrayIndex Parsed, Location)
arrayAccessNPS = do
  startLoc <- getLocation
  n <- noLoc name
  (e, loc) <- list1 (brackets (noLoc exp))
  return (ArrayIndex (startLoc, loc) (ExpName n) e, loc)

arrayAccessSuffix :: P (Exp Parsed -> ArrayIndex Parsed, Location)
arrayAccessSuffix = do
  startLoc <- getLocation
  (e, loc) <- list1 (brackets (noLoc exp))
  return (\ref -> ArrayIndex (startLoc, loc) ref e, loc)

arrayAccess :: P (ArrayIndex Parsed, Location)
arrayAccess =
  try arrayAccessNPS <|> do
    aap <- primaryNoNewArrayNPS |>> primarySuffix
    case aap of
      (ArrayAccess ain, loc) -> return (ain, loc)
      _ -> fail ""

{-
arrayAccess :: P (Exp, Exp)
arrayAccess = do
    ref <- arrayRef
    e   <- brackets exp
    return (ref, e)

arrayRef :: P Exp
arrayRef = ExpName <$> name <|> primaryNoNewArray
-}

arrayCreation :: P (Exp Parsed, Location)
arrayCreation = do
  startLoc <- getLocation
  tok KW_New
  t <- nonArrayType
  try
    ( do
        ds <- noLoc $ list1 emptyBrackets
        (ai, loc) <- arrayInit
        return (ArrayCreateInit (startLoc, loc) t (length ds) ai, loc)
    )
    <|> ( do
            (des, desLoc) <- list1 $ try $ brackets (noLoc exp)
            ds <- list emptyBrackets
            let len = length ds
                loc =
                  if len < 1
                    then desLoc
                    else snd (last ds)
            return (ArrayCreate (startLoc, loc) t des len, loc)
        )

literal :: P (Literal, Location)
literal = do
  startLoc <- getLocation
  endLoc <- getEndLoc
  lit <-
    javaToken $ \case
      IntTok i -> Just (Int (startLoc, endLoc) i)
      LongTok l -> Just (Word (startLoc, endLoc) l)
      DoubleTok d -> Just (Double (startLoc, endLoc) d)
      FloatTok f -> Just (Float (startLoc, endLoc) f)
      CharTok c -> Just (Char (startLoc, endLoc) c)
      StringTok s -> Just (String (startLoc, endLoc) s)
      BoolTok b -> Just (Boolean (startLoc, endLoc) b)
      NullTok -> Just (Null (startLoc, endLoc))
      _ -> Nothing
  return (lit, endLoc)

-- Operators

preIncDecOp, prefixOp, postfixOp :: P (Location -> Exp Parsed -> Exp Parsed, Location)
preIncDecOp = do
  startLoc <- getLocation
  (constr, endLoc) <- attrTok Op_PPlus PreIncrement <|> attrTok Op_MMinus PreDecrement
  return (\expEndLoc -> constr (startLoc, expEndLoc), endLoc)
prefixOp = do
  startLoc <- getLocation
  (constr, endLoc) <- attrTok Op_Bang PreNot <|> attrTok Op_Tilde PreBitCompl <|> attrTok Op_Plus PrePlus <|> attrTok Op_Minus PreMinus
  return (\expEndLoc -> constr (startLoc, expEndLoc), endLoc)
postfixOp = do
  (constr, endLoc) <- attrTok Op_PPlus PostIncrement <|> attrTok Op_MMinus PostDecrement
  return (\expStartLoc -> constr (expStartLoc, endLoc), endLoc)

assignOp :: P AssignOp
assignOp =
  (tok Op_Equal >> return EqualA)
    <|> (tok Op_StarE >> return MultA)
    <|> (tok Op_SlashE >> return DivA)
    <|> (tok Op_PercentE >> return RemA)
    <|> (tok Op_PlusE >> return AddA)
    <|> (tok Op_MinusE >> return SubA)
    <|> (tok Op_LShiftE >> return LShiftA)
    <|> (tok Op_RShiftE >> return RShiftA)
    <|> (tok Op_RRShiftE >> return RRShiftA)
    <|> (tok Op_AndE >> return AndA)
    <|> (tok Op_CaretE >> return XorA)
    <|> (tok Op_OrE >> return OrA)

----------------------------------------------------------------------------
-- Types

ttype :: P Type
ttype = try (RefType <$> noLoc refType) <|> PrimType <$> primType

primType :: P PrimType
primType = do
  startLoc <- getLocation
  (constr, endLoc) <-
    attrTok KW_Boolean BooleanT
      <|> attrTok KW_Byte ByteT
      <|> attrTok KW_Short ShortT
      <|> attrTok KW_Int IntT
      <|> attrTok KW_Long LongT
      <|> attrTok KW_Char CharT
      <|> attrTok KW_Float FloatT
      <|> attrTok KW_Double DoubleT
  return (constr (startLoc, endLoc))

refType :: P (RefType, Location)
refType = do
  startLoc <- getLocation
  ( do
      pt <- primType
      (_, loc) :| bs <- list1NoLoc emptyBrackets
      return
        ( mapFst
            (\a -> a pt)
            ( foldl
                (f startLoc)
                (ArrayType (startLoc, loc) . PrimType, loc)
                bs
            )
        )
    )
    <|> ( do
            (ct, ctLoc) <- classType
            bs <- list emptyBrackets
            return
              ( mapFst
                  (\a -> a ct)
                  ( foldl
                      (f startLoc)
                      (ClassRefType, ctLoc)
                      bs
                  )
              )
        )
    <?> "refType"
  where
    f s (c, _) (_, loc) = (ArrayType (s, loc) . RefType . c, loc)

nonArrayType :: P Type
nonArrayType =
  PrimType <$> primType
    <|> RefType . ClassRefType <$> noLoc classType

classType :: P (ClassType, Location)
classType = do
  startLoc <- getLocation
  (cts, loc) <- seplist1 classTypeSpec period
  return (ClassType (startLoc, loc) cts, loc)

classTypeSpec :: P ((Ident, [TypeArgument]), Location)
classTypeSpec = do
  (i, iLoc) <- ident
  mtas <- opt typeArgs
  let (tas, loc) = maybe ([], iLoc) (mapFst NonEmpty.toList) mtas
  return ((i, tas), loc)

resultType :: P (Maybe Type)
resultType = tok KW_Void >> return Nothing <|> Just <$> ttype <?> "resultType"

refTypeList :: P (NonEmpty RefType)
refTypeList = noLoc (seplist1 refType comma)

----------------------------------------------------------------------------
-- Type parameters and arguments

typeParams :: P (NonEmpty TypeParam)
typeParams = noLoc $ angles (seplist1NoLoc typeParam comma)

typeParam :: P TypeParam
typeParam = do
  i <- noLoc ident
  bs <- neoptList bounds
  return $ TypeParam i bs

bounds :: P (NonEmpty RefType)
bounds = tok KW_Extends >> noLoc (seplist1 refType (tok Op_And))

typeArgs :: P (NonEmpty TypeArgument, Location)
typeArgs = angles (seplist1NoLoc typeArg comma)

typeArg :: P TypeArgument
typeArg =
  tok Op_Query
    >> Wildcard
    <$> opt wildcardBound
    <|> ActualType <$> noLoc refType

wildcardBound :: P WildcardBound
wildcardBound =
  tok KW_Extends
    >> ExtendsBound
    <$> noLoc refType
    <|> tok KW_Super
      >> SuperBound
      <$> noLoc refType

refTypeArgs :: P (NonEmpty RefType)
refTypeArgs = noLoc $ angles refTypeList

----------------------------------------------------------------------------
-- Names

name :: P (Name, Location)
name = do
  startLoc <- getLocation
  (idents, endLoc) <- seplist1 ident period
  return (Name (startLoc, endLoc) idents, endLoc)

ident :: P (Ident, Location)
ident = do
  startLoc <- getLocation
  loc <- getEndLoc
  i <- javaToken $ \case
    IdentTok s -> Just (Ident (startLoc, loc) s)
    _ -> Nothing
  return (i, loc)

fixedIdent :: String -> a -> P a
fixedIdent fixed result =
  javaToken $ \case
    IdentTok s | s == fixed -> Just result
    _ -> Nothing

attrFixedIdent :: String -> a -> P (a, Location)
attrFixedIdent fixed result = do
  loc <- getEndLoc
  a <- fixedIdent fixed result
  return (a, loc)

------------------------------------------------------------

empty :: P ()
empty = return ()

opt :: P a -> P (Maybe a)
opt = optionMaybe

bopt :: P a -> P Bool
bopt p = opt p >>= \ma -> return $ isJust ma

neoptList :: P (NonEmpty a) -> P [a]
neoptList p = do
  mas <- opt p
  case mas of
    Nothing -> return []
    Just as -> return (NonEmpty.toList as)

lopt :: P [a] -> P [a]
lopt p = do
  mas <- opt p
  case mas of
    Nothing -> return []
    Just as -> return as

list :: P a -> P [a]
list = option [] . many1

list1 :: P (a, Location) -> P (NonEmpty a, Location)
list1 p = do
  ll <- many1 p
  let l = NonEmpty.fromList (map fst ll)
      loc = snd (last ll)
  return (l, loc)

list1NoLoc :: P a -> P (NonEmpty a)
list1NoLoc p =
  NonEmpty.fromList <$> many1 p

seplist :: P (a, Location) -> P sep -> P ([a], Location)
-- seplist = sepBy
seplist p sep = do
  startLoc <- getLocation
  option ([], startLoc) (mapFst NonEmpty.toList <$> seplist1 p sep)

seplistNoLoc :: P a -> P sep -> P [a]
-- seplist = sepBy
seplistNoLoc p sep = option [] $ NonEmpty.toList <$> seplist1NoLoc p sep

seplist1 :: P (a, Location) -> P sep -> P (NonEmpty a, Location)
-- seplist1 = sepBy1
seplist1 p sep =
  p >>= \(a, loc) ->
    try
      ( do
          _ <- sep
          (as, l) <- seplist1 p sep
          return (a <| as, l)
      )
      <|> return (a :| [], loc)

seplist1NoLoc :: P a -> P sep -> P (NonEmpty a)
-- seplist1 = sepBy1
seplist1NoLoc p sep =
  p >>= \a ->
    try
      ( do
          _ <- sep
          as <- seplist1NoLoc p sep
          return (a <| as)
      )
      <|> return (a :| [])

(|>>) :: P (a, Location) -> P (a -> a, Location) -> P (a, Location)
start |>> suffix = do
  x <- start
  ss <- list suffix
  return (foldl (\(a, _) (s, loc) -> (s a, loc)) x ss)

------------------------------------------------------------

javaToken :: (Token -> Maybe a) -> P a
javaToken test = token showT posT testT
  where
    showT (L1 _ _ t) = show t
    posT (L1 p _ _) = pos2sourcePos p
    testT (L1 _ _ t) = test t

tok, matchToken :: Token -> P ()
tok = matchToken
matchToken t = javaToken (\r -> if r == t then Just () else Nothing)

attrTok :: Token -> b -> P (b, Location)
attrTok t constr =
  fmap (\(_, endLoc) -> (constr, endLoc)) (tokWithEndLoc t)

tokWithEndLoc :: Token -> P ((), Location)
tokWithEndLoc t = do
  endLoc <- getEndLoc
  x <- tok t
  return (x, endLoc)

pos2sourcePos :: (Int, Int) -> SourcePos
pos2sourcePos (l, c) = newPos "" l c

type Mod a = Location -> [Modifier Parsed] -> a

angles, braces, brackets, parens :: P a -> P (a, Location)
angles = betweenWithEndLoc Op_LThan Op_GThan
braces = betweenWithEndLoc OpenCurly CloseCurly
brackets = betweenWithEndLoc OpenSquare CloseSquare
parens = betweenWithEndLoc OpenParen CloseParen

betweenWithEndLoc :: Token -> Token -> P a -> P (a, Location)
betweenWithEndLoc open close p = do
  _ <- tok open
  x <- p
  (_, endLoc) <- tokWithEndLoc close
  return (x, endLoc)

emptyBrackets :: P ((), Location)
emptyBrackets = brackets (pure ())

endSemi :: P a -> P (a, Location)
endSemi p = do
  a <- p
  endLoc <- getEndLoc
  semiColon
  return (a, endLoc)

comma, colon, semiColon, period :: P ()
comma = tok Comma
colon = tok Op_Colon
semiColon = tok SemiColon
period = tok Period
