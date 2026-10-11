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
    classDeclaration,
    normalInterfaceDeclaration,
    classMemberDeclaration,
    fieldDeclaration,
    methodDeclaration,
    constructorDeclaration,
    interfaceMemberDeclaration,
    interfaceMethodDeclaration,
    formalParams,
    formalParam,
    modifier,
    variableDeclarators,
    variableDeclarator,
    block,
    blockStatement,
    statement,
    stmtExp,
    expression,
    primary,
    literal,
    ttype,
    primitiveType,
    referenceType,
    classType,
    resultType,
    lambdaExp,
    methodReference,
    typeParameters,
    typeParameter,
    name,
    identifier,
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
    lookAhead,
    many,
    notFollowedBy,
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

-- A trick to allow >> and >>=, normally infixl 1, to be
-- used inside branches of <|>, which is declared as infixr 1.
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
  try
    ( PackageDecl
        <$> many (Annotation <$> annotation)
        <* tok KW_Package
    )
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
  Just <$> classOrInterfaceDeclaration
    <|> Nothing <$ semiColon

----------------------------------------------------------------------------
-- Declarations

-- Class declarations

classOrInterfaceDeclaration :: P (TypeDecl Parsed)
classOrInterfaceDeclaration = do
  startPos <- getPosition
  ms <- many modifier
  do
    cd <- noPos classDeclaration
    return (ClassTypeDecl (cd startPos ms))
    <|> do
      iDecl <- annotationTypeDeclaration <|> normalInterfaceDeclaration
      return (InterfaceTypeDecl (iDecl startPos ms))

classDeclaration :: P (Mod (ClassDecl Parsed), SourcePos)
classDeclaration =
  normalClassDeclaration
    <|> recordDeclaration
    <|> enumDeclaration

normalClassDeclaration :: P (Mod (ClassDecl Parsed), SourcePos)
normalClassDeclaration = do
  tok KW_Class
  i <- noPos identifier
  tps <- typeParameters
  mex <- classExtends
  imp <- classImplements
  (bod, endPos) <- classBody
  return (\pos ms -> ClassDecl (pos, endPos) ms i tps mex imp bod, endPos)

classExtends :: P (Maybe RefType)
classExtends = optionMaybe (tok KW_Extends >> noPos referenceType)

interfaceExtends :: P [RefType]
interfaceExtends = NonEmpty.optional (tok KW_Extends >> referenceTypeList)

-- Since Java 17
classPermits :: P [RefType]
classPermits = NonEmpty.optional (fixedIdent "permits" >> referenceTypeList)

classImplements :: P [RefType]
classImplements = NonEmpty.optional (tok KW_Implements >> referenceTypeList)

enumDeclaration :: P (Mod (ClassDecl Parsed), SourcePos)
enumDeclaration = do
  tok KW_Enum
  i <- noPos identifier
  imp <- classImplements
  (bod, endPos) <- enumBody
  return (\pos ms -> EnumDecl (pos, endPos) ms i imp bod, endPos)

-- Since Java 16
recordDeclaration :: P (Mod (ClassDecl Parsed), SourcePos)
recordDeclaration = do
  tok KW_Record
  i <- noPos identifier
  tps <- typeParameters
  fields <- noPos $ parens $ sepBy recordField comma
  imp <- classImplements
  (bod, endPos) <- classBody
  return (\pos ms -> RecordDecl (pos, endPos) ms i tps fields imp bod, endPos)
  where
    recordField = RecordFieldDecl <$> ttype <*> noPos identifier

classBody :: P (ClassBody Parsed, SourcePos)
classBody = Bifunctor.first ClassBody <$> braces classBodyDeclarations

enumBody :: P (EnumBody Parsed, SourcePos)
enumBody =
  braces $
    EnumBody
      <$> sepEndBy enumConstant comma
      <*> option [] enumBodyDeclarations

enumConstant :: P (EnumConstant Parsed)
enumConstant =
  EnumConstant
    <$> many (Annotation <$> annotation)
    <*> noPos identifier
    <*> option [] (noPos args)
    <*> optionMaybe (noPos classBody)

enumBodyDeclarations :: P [Decl Parsed]
enumBodyDeclarations = semiColon >> classBodyDeclarations

classBodyDeclarations :: P [Decl Parsed]
classBodyDeclarations = manyMaybes classBodyDeclaration

-- Interface declarations

annotationTypeDeclaration :: P (Mod (InterfaceDecl Parsed))
annotationTypeDeclaration = do
  try (tok Op_AtSign >> tok KW_Interface)
  i <- noPos identifier
  tps <- typeParameters
  exs <- interfaceExtends
  ps <- classPermits
  (bod, endPos) <- interfaceBody
  return \pos ms -> InterfaceDecl (pos, endPos) InterfaceAnnotation ms i tps exs ps bod

normalInterfaceDeclaration :: P (Mod (InterfaceDecl Parsed))
normalInterfaceDeclaration = do
  tok KW_Interface
  i <- noPos identifier
  tps <- typeParameters
  exs <- interfaceExtends
  ps <- classPermits
  (bod, endPos) <- interfaceBody
  return \pos ms -> InterfaceDecl (pos, endPos) InterfaceNormal ms i tps exs ps bod

interfaceBody :: P (InterfaceBody Parsed, SourcePos)
interfaceBody = braces (InterfaceBody <$> manyMaybes interfaceBodyDeclaration)

-- Declarations

classBodyDeclaration :: P (Maybe (Decl Parsed))
classBodyDeclaration =
  skipMany1 semiColon $> Nothing
    <|> do
      startPos <- getPosition
      mst <- try (optionBool (tok KW_Static) <* lookAhead (tok OpenCurly))
      (bl, endPos) <- block
      return (Just (InitDecl (startPos, endPos) mst bl))
    <|> do
      pos <- getPosition
      ms <- many modifier
      dec <- classMemberDeclaration
      return (Just (MemberDecl (dec pos ms)))

classMemberDeclaration :: P (Mod (MemberDecl Parsed))
classMemberDeclaration =
  do
    cd <- noPos classDeclaration
    return \pos ms -> MemberClassDecl (cd pos ms)
    <|> do
      iDecl <- annotationTypeDeclaration <|> normalInterfaceDeclaration
      return \pos ms -> MemberInterfaceDecl (iDecl pos ms)
    <|> try fieldDeclaration
    <|> try methodDeclaration
    <|> constructorDeclaration

fieldDeclaration :: P (Mod (MemberDecl Parsed))
fieldDeclaration = do
  typ <- ttype
  vds <- variableDeclarators
  endPos <- semiColon
  return \pos ms -> FieldDecl (pos, endPos) ms typ vds

methodDeclaration :: P (Mod (MemberDecl Parsed))
methodDeclaration = do
  tps <- typeParameters
  rt <- resultType
  i <- noPos identifier
  fps <- formalParams
  thr <- throws
  (bod, endPos) <- methodBody
  return \pos ms -> MethodDecl (pos, endPos) ms tps rt i fps thr Nothing bod

methodBody :: P (MethodBody Parsed, SourcePos)
methodBody = onlySemi <|> fullBody
  where
    onlySemi = (MethodBody Nothing,) <$> semiColon
    fullBody = Bifunctor.first (MethodBody . Just) <$> block

constructorDeclaration :: P (Mod (MemberDecl Parsed))
constructorDeclaration = do
  tps <- typeParameters
  i <- noPos identifier
  fps <- option [] formalParams -- record constructors omit the argument list
  thr <- throws
  (bod, endPos) <- constructorBody
  return \pos ms -> ConstructorDecl (pos, endPos) ms tps i fps thr bod

constructorBody :: P (ConstructorBody Parsed, SourcePos)
constructorBody =
  braces $
    ConstructorBody
      <$> optionMaybe explicitConstructorInvocation
      <*> many (noPos blockStatement)

explicitConstructorInvocation :: P (ExplConstrInv Parsed)
explicitConstructorInvocation =
  noPos . endSemi $
    try
      ( ThisInvoke
          <$> NonEmpty.optional referenceTypeArguments
          <* tok KW_This
          <* lookAhead (tok OpenParen)
      )
      <*> noPos args
      <|> try
        ( SuperInvoke
            <$> NonEmpty.optional referenceTypeArguments
            <* tok KW_Super
            <* lookAhead (tok OpenParen)
        )
        <*> noPos args
      <|> try
        ( PrimarySuperInvoke
            <$> noPos primary
            <* period
            <*> NonEmpty.optional referenceTypeArguments
            <* tok KW_Super
            <* lookAhead (tok OpenParen)
        )
        <*> noPos args

-- TODO: This should be parsed like class bodies, and post-checked.
--       That would give far better error messages.
interfaceBodyDeclaration :: P (Maybe (MemberDecl Parsed))
interfaceBodyDeclaration =
  semiColon $> Nothing
    <|> do
      pos <- getPosition
      ms <- many modifier
      imd <- interfaceMemberDeclaration
      return $ Just (imd pos ms)

interfaceMemberDeclaration :: P (Mod (MemberDecl Parsed))
interfaceMemberDeclaration =
  do
    cd <- noPos classDeclaration
    return \pos ms -> MemberClassDecl (cd pos ms)
    <|> do
      iDecl <- annotationTypeDeclaration <|> normalInterfaceDeclaration
      return \pos ms -> MemberInterfaceDecl (iDecl pos ms)
    <|> try fieldDeclaration
    <|> interfaceMethodDeclaration

interfaceMethodDeclaration :: P (Mod (MemberDecl Parsed))
interfaceMethodDeclaration = do
  tps <- typeParameters
  rt <- resultType
  i <- noPos identifier
  fps <- formalParams
  thr <- throws
  def <- optionMaybe defaultValue
  endPos <- semiColon
  return \pos ms -> MethodDecl (pos, endPos) ms tps rt i fps thr def (MethodBody Nothing)

defaultValue :: P (Exp Parsed)
defaultValue = tok KW_Default >> noPos expression

throws :: P [RefType]
throws = NonEmpty.optional (tok KW_Throws >> referenceTypeList)

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
  (vdi, pos) <- variableDeclaratorId
  return (FormalParam (startPos, pos) ms typ var vdi)

catchFormalParam :: P (CatchFormalParam Parsed)
catchFormalParam = do
  startPos <- getPosition
  typs <- NonEmpty.sepBy ((,) <$> many modifier <*> ttype) (tok Op_Or)
  (vdi, pos) <- variableDeclaratorId
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
  try (tok Op_AtSign >> notFollowedBy (tok KW_Interface))
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
elementValuePair = (,) <$> noPos identifier <* tok Op_Equal <*> elementValue

elementValue :: P (ElementValue Parsed)
elementValue =
  EVVal . InitArray <$> noPos arrayInitializer
    <|> EVVal . InitExp <$> noPos condExp
    <|> EVAnn <$> annotation

----------------------------------------------------------------------------
-- Variable declarations

variableDeclarators :: P (NonEmpty (VarDecl Parsed))
variableDeclarators = NonEmpty.sepBy variableDeclarator comma

variableDeclarator :: P (VarDecl Parsed)
variableDeclarator = do
  startPos <- getPosition
  (vid, vidPos) <- variableDeclaratorId
  (mvi, endPos) <- optionMaybePos vidPos (tok Op_Equal >> variableInitializer)
  return (VarDecl (startPos, endPos) vid mvi)

variableDeclaratorId :: P (VarDeclId, SourcePos)
variableDeclaratorId = do
  startPos <- getPosition
  (i, noArrPos) <- identifier
  ebs <- many emptyBrackets
  let endPos = NonEmpty.last (noArrPos :| ebs)
  return (foldl' (\f pos -> VarDeclArray (startPos, pos) . f) VarId ebs i, endPos)

localVariableDeclaration :: P (SourcePos, [Modifier Parsed], Type, NonEmpty (VarDecl Parsed))
localVariableDeclaration =
  (,,,)
    <$> getPosition
    <*> many modifier
    <*> ttype
    <*> variableDeclarators

variableInitializer :: P (VarInit Parsed, SourcePos)
variableInitializer =
  Bifunctor.first InitArray <$> arrayInitializer
    <|> Bifunctor.first InitExp <$> expression

arrayInitializer :: P (ArrayInit Parsed, SourcePos)
arrayInitializer = braces do
  startPos <- getPosition
  (vis, visPos) <- sepEndByPos variableInitializer comma
  return (ArrayInit (startPos, visPos) vis)

----------------------------------------------------------------------------
-- Statements

block :: P (Block Parsed, SourcePos)
block = do
  startPos <- getPosition
  (bs, pos) <- braces (NonEmpty.optional (noPos blockStatements))
  return (Block (startPos, pos) bs, pos)

blockStatements :: P (NonEmpty (BlockStmt Parsed), SourcePos)
blockStatements = manyNonEmptyPos blockStatement

blockStatement :: P (BlockStmt Parsed, SourcePos)
blockStatement =
  try do
    pos <- getPosition
    ms <- many modifier
    (cd, endPos) <- classDeclaration
    return (LocalClass (cd pos ms), endPos)
    <|> try do
      ((startPos, m, t, vds), endPos) <- endSemi localVariableDeclaration
      return (LocalVars (startPos, endPos) m t vds, endPos)
    <|> Bifunctor.first BlockStmt <$> statement

statement :: P (Stmt Parsed, SourcePos)
statement =
  ifStatement
    <|> whileStatement
    <|> forStatement
    <|> labeledStatement
    <|> statementWithoutTrailingSubstatement
  where
    ifStatement = do
      startPos <- getPosition
      tok KW_If
      e <- noPos $ parens (noPos expression)
      try do
        th <- noPos statementNoShortIf
        tok KW_Else
        (el, endPos) <- statement
        return (IfThenElse (startPos, endPos) e th el, endPos)
        <|> do
          (th, endPos) <- statement
          return (IfThen (startPos, endPos) e th, endPos)
    whileStatement = do
      startPos <- getPosition
      tok KW_While
      e <- noPos $ parens (noPos expression)
      (s, endPos) <- statement
      return (While (startPos, endPos) e s, endPos)
    forStatement = do
      startPos <- getPosition
      tok KW_For
      f <-
        noPos $
          parens $
            try do
              fi <- optionMaybe forInit
              void semiColon
              e <- optionMaybe (noPos expression)
              void semiColon
              fu <- forUp
              return \pos -> BasicFor (startPos, pos) fi e fu
              <|> do
                ms <- many modifier
                t <- ttype
                i <- noPos identifier
                void colon
                e <- noPos expression
                return \pos -> EnhancedFor (startPos, pos) ms t i e
      (s, endPos) <- statement
      return (f endPos s, endPos)
    labeledStatement = do
      startPos <- getPosition
      lbl <- try (noPos identifier <* colon)
      (s, endPos) <- statement
      return (Labeled (startPos, endPos) lbl s, endPos)

statementNoShortIf :: P (Stmt Parsed, SourcePos)
statementNoShortIf =
  ifThenElseStatementNoShortIf
    <|> whileStatementNoShortIf
    <|> forStatementNoShortIf
    <|> labeledStatementNoShortIf
    <|> statementWithoutTrailingSubstatement
  where
    ifThenElseStatementNoShortIf = do
      startPos <- getPosition
      tok KW_If
      e <- noPos $ parens (noPos expression)
      th <- noPos statementNoShortIf
      tok KW_Else
      (el, endPos) <- statementNoShortIf
      return (IfThenElse (startPos, endPos) e th el, endPos)
    whileStatementNoShortIf = do
      startPos <- getPosition
      tok KW_While
      e <- noPos $ parens (noPos expression)
      (s, endPos) <- statementNoShortIf
      return (While (startPos, endPos) e s, endPos)
    forStatementNoShortIf = do
      startPos <- getPosition
      tok KW_For
      f <-
        noPos $
          parens $
            try do
              fi <- optionMaybe forInit
              void semiColon
              e <- optionMaybe (noPos expression)
              void semiColon
              fu <- forUp
              return \pos -> BasicFor (startPos, pos) fi e fu
              <|> do
                ms <- many modifier
                t <- ttype
                i <- noPos identifier
                void colon
                e <- noPos expression
                return \pos -> EnhancedFor (startPos, pos) ms t i e
      (s, endPos) <- statementNoShortIf
      return (f endPos s, endPos)
    labeledStatementNoShortIf = do
      startPos <- getPosition
      i <- try (noPos identifier <* colon)
      (s, endPos) <- statementNoShortIf
      return (Labeled (startPos, endPos) i s, endPos)

statementWithoutTrailingSubstatement :: P (Stmt Parsed, SourcePos)
statementWithoutTrailingSubstatement =
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
        e <- noPos expression
        me2 <- optionMaybe $ colon >> noPos expression
        pos <- semiColon
        return (Assert (startPos, pos) e me2, pos)
      <|>
      -- switch stmts
      do
        tok KW_Switch
        e <- noPos $ parens (noPos expression)
        ((style, sb), endPos) <- switchBlock
        return (Switch (startPos, endPos) style e sb, endPos)
      <|>
      -- do-while loops
      do
        tok KW_Do
        s <- noPos statement
        tok KW_While
        e <- noPos $ parens (noPos expression)
        endPos <- semiColon
        return (Do (startPos, endPos) s e, endPos)
      <|>
      -- break
      do
        tok KW_Break
        mi <- optionMaybe (noPos identifier)
        endPos <- semiColon
        return (Break (startPos, endPos) mi, endPos)
      <|>
      -- continue
      do
        tok KW_Continue
        mi <- optionMaybe (noPos identifier)
        pos <- semiColon
        return (Continue (startPos, pos) mi, pos)
      <|>
      -- return
      do
        tok KW_Return
        me <- optionMaybe (noPos expression)
        endPos <- semiColon
        return (Return (startPos, endPos) me, endPos)
      <|>
      -- synchronized
      do
        tok KW_Synchronized
        e <- noPos $ parens (noPos expression)
        (b, endPos) <- block
        return (Synchronized (startPos, endPos) e b, endPos)
      <|>
      -- throw
      do
        tok KW_Throw
        e <- noPos expression
        pos <- semiColon
        return (Throw (startPos, pos) e, pos)
      <|>
      -- try-catch, both with and without a finally clause
      do
        tok KW_Try
        resources <- tryResourceList
        (b, blockEndPos) <- block
        (cs, cEndPos) <- manyPos blockEndPos catch
        (mf, endPos) <- optionMaybePos cEndPos (tok KW_Finally >> block)
        when (null resources && null cs && null mf) $
          fail "try needs at least one catch or finally clause"
        return
          ( Try (startPos, endPos) resources b cs mf,
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
    (_, m, t, vds) <- localVariableDeclaration
    return (ForLocalVars m t vds)
    <|> ForInitExps <$> NonEmpty.sepBy stmtExp comma

forUp :: P [Exp Parsed]
forUp = sepBy stmtExp comma

-- Switches

switchBlock :: P ((SwitchStyle, [SwitchBlock Parsed]), SourcePos)
switchBlock = braces (try old <|> new)
  where
    old = (SwitchOldStyle,) <$> many switchBlockStatement
    new = (SwitchNewStyle,) <$> many switchRule

switchBlockStatement :: P (SwitchBlock Parsed)
switchBlockStatement = do
  startPos <- getPosition
  lbl <- switchLabel
  lblPos <- colon
  (bss, pos) <- manyPos lblPos blockStatement
  return (SwitchBlock (startPos, pos) lbl bss)

-- Since Java 14
switchRule :: P (SwitchBlock Parsed)
switchRule = do
  startPos <- getPosition
  lbl <- switchLabel
  tok LambdaArrow
  (bss, pos) <- braces (many (noPos blockStatement)) <|> Bifunctor.first pure <$> blockStatement
  return (SwitchBlock (startPos, pos) lbl bss)

switchLabel :: P (SwitchLabel Parsed)
switchLabel =
  SwitchCase <$ tok KW_Case <*> NonEmpty.sepBy (noPos condExp) comma
    <|> Default <$ tok KW_Default

-- Since Java 14
switchExpression :: P (Exp Parsed, SourcePos)
switchExpression = do
  startPos <- getPosition
  tok KW_Switch
  e <- noPos $ parens (noPos expression)
  (branches, pos) <- braces switchExpBody
  return (SwitchExp (startPos, pos) e branches, pos)
  where
    switchExpBody = NonEmpty.many do
      lbl <- switchLabel
      tok LambdaArrow
      body <-
        SwitchExpBranchBlock . fst <$> braces (many (noPos blockStatement))
          <|> SwitchExpBranchBlock . pure . fst <$> try blockStatement
          <|> SwitchExpBranchExp <$> branchExp
      return (SwitchExpBranch lbl body)
    branchExp = noPos expression <* semiColon

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
        <|> TryResourceVarAccess <$> noPos identifier

resourceDecl :: P (ResourceDecl Parsed)
resourceDecl =
  ResourceDecl
    <$> many modifier
    <*> ttype
    <*> noPos variableDeclaratorId
    <* tok Op_Equal
    <*> noPos variableInitializer

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
    <|> try (noPos methodReference)
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

expression :: P (Exp Parsed, SourcePos)
expression = assignExp

assignExp :: P (Exp Parsed, SourcePos)
assignExp =
  try switchExpression
    <|> try methodReference
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
  th <- noPos expression
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
    (t, refPos) <- referenceType
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
    <|> parens (noPos expression)
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
  tas <- NonEmpty.optional referenceTypeArguments
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
        <*> noPos identifier
        <* tok Op_LThan
        <* tok Op_GThan
    )
    <|> try
      ( TypeDeclSpecifierUnqualifiedWithDiamond
          <$> noPos identifier
          <* tok Op_LThan
          <* tok Op_GThan
      )
    <|> TypeDeclSpecifier <$> noPos classType

instanceCreationSuffix :: P (Exp Parsed -> Exp Parsed, SourcePos)
instanceCreationSuffix = do
  startPos <- getPosition
  period >> tok KW_New
  tas <- NonEmpty.optional referenceTypeArguments
  i <- noPos identifier
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
  LambdaSingleParam <$> noPos identifier
    <|> noPos
      ( parens $
          LambdaFormalParams <$> try (sepBy formalParam comma)
            <|> LambdaInferredParams <$> sepBy (noPos identifier) comma
      )

lambdaExp :: P (Exp Parsed, SourcePos)
lambdaExp = do
  startPos <- getPosition
  params <- lambdaParams
  tok LambdaArrow
  (e, pos) <-
    Bifunctor.first LambdaBlock <$> block
      <|> Bifunctor.first LambdaExpression <$> expression
  return (Lambda (startPos, pos) params e, pos)

-- TODO: Parse the remaining alternatives of MethodReference.
-- (JLS 15.13. Method Reference Expressions)
-- MethodReference
--   = ExpressionName, "::", [ TypeArguments ], Identifier
--   | ReferenceType, "::", [ TypeArguments ], Identifier
--   | Primary, "::", [ TypeArguments ], Identifier
--   | "super", "::", [ TypeArguments ], Identifier
--   | TypeName, ".", "super", "::", [ TypeArguments ], Identifier
--   | ClassType, "::", [ TypeArguments ], "new"
--   | ArrayType, "::", "new"
methodReference :: P (Exp Parsed, SourcePos)
methodReference = do
  startPos <- getPosition
  n <- noPos name
  tok MethodRefSep
  (mrt, pos) <-
    attrTokWithEndPos KW_New MethodRefConstructor
      <|> Bifunctor.first MethodRefIdent <$> identifier
  return (MethodRef (startPos, pos) n mrt, pos)

fieldAccessNPS :: P (FieldAccess Parsed, SourcePos)
fieldAccessNPS = do
  startPos <- getPosition
  do
    tok KW_Super >> void period
    (i, pos) <- identifier
    return (SuperFieldAccess (startPos, pos) i, pos)
    <|> do
      n <- noPos name
      period >> tok KW_Super >> void period
      (i, pos) <- identifier
      return (ClassFieldAccess (startPos, pos) n i, pos)

fieldAccessSuffix :: P (Exp Parsed -> FieldAccess Parsed, SourcePos)
fieldAccessSuffix = do
  startPos <- getPosition
  void period
  (i, pos) <- identifier
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
    rts <- NonEmpty.optional referenceTypeArguments
    i <- noPos identifier
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
      rts <- NonEmpty.optional referenceTypeArguments
      i <- noPos identifier
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
      (i, pos) <- identifier
      ((i : nids, pos),,)
        <$> try (period >> referenceTypeArguments <&> NonEmpty.toList)
        <*> noPos identifier
        <|> try (period >> go pos (i : nids))
        <|> return ((nids, endPos), [], i)

methodInvocationSuffix :: P (Exp Parsed -> MethodInvocation Parsed, SourcePos)
methodInvocationSuffix = do
  startPos <- getPosition
  void period
  rts <- NonEmpty.optional referenceTypeArguments
  i <- noPos identifier
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
args = parens (sepBy (noPos expression) comma)

-- Arrays

arrayAccessNPS :: P (ArrayIndex Parsed, SourcePos)
arrayAccessNPS = do
  startPos <- getPosition
  n <- noPos name
  (e, pos) <- manyNonEmptyPos (brackets (noPos expression))
  return (ArrayIndex (startPos, pos) (ExpName n) e, pos)

arrayAccessSuffix :: P (Exp Parsed -> ArrayIndex Parsed, SourcePos)
arrayAccessSuffix = do
  startPos <- getPosition
  (e, pos) <- manyNonEmptyPos (brackets (noPos expression))
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
    (ai, pos) <- arrayInitializer
    return (ArrayCreateInit (startPos, pos) t (length ds) ai, pos)
    <|> do
      (des, desPos) <- manyNonEmptyPos $ try $ brackets (noPos expression)
      ds <- many emptyBrackets
      let len = length ds
          pos = NonEmpty.last (desPos :| ds)
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
  RefType <$> try (noPos referenceType)
    <|> PrimType <$> primitiveType

primitiveType :: P PrimType
primitiveType = do
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

referenceType :: P (RefType, SourcePos)
referenceType = do
  startPos <- getPosition
  do
    pt <- primitiveType
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
  PrimType <$> primitiveType
    <|> RefType . ClassRefType <$> noPos classType

classType :: P (ClassType, SourcePos)
classType = do
  startPos <- getPosition
  (cts, pos) <- trySepByNonEmptyPos classTypeSpec period
  return (ClassType (startPos, pos) cts, pos)

classTypeSpec :: P ((Ident, [TypeArgument]), SourcePos)
classTypeSpec = do
  (i, iPos) <- identifier
  (tas, pos) <- option ([], iPos) (Bifunctor.first NonEmpty.toList <$> typeArguments)
  return ((i, tas), pos)

resultType :: P (Maybe Type)
resultType =
  attrTok KW_Void Nothing
    <|> Just <$> ttype
    <?> "resultType"

referenceTypeList :: P (NonEmpty RefType)
referenceTypeList = NonEmpty.sepBy (noPos referenceType) comma

----------------------------------------------------------------------------
-- Type parameters and arguments

typeParameters :: P [TypeParam Parsed]
typeParameters = NonEmpty.optional $ noPos $ angles $ NonEmpty.sepBy typeParameter comma

typeParameter :: P (TypeParam Parsed)
typeParameter =
  TypeParam
    <$> many (Annotation <$> annotation)
    <*> noPos identifier
    <*> NonEmpty.optional bounds

bounds :: P (NonEmpty RefType)
bounds = tok KW_Extends >> NonEmpty.sepBy (noPos referenceType) (tok Op_And)

typeArguments :: P (NonEmpty TypeArgument, SourcePos)
typeArguments = angles $ NonEmpty.sepBy typeArgument comma

typeArgument :: P TypeArgument
typeArgument =
  Wildcard <$ tok Op_Query <*> optionMaybe wildcardBound
    <|> ActualType <$> noPos referenceType

wildcardBound :: P WildcardBound
wildcardBound =
  ExtendsBound <$ tok KW_Extends <*> noPos referenceType
    <|> SuperBound <$ tok KW_Super <*> noPos referenceType

-- 8.8.7.1. Explicit Constructor Invocations:
--     If TypeArguments is present to the left of this or super, then it is a
--     compile-time error if any of the type arguments are wildcards (§4.5.1).
-- 15.12. Method Invocation Expressions:
--     If TypeArguments is present to the left of Identifier, then it is a
--     compile-time error if any of the type arguments are wildcards (§4.5.1).
referenceTypeArguments :: P (NonEmpty RefType)
referenceTypeArguments = noPos $ angles referenceTypeList

----------------------------------------------------------------------------
-- Names

name :: P (Name, SourcePos)
name = do
  startPos <- getPosition
  (idents, endPos) <- trySepByNonEmptyPos identifier period
  return (Name (startPos, endPos) idents, endPos)

identifier :: P (Ident, SourcePos)
identifier = do
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

manyPos :: SourcePos -> P (a, SourcePos) -> P ([a], SourcePos)
manyPos pos p = option ([], pos) (Bifunctor.first NonEmpty.toList <$> manyNonEmptyPos p)

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
