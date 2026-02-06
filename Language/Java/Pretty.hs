{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Java.Pretty where

import Data.Char (toLower)
import Data.List (intersperse)
import Language.Java.Syntax
import Text.PrettyPrint
import Text.Printf (printf)

#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
import qualified Data.List.NonEmpty as NonEmpty
#endif

prettyPrint :: (Pretty a) => a -> String
prettyPrint = show . pretty

parenPrec :: Int -> Int -> Doc -> Doc
parenPrec inheritedPrec currentPrec t
  | inheritedPrec <= 0 = t
  | inheritedPrec < currentPrec = parens t
  | otherwise = t

class Pretty a where
  pretty :: a -> Doc
  pretty = prettyPrec 0

  prettyPrec :: Int -> a -> Doc
  prettyPrec _ = pretty

class (Pretty (XNameClassification x)) => PrettyExtension x

instance PrettyExtension Parsed

instance PrettyExtension Analyzed

-----------------------------------------------------------------------
-- Packages

instance (PrettyExtension p) => Pretty (CompilationUnit p) where
  prettyPrec p (CompilationUnit mpd ids tds) =
    vcat $ (maybePP p mpd : map (prettyPrec p) ids) ++ map (prettyPrec p) tds

instance Pretty PackageDecl where
  prettyPrec p (PackageDecl name) = text "package" <+> prettyPrec p name <> semi

instance Pretty ImportDecl where
  prettyPrec p (ImportDecl _ st name wc) =
    text "import"
      <+> opt st (text "static")
      <+> prettyPrec p name
      <> opt wc (text ".*")
      <> semi

-----------------------------------------------------------------------
-- Declarations

instance (PrettyExtension p) => Pretty (TypeDecl p) where
  prettyPrec p (ClassTypeDecl cd) = prettyPrec p cd
  prettyPrec p (InterfaceTypeDecl itd) = prettyPrec p itd

instance (PrettyExtension p) => Pretty (ClassDecl p) where
  prettyPrec p (EnumDecl _loc mods ident impls body) =
    hsep
      [ hsep (map (prettyPrec p) mods),
        text "enum",
        prettyPrec p ident,
        ppImplements p impls
      ]
      $$ prettyPrec p body
  prettyPrec p (ClassDecl _loc mods ident tParams mSuper impls body) =
    hsep
      [ hsep (map (prettyPrec p) mods),
        text "class",
        prettyPrec p ident,
        ppTypeParams p tParams,
        ppExtends p (maybe [] return mSuper),
        ppImplements p impls
      ]
      $$ prettyPrec p body
  prettyPrec p (RecordDecl _ mods ident tParams recordFields impls body) =
    hsep
      [ hsep (map (prettyPrec p) mods),
        text "record",
        prettyPrec p ident,
        ppTypeParams p tParams,
        ppArgs p recordFields,
        ppImplements p impls
      ]
      $$ prettyPrec p body

instance (PrettyExtension p) => Pretty (ClassBody p) where
  prettyPrec p (ClassBody ds) =
    braceBlock (map (prettyPrec p) ds)

instance (PrettyExtension p) => Pretty (EnumBody p) where
  prettyPrec p (EnumBody cs ds) =
    braceBlock $
      punctuate comma (map (prettyPrec p) cs)
        ++ opt (not $ null ds) semi
        : map (prettyPrec p) ds

instance (PrettyExtension p) => Pretty (EnumConstant p) where
  prettyPrec p (EnumConstant ident args mBody) =
    prettyPrec p ident
      -- needs special treatment since even the parens are optional
      <> opt (not $ null args) (ppArgs p args)
      $$ maybePP p mBody

instance (PrettyExtension p) => Pretty (InterfaceDecl p) where
  prettyPrec p (InterfaceDecl _ kind mods ident tParams impls permits body) =
    hsep
      [ hsep (map (prettyPrec p) mods),
        text
          ( case kind of
              InterfaceNormal -> "interface"
              InterfaceAnnotation -> "@interface"
          ),
        prettyPrec p ident,
        ppTypeParams p tParams,
        ppExtends p impls,
        ppPermits p permits
      ]
      $$ prettyPrec p body

instance (PrettyExtension p) => Pretty (InterfaceBody p) where
  prettyPrec p (InterfaceBody mds) =
    braceBlock (map (prettyPrec p) mds)

instance (PrettyExtension p) => Pretty (Decl p) where
  prettyPrec p (MemberDecl md) = prettyPrec p md
  prettyPrec p (InitDecl _ b bl) =
    opt b (text "static") <+> prettyPrec p bl

instance (PrettyExtension p) => Pretty (MemberDecl p) where
  prettyPrec p (FieldDecl _loc mods t vds) =
    hsep (map (prettyPrec p) mods ++ prettyPrec p t : punctuate (text ",") (map (prettyPrec p) (NonEmpty.toList vds))) <> semi
  prettyPrec p (MethodDecl _loc mods tParams mt ident fParams throws def body) =
    hsep
      [ hsep (map (prettyPrec p) mods),
        ppTypeParams p tParams,
        ppResultType p mt,
        prettyPrec p ident,
        ppArgs p fParams,
        ppThrows p throws,
        ppDefault p def
      ]
      $$ prettyPrec p body
  prettyPrec p (ConstructorDecl _loc mods tParams ident fParams throws body) =
    hsep
      [ hsep (map (prettyPrec p) mods),
        ppTypeParams p tParams,
        prettyPrec p ident,
        ppArgs p fParams,
        ppThrows p throws
      ]
      $$ prettyPrec p body
  prettyPrec p (MemberClassDecl cd) = prettyPrec p cd
  prettyPrec p (MemberInterfaceDecl mid) = prettyPrec p mid

instance Pretty RecordFieldDecl where
  prettyPrec p (RecordFieldDecl t ident) =
    prettyPrec p t <+> prettyPrec p ident

instance (PrettyExtension p) => Pretty (VarDecl p) where
  prettyPrec p (VarDecl _ vdId Nothing) = prettyPrec p vdId
  prettyPrec p (VarDecl _ vdId (Just ie)) =
    (prettyPrec p vdId <+> char '=') <+> prettyPrec p ie

instance Pretty VarDeclId where
  prettyPrec p (VarId ident) = prettyPrec p ident
  prettyPrec p (VarDeclArray _ vId) = prettyPrec p vId <> text "[]"

instance (PrettyExtension p) => Pretty (VarInit p) where
  prettyPrec p (InitExp e) = prettyPrec p e
  prettyPrec p (InitArray (ArrayInit _ ai)) =
    text "{" <+> hsep (punctuate comma (map (prettyPrec p) ai)) <+> text "}"

instance (PrettyExtension p) => Pretty (FormalParam p) where
  prettyPrec p (FormalParam _ mods t b vId) =
    hsep
      [ hsep (map (prettyPrec p) mods),
        prettyPrec p t <> opt b (text "..."),
        prettyPrec p vId
      ]

instance (PrettyExtension p) => Pretty (CatchFormalParam p) where
  prettyPrec p (CatchFormalParam _ ts vId) =
    hsep
      [ hsep (intersperse (text "|") $ map (\(m, t) -> hsep (map (prettyPrec p) m) <+> prettyPrec p t) (NonEmpty.toList ts)),
        prettyPrec p vId
      ]

instance (PrettyExtension p) => Pretty (MethodBody p) where
  prettyPrec p (MethodBody mBlock) = maybe semi (prettyPrec p) mBlock

instance (PrettyExtension p) => Pretty (ConstructorBody p) where
  prettyPrec p (ConstructorBody mECI stmts) =
    braceBlock $ maybePP p mECI : map (prettyPrec p) stmts

instance (PrettyExtension p) => Pretty (ExplConstrInv p) where
  prettyPrec p (ThisInvoke rts args) =
    ppTypeParams p rts <+> text "this" <> ppArgs p args <> semi
  prettyPrec p (SuperInvoke rts args) =
    ppTypeParams p rts <+> text "super" <> ppArgs p args <> semi
  prettyPrec p (PrimarySuperInvoke e rts args) =
    prettyPrec p e
      <> char '.'
      <> ppTypeParams p rts
      <+> text "super"
      <> ppArgs p args
      <> semi

instance (PrettyExtension p) => Pretty (Modifier p) where
  prettyPrec p (Annotation ann) = prettyPrec p ann $+$ nest (-1) (text "")
  prettyPrec _ (Public _) = text "public"
  prettyPrec _ (Private _) = text "private"
  prettyPrec _ (Protected _) = text "protected"
  prettyPrec _ (Abstract _) = text "abstract"
  prettyPrec _ (Final _) = text "final"
  prettyPrec _ (Static _) = text "static"
  prettyPrec _ (StrictFP _) = text "strictfp"
  prettyPrec _ (Transient _) = text "transient"
  prettyPrec _ (Volatile _) = text "volatile"
  prettyPrec _ (Native _) = text "native"
  prettyPrec _ (Synchronized_ _) = text "synchronized"
  prettyPrec _ (Sealed _) = text "sealed"

instance (PrettyExtension p) => Pretty (Annotation p) where
  prettyPrec p x =
    text "@" <> prettyPrec p (annName x) <> case x of
      MarkerAnnotation {} -> text ""
      SingleElementAnnotation {} -> text "(" <> prettyPrec p (annValue x) <> text ")"
      NormalAnnotation {} -> text "(" <> ppEVList p (NonEmpty.toList (annKV x)) <> text ")"

ppEVList p = hsep . punctuate comma . map (\(k, v) -> prettyPrec p k <+> text "=" <+> prettyPrec p v)

instance (PrettyExtension p) => Pretty (ElementValue p) where
  prettyPrec p (EVVal vi) = prettyPrec p vi
  prettyPrec p (EVAnn ann) = prettyPrec p ann

-----------------------------------------------------------------------
-- Statements

instance (PrettyExtension p) => Pretty (Block p) where
  prettyPrec p (Block _ stmts) = braceBlock $ map (prettyPrec p) stmts

instance (PrettyExtension p) => Pretty (BlockStmt p) where
  prettyPrec p (BlockStmt stmt) = prettyPrec p stmt
  prettyPrec p (LocalClass cd) = prettyPrec p cd
  prettyPrec p (LocalVars _ mods t vds) =
    hsep (map (prettyPrec p) mods)
      <+> prettyPrec p t
      <+> hsep (punctuate comma $ map (prettyPrec p) (NonEmpty.toList vds))
      <> semi

instance (PrettyExtension p) => Pretty (Stmt p) where
  prettyPrec p (StmtBlock block) = prettyPrec p block
  prettyPrec _ (IfThen _ c th) =
    text "if" <+> parens (prettyPrec 0 c) $+$ prettyNestedStmt 0 th
  prettyPrec p (IfThenElse _ c th el) =
    text "if" <+> parens (prettyPrec p c) $+$ prettyNestedStmt 0 th $+$ text "else" $+$ prettyNestedStmt 0 el
  prettyPrec p (While _ c stmt) =
    text "while" <+> parens (prettyPrec p c) $+$ prettyNestedStmt 0 stmt
  prettyPrec p (BasicFor _ mInit mE up stmt) =
    text "for"
      <+> parens
        ( hsep
            [ maybePP p mInit,
              semi,
              maybePP p mE,
              semi,
              (hsep . punctuate comma . map (prettyPrec p)) up
            ]
        )
      $+$ prettyNestedStmt p stmt
  prettyPrec p (EnhancedFor _ mods t ident e stmt) =
    hsep
      [ text "for",
        parens $
          hsep
            [ hsep (map (prettyPrec p) mods),
              prettyPrec p t,
              prettyPrec p ident,
              colon,
              prettyPrec p e
            ],
        prettyPrec p stmt
      ]
  prettyPrec _ (Empty _) = semi
  prettyPrec p (ExpStmt _ e) = prettyPrec p e <> semi
  prettyPrec p (Assert _ ass mE) =
    text "assert"
      <+> prettyPrec p ass
      <+> maybe empty ((colon <>) . prettyPrec p) mE
      <> semi
  prettyPrec p (Switch _ _style e sBlocks) =
    text "switch"
      <+> parens (prettyPrec p e)
      $$ braceBlock (map (prettyPrec p) sBlocks)
  prettyPrec p (Do _ stmt e) =
    text "do" $+$ prettyPrec p stmt <+> text "while" <+> parens (prettyPrec p e) <> semi
  prettyPrec p (Break _ mIdent) =
    text "break" <+> maybePP p mIdent <> semi
  prettyPrec p (Continue _ mIdent) =
    text "continue" <+> maybePP p mIdent <> semi
  prettyPrec p (Return _ mE) =
    text "return" <+> maybePP p mE <> semi
  prettyPrec p (Synchronized _ e block) =
    text "synchronized" <+> parens (prettyPrec p e) $$ prettyPrec p block
  prettyPrec p (Throw _ e) =
    text "throw" <+> prettyPrec p e <> semi
  prettyPrec p (Try _ _resources block catches mFinally) =
    -- FIXME: do not ignore resources
    text "try"
      $$ prettyPrec p block
      $$ vcat (map (prettyPrec p) catches ++ [ppFinally mFinally])
    where
      ppFinally Nothing = empty
      ppFinally (Just bl) = text "finally" <+> prettyPrec p bl
  prettyPrec p (Labeled _ ident stmt) =
    prettyPrec p ident <> colon <+> prettyPrec p stmt

instance (PrettyExtension p) => Pretty (Catch p) where
  prettyPrec p (Catch fParam block) =
    hsep [text "catch", parens (prettyPrec p fParam)] $$ prettyPrec p block

instance (PrettyExtension p) => Pretty (SwitchBlock p) where
  prettyPrec p (SwitchBlock _ lbl stmts) =
    vcat (prettyPrec p lbl <> colon : map (nest 2 . prettyPrec p) stmts)

instance (PrettyExtension p) => Pretty (SwitchLabel p) where
  prettyPrec _p (SwitchCase e) =
    text "case" <+> hsep (intersperse comma $ map (prettyPrec 0) (NonEmpty.toList e))
  prettyPrec _p Default = text "default"

instance (PrettyExtension p) => Pretty (SwitchExpBranch p) where
  prettyPrec :: (PrettyExtension p) => Int -> SwitchExpBranch p -> Doc
  prettyPrec p (SwitchExpBranch lbl (SwitchExpBranchExp e)) =
    prettyPrec p lbl <+> text "->" <+> prettyPrec p e
  prettyPrec p (SwitchExpBranch lbl (SwitchExpBranchBlock blockStmts)) =
    prettyPrec p lbl <+> text "->" <+> braceBlock (map (prettyPrec p) blockStmts)

instance (PrettyExtension p) => Pretty (ForInit p) where
  prettyPrec p (ForLocalVars mods t vds) =
    hsep $
      map (prettyPrec p) mods
        ++ prettyPrec p t
        : punctuate comma (map (prettyPrec p) (NonEmpty.toList vds))
  prettyPrec p (ForInitExps es) =
    hsep $ punctuate comma (map (prettyPrec p) (NonEmpty.toList es))

-----------------------------------------------------------------------
-- Expressions

instance (PrettyExtension p) => Pretty (Exp p) where
  prettyPrec p (Lit l) = prettyPrec p l
  prettyPrec p (ClassLit _ mT) =
    ppResultType p mT <> text ".class"
  prettyPrec _ (This _) = text "this"
  prettyPrec p (ThisClass _ name) =
    prettyPrec p name <> text ".this"
  prettyPrec p (InstanceCreation _ tArgs tds args mBody) =
    hsep
      [ text "new",
        ppTypeParams p tArgs,
        prettyPrec p tds <> ppArgs p args
      ]
      $$ maybePP p mBody
  prettyPrec p (QualInstanceCreation _ e tArgs ident args mBody) =
    hsep
      [ prettyPrec p e <> char '.' <> text "new",
        ppTypeParams p tArgs,
        prettyPrec p ident <> ppArgs p args
      ]
      $$ maybePP p mBody
  prettyPrec p (ArrayCreate _ t es k) =
    text "new"
      <+> hcat
        ( prettyPrec p t
            : map (brackets . prettyPrec p) (NonEmpty.toList es)
            ++ replicate k (text "[]")
        )
  prettyPrec p (ArrayCreateInit _ t k i) =
    text "new"
      <+> hcat (prettyPrec p t : replicate k (text "[]"))
      <+> prettyPrec p i
  prettyPrec p (FieldAccess fa) = parenPrec p 1 $ prettyPrec 1 fa
  prettyPrec p (MethodInv mi) = parenPrec p 1 $ prettyPrec 1 mi
  prettyPrec p (ArrayAccess ain) = parenPrec p 1 $ prettyPrec 1 ain
  prettyPrec p (ExpName name) = prettyPrec p name
  prettyPrec p (PostIncrement _ e) = parenPrec p 1 $ prettyPrec 2 e <> text "++"
  prettyPrec p (PostDecrement _ e) = parenPrec p 1 $ prettyPrec 2 e <> text "--"
  prettyPrec p (PreIncrement _ e) = parenPrec p 1 $ text "++" <> prettyPrec 2 e
  prettyPrec p (PreDecrement _ e) = parenPrec p 1 $ text "--" <> prettyPrec 2 e
  prettyPrec p (PrePlus _ e) = parenPrec p 2 $ char '+' <> prettyPrec 2 e
  prettyPrec p (PreMinus _ e) = parenPrec p 2 $ char '-' <> prettyPrec 2 e
  prettyPrec p (PreBitCompl _ e) = parenPrec p 2 $ char '~' <> prettyPrec 2 e
  prettyPrec p (PreNot _ e) = parenPrec p 2 $ char '!' <> prettyPrec 2 e
  prettyPrec p (Cast _ t e) = parenPrec p 2 $ parens (prettyPrec p t) <+> prettyPrec 2 e
  prettyPrec p (BinOp _ e1 op e2) =
    let prec = opPrec op
     in parenPrec p prec (prettyPrec prec e1 <+> prettyPrec p op <+> prettyPrec prec e2)
  prettyPrec p (InstanceOf _ e rt mName) =
    let cp = opPrec LThan
        prettyName =
          case mName of
            Nothing -> empty
            Just n -> char ' ' <> pretty n
     in parenPrec p cp $
          prettyPrec cp e
            <+> text "instanceof"
            <+> prettyPrec cp rt
            <> prettyName
  prettyPrec p (Cond _ c th el) =
    parenPrec p 13 $
      prettyPrec 13 c
        <+> char '?'
        <+> prettyPrec p th
        <+> colon
        <+> prettyPrec 13 el
  prettyPrec p (Assign _ lhs aop e) =
    hsep [prettyPrec p lhs, prettyPrec p aop, prettyPrec p e]
  prettyPrec p (Lambda _ params body) =
    prettyPrec p params <+> text "->" <+> prettyPrec p body
  prettyPrec p (MethodRef _ i1 (MethodRefIdent i2)) =
    prettyPrec p i1 <+> text "::" <+> prettyPrec p i2
  prettyPrec p (MethodRef _ i1 MethodRefConstructor) =
    prettyPrec p i1 <+> text "::new"
  prettyPrec p (SwitchExp _ e branches) =
    hsep
      [ text "switch",
        parens (prettyPrec p e),
        braceBlock (map (prettyPrec p) (NonEmpty.toList branches))
      ]

instance (PrettyExtension p) => Pretty (LambdaParams p) where
  prettyPrec p (LambdaSingleParam ident) = prettyPrec p ident
  prettyPrec p (LambdaFormalParams params) = ppArgs p params
  prettyPrec p (LambdaInferredParams idents) = ppArgs p idents

instance (PrettyExtension p) => Pretty (LambdaExpression p) where
  prettyPrec p (LambdaExpression expr) = prettyPrec p expr
  prettyPrec p (LambdaBlock block) = prettyPrec p block

instance Pretty Literal where
  prettyPrec _ (Int _ i) = text (show i)
  prettyPrec _ (Word _ i) = text (show i) <> char 'L'
  prettyPrec _ (Float _ f) = text (show f) <> char 'F'
  prettyPrec _ (Double _ d) = text (show d)
  prettyPrec _ (Boolean _ b) = text . map toLower $ show b
  prettyPrec _ (Char _ c) = quotes $ text (escapeChar c)
  prettyPrec _ (String _ s) = doubleQuotes $ text (concatMap escapeString s)
  prettyPrec _ (Null _) = text "null"

instance Pretty Op where
  prettyPrec _ op = text $ case op of
    Mult -> "*"
    Div -> "/"
    Rem -> "%"
    Add -> "+"
    Sub -> "-"
    LShift -> "<<"
    RShift -> ">>"
    RRShift -> ">>>"
    LThan -> "<"
    GThan -> ">"
    LThanE -> "<="
    GThanE -> ">="
    Equal -> "=="
    NotEq -> "!="
    And -> "&"
    Xor -> "^"
    Or -> "|"
    CAnd -> "&&"
    COr -> "||"

instance Pretty AssignOp where
  prettyPrec _ aop = text $ case aop of
    EqualA -> "="
    MultA -> "*="
    DivA -> "/="
    RemA -> "%="
    AddA -> "+="
    SubA -> "-="
    LShiftA -> "<<="
    RShiftA -> ">>="
    RRShiftA -> ">>>="
    AndA -> "&="
    XorA -> "^="
    OrA -> "|="

instance (PrettyExtension p) => Pretty (Lhs p) where
  prettyPrec p (NameLhs name) = prettyPrec p name
  prettyPrec p (FieldLhs fa) = prettyPrec p fa
  prettyPrec p (ArrayLhs ain) = prettyPrec p ain

instance (PrettyExtension p) => Pretty (ArrayIndex p) where
  prettyPrec p (ArrayIndex _ ref e) = prettyPrec p ref <> hcat (map (brackets . prettyPrec p) (NonEmpty.toList e))

instance (PrettyExtension p) => Pretty (FieldAccess p) where
  prettyPrec p (PrimaryFieldAccess _ e ident) =
    prettyPrec p e <> char '.' <> prettyPrec p ident
  prettyPrec p (SuperFieldAccess _ ident) =
    text "super." <> prettyPrec p ident
  prettyPrec p (ClassFieldAccess _ name ident) =
    prettyPrec p name <> text "." <> prettyPrec p ident

instance (PrettyExtension p) => Pretty (MethodInvocation p) where
  prettyPrec p (MethodCall _ mName methodId args) =
    case mName of
      Nothing -> prettyPrec p methodId <> ppArgs p args
      Just name ->
        hcat
          [ prettyPrec p name,
            char '.',
            prettyPrec p methodId,
            ppArgs p args
          ]
  prettyPrec p (PrimaryMethodCall _ e tArgs ident args) =
    hcat
      [ prettyPrec p e,
        char '.',
        ppTypeParams p tArgs,
        prettyPrec p ident,
        ppArgs p args
      ]
  prettyPrec p (SuperMethodCall _ tArgs ident args) =
    hcat
      [ text "super.",
        ppTypeParams p tArgs,
        prettyPrec p ident,
        ppArgs p args
      ]
  prettyPrec p (ClassMethodCall _ name tArgs ident args) =
    hcat
      [ prettyPrec p name,
        text ".super.",
        ppTypeParams p tArgs,
        prettyPrec p ident,
        ppArgs p args
      ]
  prettyPrec p (TypeMethodCall _ name tArgs ident args) =
    hcat
      [ prettyPrec p name,
        char '.',
        ppTypeParams p tArgs,
        prettyPrec p ident,
        ppArgs p args
      ]

instance (PrettyExtension p) => Pretty (ArrayInit p) where
  prettyPrec p (ArrayInit _ vInits) =
    braceBlock $ map (\v -> prettyPrec p v <> comma) vInits

-- braces $ hsep (punctuate comma (map (prettyPrec p) vInits))

ppArgs :: (Pretty a) => Int -> [a] -> Doc
ppArgs p = parens . hsep . punctuate comma . map (prettyPrec p)

-----------------------------------------------------------------------
-- Types

instance Pretty Type where
  prettyPrec p (PrimType pt) = prettyPrec p pt
  prettyPrec p (RefType rt) = prettyPrec p rt

instance Pretty RefType where
  prettyPrec p (ClassRefType ct) = prettyPrec p ct
  prettyPrec p (ArrayType _ t) = prettyPrec p t <> text "[]"

instance Pretty ClassType where
  prettyPrec p (ClassType _ itas) =
    hcat . punctuate (char '.') $
      map (\(i, tas) -> prettyPrec p i <> ppTypeParams p tas) (NonEmpty.toList itas)

instance Pretty TypeArgument where
  prettyPrec p (ActualType rt) = prettyPrec p rt
  prettyPrec p (Wildcard mBound) = char '?' <+> maybePP p mBound

instance Pretty TypeDeclSpecifier where
  prettyPrec p (TypeDeclSpecifier ct) = prettyPrec p ct
  prettyPrec p (TypeDeclSpecifierWithDiamond ct i d) = prettyPrec p ct <> char '.' <> prettyPrec p i <> prettyPrec p d
  prettyPrec p (TypeDeclSpecifierUnqualifiedWithDiamond i d) = prettyPrec p i <> prettyPrec p d

instance Pretty Diamond where
  prettyPrec _ Diamond = text "<>"

instance Pretty WildcardBound where
  prettyPrec p (ExtendsBound rt) = text "extends" <+> prettyPrec p rt
  prettyPrec p (SuperBound rt) = text "super" <+> prettyPrec p rt

instance Pretty PrimType where
  prettyPrec _ (BooleanT _) = text "boolean"
  prettyPrec _ (ByteT _) = text "byte"
  prettyPrec _ (ShortT _) = text "short"
  prettyPrec _ (IntT _) = text "int"
  prettyPrec _ (LongT _) = text "long"
  prettyPrec _ (CharT _) = text "char"
  prettyPrec _ (FloatT _) = text "float"
  prettyPrec _ (DoubleT _) = text "double"

instance Pretty TypeParam where
  prettyPrec p (TypeParam ident rts) =
    prettyPrec p ident
      <+> opt
        (not $ null rts)
        ( hsep $
            text "extends"
              : punctuate (text " &") (map (prettyPrec p) rts)
        )

ppTypeParams :: (Pretty a) => Int -> [a] -> Doc
ppTypeParams _ [] = empty
ppTypeParams p tps =
  char '<'
    <> hsep (punctuate comma (map (prettyPrec p) tps))
    <> char '>'

ppImplements :: Int -> [RefType] -> Doc
ppImplements _ [] = empty
ppImplements p rts =
  text "implements"
    <+> hsep (punctuate comma (map (prettyPrec p) rts))

ppExtends :: Int -> [RefType] -> Doc
ppExtends _ [] = empty
ppExtends p rts =
  text "extends"
    <+> hsep (punctuate comma (map (prettyPrec p) rts))

ppPermits :: Int -> [RefType] -> Doc
ppPermits _ [] = empty
ppPermits p rts =
  text "permits"
    <+> hsep (punctuate comma (map (prettyPrec p) rts))

ppThrows :: Int -> [ExceptionType] -> Doc
ppThrows _ [] = empty
ppThrows p ets =
  text "throws"
    <+> hsep (punctuate comma (map (prettyPrec p) ets))

ppDefault :: (PrettyExtension p) => Int -> Maybe (Exp p) -> Doc
ppDefault _ Nothing = empty
ppDefault p (Just expr) = text "default" <+> prettyPrec p expr

ppResultType :: Int -> Maybe Type -> Doc
ppResultType _ Nothing = text "void"
ppResultType p (Just a) = prettyPrec p a

-----------------------------------------------------------------------
-- Names and identifiers

instance Pretty Name where
  prettyPrec p (Name _ is) =
    hcat (punctuate (char '.') $ map (prettyPrec p) (NonEmpty.toList is))

instance Pretty ClassifiedExpressionName where
  prettyPrec p (LocalFieldAccess _ i is) = hcat (punctuate (char '.') $ map (prettyPrec p) (i : is))
  prettyPrec p (Other name) = prettyPrec p name

instance Pretty ClassifiedName where
  prettyPrec p (ExpressionName ename) = prettyPrec p ename
  prettyPrec p (TypeName name) = prettyPrec p name
  prettyPrec p (Unknown name) = prettyPrec p name

instance Pretty Ident where
  prettyPrec _ (Ident _ s) = text s

-----------------------------------------------------------------------
-- Help functionality
prettyNestedStmt :: (PrettyExtension p) => Int -> Stmt p -> Doc
prettyNestedStmt prio p@(StmtBlock _) = prettyPrec prio p
prettyNestedStmt prio p = nest 2 (prettyPrec prio p)

maybePP :: (Pretty a) => Int -> Maybe a -> Doc
maybePP p = maybe empty (prettyPrec p)

opt :: Bool -> Doc -> Doc
opt x a = if x then a else empty

braceBlock :: [Doc] -> Doc
braceBlock xs =
  char '{'
    $+$ nest 2 (vcat xs)
    $+$ char '}'

opPrec Mult = 3
opPrec Div = 3
opPrec Rem = 3
opPrec Add = 4
opPrec Sub = 4
opPrec LShift = 5
opPrec RShift = 5
opPrec RRShift = 5
opPrec LThan = 6
opPrec GThan = 6
opPrec LThanE = 6
opPrec GThanE = 6
opPrec Equal = 7
opPrec NotEq = 7
opPrec And = 8
opPrec Xor = 9
opPrec Or = 10
opPrec CAnd = 11
opPrec COr = 12

escapeGeneral :: Char -> String
escapeGeneral '\b' = "\\b"
escapeGeneral '\t' = "\\t"
escapeGeneral '\n' = "\\n"
escapeGeneral '\f' = "\\f"
escapeGeneral '\r' = "\\r"
escapeGeneral '\\' = "\\\\"
escapeGeneral c
  | c >= ' ' && c < '\DEL' = [c]
  | c <= '\xFFFF' = printf "\\u%04x" (fromEnum c)
  | otherwise = error $ "Language.Java.Pretty.escapeGeneral: Char " ++ show c ++ " too large for Java char"

escapeChar :: Char -> String
escapeChar '\'' = "\\'"
escapeChar c = escapeGeneral c

escapeString :: Char -> String
escapeString '"' = "\\\""
escapeString c
  | c <= '\xFFFF' = escapeGeneral c
  | otherwise = escapeGeneral lead ++ escapeGeneral trail
  where
    c' = fromEnum c - 0x010000
    lead = toEnum $ 0xD800 + c' `div` 0x0400
    trail = toEnum $ 0xDC00 + c' `mod` 0x0400
