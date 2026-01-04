{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Java.Syntax
  ( CompilationUnit (..),
    PackageDecl (..),
    ImportDecl (..),
    TypeDecl (..),
    ClassDecl (..),
    ClassBody (..),
    EnumBody (..),
    EnumConstant (..),
    InterfaceDecl (..),
    InterfaceBody (..),
    InterfaceKind (..),
    Decl (..),
    MemberDecl (..),
    RecordFieldDecl (..),
    VarDecl (..),
    VarDeclId (..),
    VarInit (..),
    FormalParam (..),
    CatchFormalParam (..),
    MethodBody (..),
    ConstructorBody (..),
    ExplConstrInv (..),
    Modifier (..),
    Annotation (..),
    ElementValue (..),
    Block (..),
    BlockStmt (..),
    Stmt (..),
    Catch (..),
    TryResource (..),
    ResourceDecl (..),
    SwitchBlock (..),
    SwitchLabel (..),
    SwitchExpBranch (..),
    SwitchExpBranchBody (..),
    SwitchStyle (..),
    ForInit (..),
    ExceptionType,
    Argument,
    Exp (..),
    Lhs (..),
    ArrayIndex (..),
    FieldAccess (..),
    LambdaParams (..),
    LambdaExpression (..),
    ArrayInit (..),
    MethodInvocation (..),
    MethodRefTarget (..),
    Type (..),
    RefType (..),
    ClassType (..),
    TypeArgument (..),
    TypeDeclSpecifier (..),
    Diamond (..),
    WildcardBound (..),
    PrimType (..),
    Literal (..),
    TypeParam (..),
    Ident (..),
    Name (..),
    ClassifiedExpressionName (..),
    ClassifiedName (..),
    Op (..),
    AssignOp (..),
    module Language.Java.Equality,
    Parsed,
    Analyzed,
    XNameClassification,
  )
where

import Data.Data
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Language.Java.Equality
import Language.Java.SourceSpan

-----------------------------------------------------------------------
-- AST types

data Parsed
  deriving (Data)

data Analyzed
  deriving (Data)

-- type family classes

class (Show (XNameClassification x)) => ShowExtension x

instance ShowExtension Parsed

instance ShowExtension Analyzed

class (Read (XNameClassification x)) => ReadExtension x

instance ReadExtension Parsed

instance ReadExtension Analyzed

class (Data x, Data (XNameClassification x)) => DataExtension x

instance DataExtension Parsed

instance DataExtension Analyzed

class (Equality (XNameClassification x)) => EqualityExtension x

instance EqualityExtension Parsed

instance EqualityExtension Analyzed

class (Located (XNameClassification x)) => LocatedExtension x

instance LocatedExtension Parsed

instance LocatedExtension Analyzed

-----------------------------------------------------------------------
-- Packages

-- | A compilation unit is the top level syntactic goal symbol of a Java program.
data CompilationUnit p = CompilationUnit (Maybe PackageDecl) [ImportDecl] [TypeDecl p]
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (CompilationUnit p)

deriving instance (ReadExtension p) => Read (CompilationUnit p)

deriving instance (DataExtension p) => Data (CompilationUnit p)

instance (EqualityExtension p) => Equality (CompilationUnit p) where
  eq opt (CompilationUnit mpd1 ids1 tds1) (CompilationUnit mpd2 ids2 tds2) =
    eq opt mpd1 mpd2 && eq opt ids1 ids2 && eq opt tds1 tds2

-- | A package declaration appears within a compilation unit to indicate the package to which the compilation unit belongs.
newtype PackageDecl = PackageDecl Name
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality PackageDecl where
  eq opt (PackageDecl n1) (PackageDecl n2) =
    eq opt n1 n2

-- | An import declaration allows a static member or a named type to be referred to by a single unqualified identifier.
--   The first argument signals whether the declaration only imports static members.
--   The last argument signals whether the declaration brings all names in the named type or package, or only brings
--   a single name into scope.
data ImportDecl
  = ImportDecl SourceSpan Bool {- static? -} Name Bool {- .*? -}
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality ImportDecl where
  eq opt (ImportDecl s1 b11 n1 b12) (ImportDecl s2 b21 n2 b22) =
    eq opt s1 s2 && b11 == b21 && eq opt n1 n2 && b12 == b22

instance Located ImportDecl where
  sourceSpan (ImportDecl s _ _ _) = s

-----------------------------------------------------------------------
-- Declarations

-- | A type declaration declares a class type or an interface type.
data TypeDecl p
  = ClassTypeDecl (ClassDecl p)
  | InterfaceTypeDecl (InterfaceDecl p)
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (TypeDecl p)

deriving instance (ReadExtension p) => Read (TypeDecl p)

deriving instance (DataExtension p) => Data (TypeDecl p)

instance (EqualityExtension p) => Equality (TypeDecl p) where
  eq opt (ClassTypeDecl cd1) (ClassTypeDecl cd2) =
    eq opt cd1 cd2
  eq opt (InterfaceTypeDecl id1) (InterfaceTypeDecl id2) =
    eq opt id1 id2
  eq _ _ _ = False

instance Located (TypeDecl p) where
  sourceSpan (ClassTypeDecl cd) = sourceSpan cd
  sourceSpan (InterfaceTypeDecl itd) = sourceSpan itd

-- | A class declaration specifies a new named reference type.
data ClassDecl p
  = ClassDecl SourceSpan [Modifier p] Ident [TypeParam] (Maybe RefType) [RefType] (ClassBody p)
  | RecordDecl SourceSpan [Modifier p] Ident [TypeParam] [RecordFieldDecl] [RefType] (ClassBody p)
  | EnumDecl SourceSpan [Modifier p] Ident [RefType] (EnumBody p)
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (ClassDecl p)

deriving instance (ReadExtension p) => Read (ClassDecl p)

deriving instance (DataExtension p) => Data (ClassDecl p)

instance (EqualityExtension p) => Equality (ClassDecl p) where
  eq opt (ClassDecl s1 ms1 i1 tps1 mrt1 rts1 cb1) (ClassDecl s2 ms2 i2 tps2 mrt2 rts2 cb2) =
    eq opt s1 s2 && eq opt ms1 ms2 && eq opt i1 i2 && eq opt tps1 tps2 && eq opt mrt1 mrt2 && eq opt rts1 rts2 && eq opt cb1 cb2
  eq opt (RecordDecl s1 ms1 i1 tps1 rfds1 rts1 cb1) (RecordDecl s2 ms2 i2 tps2 rfds2 rts2 cb2) =
    eq opt s1 s2 && eq opt ms1 ms2 && eq opt i1 i2 && eq opt tps1 tps2 && eq opt rfds1 rfds2 && eq opt rts1 rts2 && eq opt cb1 cb2
  eq opt (EnumDecl s1 ms1 i1 rts1 eb1) (EnumDecl s2 ms2 i2 rts2 eb2) =
    eq opt s1 s2 && eq opt ms1 ms2 && eq opt i1 i2 && eq opt rts1 rts2 && eq opt eb1 eb2
  eq _ _ _ = False

instance Located (ClassDecl p) where
  sourceSpan (ClassDecl s _ _ _ _ _ _) = s
  sourceSpan (RecordDecl s _ _ _ _ _ _) = s
  sourceSpan (EnumDecl s _ _ _ _) = s

-- | A class body may contain declarations of members of the class, that is,
--   fields, classes, interfaces and methods.
--   A class body may also contain instance initializers, static
--   initializers, and declarations of constructors for the class.
newtype ClassBody p = ClassBody [Decl p]
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (ClassBody p)

deriving instance (ReadExtension p) => Read (ClassBody p)

deriving instance (DataExtension p) => Data (ClassBody p)

instance (EqualityExtension p) => Equality (ClassBody p) where
  eq opt (ClassBody ds1) (ClassBody ds2) =
    eq opt ds1 ds2

-- | The body of an enum type may contain enum constants.
data EnumBody p = EnumBody [EnumConstant p] [Decl p]
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (EnumBody p)

deriving instance (ReadExtension p) => Read (EnumBody p)

deriving instance (DataExtension p) => Data (EnumBody p)

instance (EqualityExtension p) => Equality (EnumBody p) where
  eq opt (EnumBody ecs1 ds1) (EnumBody ecs2 ds2) =
    eq opt ecs1 ecs2 && eq opt ds1 ds2

-- | An enum constant defines an instance of the enum type.
data EnumConstant p = EnumConstant Ident [Argument p] (Maybe (ClassBody p))
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (EnumConstant p)

deriving instance (ReadExtension p) => Read (EnumConstant p)

deriving instance (DataExtension p) => Data (EnumConstant p)

instance (EqualityExtension p) => Equality (EnumConstant p) where
  eq opt (EnumConstant i1 as1 mcb1) (EnumConstant i2 as2 mcb2) =
    eq opt i1 i2 && eq opt as1 as2 && eq opt mcb1 mcb2

-- | An interface declaration introduces a new reference type whose members
--   are classes, interfaces, constants and abstract methods. This type has
--   no implementation, but otherwise unrelated classes can implement it by
--   providing implementations for its abstract methods.
data InterfaceDecl p
  = InterfaceDecl SourceSpan InterfaceKind [Modifier p] Ident [TypeParam] [RefType {- extends -}] [RefType {- permits -}] (InterfaceBody p)
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (InterfaceDecl p)

deriving instance (ReadExtension p) => Read (InterfaceDecl p)

deriving instance (DataExtension p) => Data (InterfaceDecl p)

instance (EqualityExtension p) => Equality (InterfaceDecl p) where
  eq opt (InterfaceDecl s1 ik1 ms1 i1 tps1 rts11 rts12 ib1) (InterfaceDecl s2 ik2 ms2 i2 tps2 rts21 rts22 ib2) =
    eq opt s1 s2 && eq opt ik1 ik2 && eq opt ms1 ms2 && eq opt i1 i2 && eq opt tps1 tps2 && eq opt rts11 rts21 && eq opt rts12 rts22 && eq opt ib1 ib2

instance Located (InterfaceDecl p) where
  sourceSpan (InterfaceDecl s _ _ _ _ _ _ _) = s

-- | Interface can declare either a normal interface or an annotation
data InterfaceKind = InterfaceNormal | InterfaceAnnotation
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality InterfaceKind where
  eq _ InterfaceNormal InterfaceNormal = True
  eq _ InterfaceAnnotation InterfaceAnnotation = True
  eq _ _ _ = False

-- | The body of an interface may declare members of the interface.
newtype InterfaceBody p
  = InterfaceBody [MemberDecl p]
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (InterfaceBody p)

deriving instance (ReadExtension p) => Read (InterfaceBody p)

deriving instance (DataExtension p) => Data (InterfaceBody p)

instance (EqualityExtension p) => Equality (InterfaceBody p) where
  eq opt (InterfaceBody mds1) (InterfaceBody mds2) =
    eq opt mds1 mds2

-- | A declaration is either a member declaration, or a declaration of an
--   initializer, which may be static.
data Decl p
  = MemberDecl (MemberDecl p)
  | InitDecl SourceSpan Bool (Block p)
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (Decl p)

deriving instance (ReadExtension p) => Read (Decl p)

deriving instance (DataExtension p) => Data (Decl p)

instance (EqualityExtension p) => Equality (Decl p) where
  eq opt (MemberDecl md1) (MemberDecl md2) =
    eq opt md1 md2
  eq opt (InitDecl s1 b1 bl1) (InitDecl s2 b2 bl2) =
    eq opt s1 s2 && b1 == b2 && eq opt bl1 bl2
  eq _ _ _ = False

instance Located (Decl p) where
  sourceSpan (MemberDecl md) = sourceSpan md
  sourceSpan (InitDecl s _ _) = s

-- | A class or interface member can be an inner class or interface, a field or
--   constant, or a method or constructor. An interface may only have as members
--   constants (not fields), abstract methods, and no constructors.
data MemberDecl p
  = -- | The variables of a class type are introduced by field declarations.
    FieldDecl SourceSpan [Modifier p] Type (NonEmpty (VarDecl p))
  | -- | A method declares executable code that can be invoked, passing a fixed number of values as arguments.
    MethodDecl SourceSpan [Modifier p] [TypeParam] (Maybe Type) Ident [FormalParam p] [ExceptionType] (Maybe (Exp p)) (MethodBody p)
  | -- | A constructor is used in the creation of an object that is an instance of a class.
    ConstructorDecl SourceSpan [Modifier p] [TypeParam] Ident [FormalParam p] [ExceptionType] (ConstructorBody p)
  | -- | A member class is a class whose declaration is directly enclosed in another class or interface declaration.
    MemberClassDecl (ClassDecl p)
  | -- | A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
    MemberInterfaceDecl (InterfaceDecl p)
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (MemberDecl p)

deriving instance (ReadExtension p) => Read (MemberDecl p)

deriving instance (DataExtension p) => Data (MemberDecl p)

instance (EqualityExtension p) => Equality (MemberDecl p) where
  eq opt (FieldDecl s1 ms1 t1 vds1) (FieldDecl s2 ms2 t2 vds2) =
    eq opt s1 s2 && eq opt ms1 ms2 && eq opt t1 t2 && eq opt vds1 vds2
  eq opt (MethodDecl s1 ms1 tps1 mt1 i1 fps1 ets1 me1 mb1) (MethodDecl s2 ms2 tps2 mt2 i2 fps2 ets2 me2 mb2) =
    eq opt s1 s2 && eq opt ms1 ms2 && eq opt tps1 tps2 && eq opt mt1 mt2 && eq opt i1 i2 && eq opt fps1 fps2 && eq opt ets1 ets2 && eq opt me1 me2 && eq opt mb1 mb2
  eq opt (ConstructorDecl s1 ms1 tps1 i1 fps1 ets1 cb1) (ConstructorDecl s2 ms2 tps2 i2 fps2 ets2 cb2) =
    eq opt s1 s2 && eq opt ms1 ms2 && eq opt tps1 tps2 && eq opt i1 i2 && eq opt fps1 fps2 && eq opt ets1 ets2 && eq opt cb1 cb2
  eq opt (MemberClassDecl cd1) (MemberClassDecl cd2) =
    eq opt cd1 cd2
  eq opt (MemberInterfaceDecl id1) (MemberInterfaceDecl id2) =
    eq opt id1 id2
  eq _ _ _ = False

instance Located (MemberDecl p) where
  sourceSpan (FieldDecl s _ _ _) = s
  sourceSpan (MethodDecl s _ _ _ _ _ _ _ _) = s
  sourceSpan (ConstructorDecl s _ _ _ _ _ _) = s
  sourceSpan (MemberClassDecl mcd) = sourceSpan mcd
  sourceSpan (MemberInterfaceDecl mid) = sourceSpan mid

-- | A field declaration of a record
data RecordFieldDecl
  = RecordFieldDecl Type Ident
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality RecordFieldDecl where
  eq opt (RecordFieldDecl t1 i1) (RecordFieldDecl t2 i2) =
    eq opt t1 t2 && eq opt i1 i2

-- | A declaration of a variable, which may be explicitly initialized.
data VarDecl p
  = VarDecl SourceSpan VarDeclId (Maybe (VarInit p))
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (VarDecl p)

deriving instance (ReadExtension p) => Read (VarDecl p)

deriving instance (DataExtension p) => Data (VarDecl p)

instance (EqualityExtension p) => Equality (VarDecl p) where
  eq opt (VarDecl s1 vdi1 mvi1) (VarDecl s2 vdi2 mvi2) =
    eq opt s1 s2 && eq opt vdi1 vdi2 && eq opt mvi1 mvi2

instance Located (VarDecl p) where
  sourceSpan (VarDecl s _ _) = s

-- | The name of a variable in a declaration, which may be an array.
data VarDeclId
  = VarId Ident
  | -- | Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.
    VarDeclArray SourceSpan VarDeclId
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality VarDeclId where
  eq opt (VarId i1) (VarId i2) =
    eq opt i1 i2
  eq opt (VarDeclArray s1 vdi1) (VarDeclArray s2 vdi2) =
    eq opt s1 s2 && eq opt vdi1 vdi2
  eq _ _ _ = False

instance Located VarDeclId where
  sourceSpan (VarId i) = sourceSpan i
  sourceSpan (VarDeclArray s _) = s

-- | Explicit initializer for a variable declaration.
data VarInit p
  = InitExp (Exp p)
  | InitArray (ArrayInit p)
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (VarInit p)

deriving instance (ReadExtension p) => Read (VarInit p)

deriving instance (DataExtension p) => Data (VarInit p)

instance (EqualityExtension p) => Equality (VarInit p) where
  eq opt (InitExp e1) (InitExp e2) =
    eq opt e1 e2
  eq opt (InitArray ai1) (InitArray ai2) =
    eq opt ai1 ai2
  eq _ _ _ = False

instance (LocatedExtension p) => Located (VarInit p) where
  sourceSpan (InitExp e) = sourceSpan e
  sourceSpan (InitArray ai) = sourceSpan ai

-- | A formal parameter in method declaration. The last parameter
--   for a given declaration may be marked as variable arity,
--   indicated by the boolean argument.
data FormalParam p = FormalParam SourceSpan [Modifier p] Type Bool VarDeclId
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (FormalParam p)

deriving instance (ReadExtension p) => Read (FormalParam p)

deriving instance (DataExtension p) => Data (FormalParam p)

instance (EqualityExtension p) => Equality (FormalParam p) where
  eq opt (FormalParam s1 ms1 t1 b1 vdi1) (FormalParam s2 ms2 t2 b2 vdi2) =
    eq opt s1 s2 && eq opt ms1 ms2 && eq opt t1 t2 && b1 == b2 && eq opt vdi1 vdi2

instance Located (FormalParam p) where
  sourceSpan (FormalParam s _ _ _ _) = s

-- | A formal parameter in catch clauses. Unlike formal parameters in
--   method declarations, multiple types can be specified.
data CatchFormalParam p = CatchFormalParam SourceSpan (NonEmpty ([Modifier p], Type)) VarDeclId
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (CatchFormalParam p)

deriving instance (ReadExtension p) => Read (CatchFormalParam p)

deriving instance (DataExtension p) => Data (CatchFormalParam p)

instance (EqualityExtension p) => Equality (CatchFormalParam p) where
  eq opt (CatchFormalParam s1 t1 vdi1) (CatchFormalParam s2 t2 vdi2) =
    eq opt s1 s2 && eq opt t1 t2 && eq opt vdi1 vdi2

instance Located (CatchFormalParam p) where
  sourceSpan (CatchFormalParam s _ _) = s

-- | A method body is either a block of code that implements the method or simply a
--   semicolon, indicating the lack of an implementation (modelled by 'Nothing').
newtype MethodBody p = MethodBody (Maybe (Block p))
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (MethodBody p)

deriving instance (ReadExtension p) => Read (MethodBody p)

deriving instance (DataExtension p) => Data (MethodBody p)

instance (EqualityExtension p) => Equality (MethodBody p) where
  eq opt (MethodBody mb1) (MethodBody mb2) =
    eq opt mb1 mb2

-- | The first statement of a constructor body may be an explicit invocation of
--   another constructor of the same class or of the direct superclass.
data ConstructorBody p = ConstructorBody (Maybe (ExplConstrInv p)) [BlockStmt p]
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (ConstructorBody p)

deriving instance (ReadExtension p) => Read (ConstructorBody p)

deriving instance (DataExtension p) => Data (ConstructorBody p)

instance (EqualityExtension p) => Equality (ConstructorBody p) where
  eq opt (ConstructorBody meci1 bss1) (ConstructorBody meci2 bss2) =
    eq opt meci1 meci2 && eq opt bss1 bss2

-- | An explicit constructor invocation invokes another constructor of the
--   same class, or a constructor of the direct superclass, which may
--   be qualified to explicitly specify the newly created object's immediately
--   enclosing instance.
data ExplConstrInv p
  = ThisInvoke [RefType] [Argument p]
  | SuperInvoke [RefType] [Argument p]
  | PrimarySuperInvoke (Exp p) [RefType] [Argument p]
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (ExplConstrInv p)

deriving instance (ReadExtension p) => Read (ExplConstrInv p)

deriving instance (DataExtension p) => Data (ExplConstrInv p)

instance (EqualityExtension p) => Equality (ExplConstrInv p) where
  eq opt (ThisInvoke rts1 as1) (ThisInvoke rts2 as2) =
    eq opt rts1 rts2 && eq opt as1 as2
  eq opt (SuperInvoke rts1 as1) (SuperInvoke rts2 as2) =
    eq opt rts1 rts2 && eq opt as1 as2
  eq opt (PrimarySuperInvoke e1 rts1 as1) (PrimarySuperInvoke e2 rts2 as2) =
    eq opt e1 e2 && eq opt rts1 rts2 && eq opt as1 as2
  eq _ _ _ = False

-- | A modifier specifying properties of a given declaration. In general only
--   a few of these modifiers are allowed for each declaration type, for instance
--   a member type declaration may only specify one of public, private or protected.
data Modifier p
  = Public SourceSpan
  | Private SourceSpan
  | Protected SourceSpan
  | Abstract SourceSpan
  | Final SourceSpan
  | Static SourceSpan
  | StrictFP SourceSpan
  | Transient SourceSpan
  | Volatile SourceSpan
  | Native SourceSpan
  | Annotation (Annotation p)
  | Synchronized_ SourceSpan
  | Sealed SourceSpan
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (Modifier p)

deriving instance (ReadExtension p) => Read (Modifier p)

deriving instance (DataExtension p) => Data (Modifier p)

instance (EqualityExtension p) => Equality (Modifier p) where
  eq opt (Public s1) (Public s2) =
    eq opt s1 s2
  eq opt (Private s1) (Private s2) =
    eq opt s1 s2
  eq opt (Protected s1) (Protected s2) =
    eq opt s1 s2
  eq opt (Abstract s1) (Abstract s2) =
    eq opt s1 s2
  eq opt (Final s1) (Final s2) =
    eq opt s1 s2
  eq opt (Static s1) (Static s2) =
    eq opt s1 s2
  eq opt (StrictFP s1) (StrictFP s2) =
    eq opt s1 s2
  eq opt (Transient s1) (Transient s2) =
    eq opt s1 s2
  eq opt (Volatile s1) (Volatile s2) =
    eq opt s1 s2
  eq opt (Native s1) (Native s2) =
    eq opt s1 s2
  eq opt (Annotation a1) (Annotation a2) =
    eq opt a1 a2
  eq opt (Synchronized_ s1) (Synchronized_ s2) =
    eq opt s1 s2
  eq opt (Sealed s1) (Sealed s2) =
    eq opt s1 s2
  eq _ _ _ = False

instance Located (Modifier p) where
  sourceSpan (Public s) = s
  sourceSpan (Private s) = s
  sourceSpan (Protected s) = s
  sourceSpan (Abstract s) = s
  sourceSpan (Final s) = s
  sourceSpan (Static s) = s
  sourceSpan (StrictFP s) = s
  sourceSpan (Transient s) = s
  sourceSpan (Volatile s) = s
  sourceSpan (Native s) = s
  sourceSpan (Annotation a) = sourceSpan a
  sourceSpan (Synchronized_ s) = s
  sourceSpan (Sealed s) = s

-- | Annotations have three different forms: no-parameter, single-parameter or key-value pairs
data Annotation p
  = NormalAnnotation
      { span :: SourceSpan,
        annName :: Name, -- Not type because not type generics not allowed
        annKV :: NonEmpty (Ident, ElementValue p)
      }
  | SingleElementAnnotation
      { span :: SourceSpan,
        annName :: Name,
        annValue :: ElementValue p
      }
  | MarkerAnnotation
      { span :: SourceSpan,
        annName :: Name
      }
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (Annotation p)

deriving instance (ReadExtension p) => Read (Annotation p)

deriving instance (DataExtension p) => Data (Annotation p)

instance (EqualityExtension p) => Equality (Annotation p) where
  eq opt (NormalAnnotation s1 n1 ievs1) (NormalAnnotation s2 n2 ievs2) =
    eq opt s1 s2 && eq opt n1 n2 && eq opt ievs1 ievs2
  eq opt (SingleElementAnnotation s1 n1 ev1) (SingleElementAnnotation s2 n2 ev2) =
    eq opt s1 s2 && eq opt n1 n2 && eq opt ev1 ev2
  eq opt (MarkerAnnotation s1 n1) (MarkerAnnotation s2 n2) =
    eq opt s1 s2 && eq opt n1 n2
  eq _ _ _ = False

instance Located (Annotation p) where
  sourceSpan (NormalAnnotation s _ _) = s
  sourceSpan (SingleElementAnnotation s _ _) = s
  sourceSpan (MarkerAnnotation s _) = s

-- | Annotations may contain  annotations or (loosely) expressions
data ElementValue p
  = EVVal (VarInit p)
  | EVAnn (Annotation p)
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (ElementValue p)

deriving instance (ReadExtension p) => Read (ElementValue p)

deriving instance (DataExtension p) => Data (ElementValue p)

instance (EqualityExtension p) => Equality (ElementValue p) where
  eq opt (EVVal vi1) (EVVal vi2) =
    eq opt vi1 vi2
  eq opt (EVAnn a1) (EVAnn a2) =
    eq opt a1 a2
  eq _ _ _ = False

instance (LocatedExtension p) => Located (ElementValue p) where
  sourceSpan (EVVal vi) = sourceSpan vi
  sourceSpan (EVAnn ann) = sourceSpan ann

-----------------------------------------------------------------------
-- Statements

-- | A block is a sequence of statements, local class declarations
--   and local variable declaration statements within braces.
data Block p = Block SourceSpan [BlockStmt p]
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (Block p)

deriving instance (ReadExtension p) => Read (Block p)

deriving instance (DataExtension p) => Data (Block p)

instance (EqualityExtension p) => Equality (Block p) where
  eq opt (Block s1 bss1) (Block s2 bss2) =
    eq opt s1 s2 && eq opt bss1 bss2

instance Located (Block p) where
  sourceSpan (Block s _) = s

-- | A block statement is either a normal statement, a local
--   class declaration or a local variable declaration.
data BlockStmt p
  = BlockStmt (Stmt p)
  | LocalClass (ClassDecl p)
  | LocalVars SourceSpan [Modifier p] Type (NonEmpty (VarDecl p))
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (BlockStmt p)

deriving instance (ReadExtension p) => Read (BlockStmt p)

deriving instance (DataExtension p) => Data (BlockStmt p)

instance (EqualityExtension p) => Equality (BlockStmt p) where
  eq opt (BlockStmt stmt1) (BlockStmt stmt2) =
    eq opt stmt1 stmt2
  eq opt (LocalClass cd1) (LocalClass cd2) =
    eq opt cd1 cd2
  eq opt (LocalVars s1 ms1 t1 vds1) (LocalVars s2 ms2 t2 vds2) =
    eq opt s1 s2 && eq opt ms1 ms2 && eq opt t1 t2 && eq opt vds1 vds2
  eq _ _ _ = False

instance Located (BlockStmt p) where
  sourceSpan (BlockStmt s) = sourceSpan s
  sourceSpan (LocalClass cd) = sourceSpan cd
  sourceSpan (LocalVars s _ _ _) = s

-- | A Java statement.
data Stmt p
  = -- | A statement can be a nested block.
    StmtBlock (Block p)
  | -- | The @if-then@ statement allows conditional execution of a statement.
    IfThen SourceSpan (Exp p) (Stmt p)
  | -- | The @if-then-else@ statement allows conditional choice of two statements, executing one or the other but not both.
    IfThenElse SourceSpan (Exp p) (Stmt p) (Stmt p)
  | -- | The @while@ statement executes an expression and a statement repeatedly until the value of the expression is false.
    While SourceSpan (Exp p) (Stmt p)
  | -- | The basic @for@ statement executes some initialization code, then executes an expression, a statement, and some
    --   update code repeatedly until the value of the expression is false.
    BasicFor SourceSpan (Maybe (ForInit p)) (Maybe (Exp p)) [Exp p] (Stmt p)
  | -- | The enhanced @for@ statement iterates over an array or a value of a class that implements the @iterator@ interface.
    EnhancedFor SourceSpan [Modifier p] Type Ident (Exp p) (Stmt p)
  | -- | An empty statement does nothing.
    Empty SourceSpan
  | -- | Certain kinds of expressions may be used as statements by following them with semicolons:
    --   assignments, pre- or post-inc- or decrementation, method invocation or class instance
    --   creation expressions.
    ExpStmt SourceSpan (Exp p)
  | -- | An assertion is a statement containing a boolean expression, where an error is reported if the expression
    --   evaluates to false.
    Assert SourceSpan (Exp p) (Maybe (Exp p))
  | -- | The switch statement transfers control to one of several statements depending on the value of an expression.
    Switch SourceSpan SwitchStyle (Exp p) [SwitchBlock p]
  | -- | The @do@ statement executes a statement and an expression repeatedly until the value of the expression is false.
    Do SourceSpan (Stmt p) (Exp p)
  | -- | A @break@ statement transfers control out of an enclosing statement.
    Break SourceSpan (Maybe Ident)
  | -- | A @continue@ statement may occur only in a while, do, or for statement. Control passes to the loop-continuation
    --   point of that statement.
    Continue SourceSpan (Maybe Ident)
  | -- A @return@ statement returns control to the invoker of a method or constructor.
    Return SourceSpan (Maybe (Exp p))
  | -- | A @synchronized@ statement acquires a mutual-exclusion lock on behalf of the executing thread, executes a block,
    --   then releases the lock. While the executing thread owns the lock, no other thread may acquire the lock.
    Synchronized SourceSpan (Exp p) (Block p)
  | -- | A @throw@ statement causes an exception to be thrown.
    Throw SourceSpan (Exp p)
  | -- | A try statement executes a block. If a value is thrown and the try statement has one or more catch clauses that
    --   can catch it, then control will be transferred to the first such catch clause. If the try statement has a finally
    --   clause, then another block of code is executed, no matter whether the try block completes normally or abruptly,
    --   and no matter whether a catch clause is first given control.
    Try SourceSpan [TryResource p] (Block p) [Catch p] (Maybe {- finally -} (Block p))
  | -- | Statements may have label prefixes.
    Labeled SourceSpan Ident (Stmt p)
  deriving (Generic, Typeable)

deriving instance (ShowExtension p) => Show (Stmt p)

deriving instance (ReadExtension p) => Read (Stmt p)

deriving instance (DataExtension p) => Data (Stmt p)

instance (EqualityExtension p) => Equality (Stmt p) where
  eq opt (StmtBlock b1) (StmtBlock b2) =
    eq opt b1 b2
  eq opt (IfThen s1 e1 stmt1) (IfThen s2 e2 stmt2) =
    eq opt s1 s2 && eq opt e1 e2 && eq opt stmt1 stmt2
  eq opt (IfThenElse s1 e1 stmt11 stmt12) (IfThenElse s2 e2 stmt21 stmt22) =
    eq opt s1 s2 && eq opt e1 e2 && eq opt stmt11 stmt21 && eq opt stmt12 stmt22
  eq opt (While s1 e1 stmt1) (While s2 e2 stmt2) =
    eq opt s1 s2 && eq opt e1 e2 && eq opt stmt1 stmt2
  eq opt (BasicFor s1 mfi1 me1 mes1 stmt1) (BasicFor s2 mfi2 me2 mes2 stmt2) =
    eq opt s1 s2 && eq opt mfi1 mfi2 && eq opt me1 me2 && eq opt mes1 mes2 && eq opt stmt1 stmt2
  eq opt (EnhancedFor s1 ms1 t1 i1 e1 stmt1) (EnhancedFor s2 ms2 t2 i2 e2 stmt2) =
    eq opt s1 s2 && eq opt ms1 ms2 && eq opt t1 t2 && eq opt i1 i2 && eq opt e1 e2 && eq opt stmt1 stmt2
  eq opt (Empty s1) (Empty s2) =
    eq opt s1 s2
  eq opt (ExpStmt s1 e1) (ExpStmt s2 e2) =
    eq opt s1 s2 && eq opt e1 e2
  eq opt (Assert s1 e1 me1) (Assert s2 e2 me2) =
    eq opt s1 s2 && eq opt e1 e2 && eq opt me1 me2
  eq opt (Switch s1 ss1 e1 sbs1) (Switch s2 ss2 e2 sbs2) =
    eq opt s1 s2 && eq opt ss1 ss2 && eq opt e1 e2 && eq opt sbs1 sbs2
  eq opt (Do s1 stmt1 e1) (Do s2 stmt2 e2) =
    eq opt s1 s2 && eq opt stmt1 stmt2 && eq opt e1 e2
  eq opt (Break s1 mi1) (Break s2 mi2) =
    eq opt s1 s2 && eq opt mi1 mi2
  eq opt (Continue s1 mi1) (Continue s2 mi2) =
    eq opt s1 s2 && eq opt mi1 mi2
  eq opt (Return s1 me1) (Return s2 me2) =
    eq opt s1 s2 && eq opt me1 me2
  eq opt (Synchronized s1 e1 b1) (Synchronized s2 e2 b2) =
    eq opt s1 s2 && eq opt e1 e2 && eq opt b1 b2
  eq opt (Throw s1 e1) (Throw s2 e2) =
    eq opt s1 s2 && eq opt e1 e2
  eq opt (Try s1 trs1 b1 cs1 mb1) (Try s2 trs2 b2 cs2 mb2) =
    eq opt s1 s2 && eq opt trs1 trs2 && eq opt b1 b2 && eq opt cs1 cs2 && eq opt mb1 mb2
  eq opt (Labeled s1 i1 stmt1) (Labeled s2 i2 stmt2) =
    eq opt s1 s2 && eq opt i1 i2 && eq opt stmt1 stmt2
  eq _ _ _ = False

instance Located (Stmt p) where
  sourceSpan (StmtBlock b) = sourceSpan b
  sourceSpan (IfThen s _ _) = s
  sourceSpan (IfThenElse s _ _ _) = s
  sourceSpan (While s _ _) = s
  sourceSpan (BasicFor s _ _ _ _) = s
  sourceSpan (EnhancedFor s _ _ _ _ _) = s
  sourceSpan (Empty s) = s
  sourceSpan (ExpStmt s _) = s
  sourceSpan (Assert s _ _) = s
  sourceSpan (Switch s _ _ _) = s
  sourceSpan (Do s _ _) = s
  sourceSpan (Break s _) = s
  sourceSpan (Continue s _) = s
  sourceSpan (Return s _) = s
  sourceSpan (Synchronized s _ _) = s
  sourceSpan (Throw s _) = s
  sourceSpan (Try s _ _ _ _) = s
  sourceSpan (Labeled s _ _) = s

-- | If a value is thrown and the try statement has one or more catch clauses that can catch it, then control will be
--   transferred to the first such catch clause.
data Catch p = Catch (CatchFormalParam p) (Block p)
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (Catch p)

deriving instance (ReadExtension p) => Read (Catch p)

deriving instance (DataExtension p) => Data (Catch p)

instance (EqualityExtension p) => Equality (Catch p) where
  eq opt (Catch fp1 b1) (Catch fp2 b2) =
    eq opt fp1 fp2 && eq opt b1 b2

data TryResource p
  = TryResourceVarDecl (ResourceDecl p)
  | TryResourceVarAccess Ident
  | TryResourceQualAccess (FieldAccess p)
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (TryResource p)

deriving instance (ReadExtension p) => Read (TryResource p)

deriving instance (DataExtension p) => Data (TryResource p)

instance (EqualityExtension p) => Equality (TryResource p) where
  eq opt (TryResourceVarDecl rd1) (TryResourceVarDecl rd2) =
    eq opt rd1 rd2
  eq opt (TryResourceVarAccess i1) (TryResourceVarAccess i2) =
    eq opt i1 i2
  eq opt (TryResourceQualAccess fa1) (TryResourceQualAccess fa2) =
    eq opt fa1 fa2
  eq _ _ _ = False

data ResourceDecl p
  = ResourceDecl [Modifier p] Type VarDeclId (VarInit p)
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (ResourceDecl p)

deriving instance (ReadExtension p) => Read (ResourceDecl p)

deriving instance (DataExtension p) => Data (ResourceDecl p)

instance (EqualityExtension p) => Equality (ResourceDecl p) where
  eq opt (ResourceDecl ms1 t1 vdi1 vi1) (ResourceDecl ms2 t2 vdi2 vi2) =
    eq opt ms1 ms2 && eq opt t1 t2 && eq opt vdi1 vdi2 && eq opt vi1 vi2

-- | A block of code labelled with a @case@ or @default@ within a @switch@ statement.
data SwitchBlock p
  = SwitchBlock SourceSpan (SwitchLabel p) [BlockStmt p]
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (SwitchBlock p)

deriving instance (ReadExtension p) => Read (SwitchBlock p)

deriving instance (DataExtension p) => Data (SwitchBlock p)

instance (EqualityExtension p) => Equality (SwitchBlock p) where
  eq opt (SwitchBlock s1 sl1 bss1) (SwitchBlock s2 sl2 bss2) =
    eq opt s1 s2 && eq opt sl1 sl2 && eq opt bss1 bss2

instance Located (SwitchBlock p) where
  sourceSpan (SwitchBlock s _ _) = s

data SwitchStyle
  = SwitchOldStyle
  | SwitchNewStyle -- JEP 361
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality SwitchStyle where
  eq _ SwitchOldStyle SwitchOldStyle = True
  eq _ SwitchNewStyle SwitchNewStyle = True
  eq _ _ _ = False

-- | A label within a @switch@ statement.
data SwitchLabel p
  = -- | The expressions contained in the @case@ must be a 'Lit' or an @enum@ constant.
    -- The list must be non-empty.
    SwitchCase (NonEmpty (Exp p))
  | Default
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (SwitchLabel p)

deriving instance (ReadExtension p) => Read (SwitchLabel p)

deriving instance (DataExtension p) => Data (SwitchLabel p)

instance (EqualityExtension p) => Equality (SwitchLabel p) where
  eq opt (SwitchCase es1) (SwitchCase es2) =
    eq opt es1 es2
  eq _ Default Default = True
  eq _ _ _ = False

data SwitchExpBranch p
  = SwitchExpBranch (SwitchLabel p) (SwitchExpBranchBody p)
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (SwitchExpBranch p)

deriving instance (ReadExtension p) => Read (SwitchExpBranch p)

deriving instance (DataExtension p) => Data (SwitchExpBranch p)

instance (EqualityExtension p) => Equality (SwitchExpBranch p) where
  eq opt (SwitchExpBranch sl1 sebb1) (SwitchExpBranch sl2 sebb2) =
    eq opt sl1 sl2 && eq opt sebb1 sebb2

data SwitchExpBranchBody p
  = SwitchExpBranchExp (Exp p)
  | SwitchExpBranchBlock [BlockStmt p]
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (SwitchExpBranchBody p)

deriving instance (ReadExtension p) => Read (SwitchExpBranchBody p)

deriving instance (DataExtension p) => Data (SwitchExpBranchBody p)

instance (EqualityExtension p) => Equality (SwitchExpBranchBody p) where
  eq opt (SwitchExpBranchExp e1) (SwitchExpBranchExp e2) =
    eq opt e1 e2
  eq opt (SwitchExpBranchBlock bss1) (SwitchExpBranchBlock bss2) =
    eq opt bss1 bss2
  eq _ _ _ = False

-- | Initialization code for a basic @for@ statement.
data ForInit p
  = ForLocalVars [Modifier p] Type (NonEmpty (VarDecl p))
  | ForInitExps (NonEmpty (Exp p))
  deriving (Generic, Typeable)

deriving instance (ShowExtension p) => Show (ForInit p)

deriving instance (ReadExtension p) => Read (ForInit p)

deriving instance (DataExtension p) => Data (ForInit p)

instance (EqualityExtension p) => Equality (ForInit p) where
  eq opt (ForLocalVars ms1 t1 vds1) (ForLocalVars ms2 t2 vds2) =
    eq opt ms1 ms2 && eq opt t1 t2 && eq opt vds1 vds2
  eq opt (ForInitExps es1) (ForInitExps es2) =
    eq opt es1 es2
  eq _ _ _ = False

-- | An exception type has to be a class type or a type variable.
type ExceptionType = RefType -- restricted to ClassType or TypeVariable

-- | Arguments to methods and constructors are expressions.
type Argument p = Exp p

-- | A Java expression.
data Exp p
  = -- | A literal denotes a fixed, unchanging value.
    Lit Literal
  | -- | A class literal, which is an expression consisting of the name of a class, interface, array,
    --   or primitive type, or the pseudo-type void (modelled by 'Nothing'), followed by a `.' and the token class.
    ClassLit SourceSpan (Maybe Type)
  | -- | The keyword @this@ denotes a value that is a reference to the object for which the instance method
    --   was invoked, or to the object being constructed.
    This SourceSpan
  | -- | Any lexically enclosing instance can be referred to by explicitly qualifying the keyword this.
    ThisClass SourceSpan Name
  | -- | A class instance creation expression is used to create new objects that are instances of classes.
    -- | The first argument is a list of non-wildcard type arguments to a generic constructor.
    --   What follows is the type to be instantiated, the list of arguments passed to the constructor, and
    --   optionally a class body that makes the constructor result in an object of an /anonymous/ class.
    InstanceCreation SourceSpan [TypeArgument] TypeDeclSpecifier [Argument p] (Maybe (ClassBody p))
  | -- | A qualified class instance creation expression enables the creation of instances of inner member classes
    --   and their anonymous subclasses.
    QualInstanceCreation SourceSpan (Exp p) [TypeArgument] Ident [Argument p] (Maybe (ClassBody p))
  | -- | An array instance creation expression is used to create new arrays. The last argument denotes the number
    --   of dimensions that have no explicit length given. These dimensions must be given last.
    ArrayCreate SourceSpan Type (NonEmpty (Exp p)) Int
  | -- | An array instance creation expression may come with an explicit initializer. Such expressions may not
    --   be given explicit lengths for any of its dimensions.
    ArrayCreateInit SourceSpan Type Int (ArrayInit p)
  | -- | A field access expression.
    FieldAccess (FieldAccess p)
  | -- | A method invocation expression.
    MethodInv (MethodInvocation p)
  | -- | An array access expression refers to a variable that is a component of an array.
    ArrayAccess (ArrayIndex p)
  | {-    | ArrayAccess Exp Exp -- Should this be made into a datatype, for consistency and use with Lhs? -}

    -- | An expression name, e.g. a variable.
    ExpName (XNameClassification p)
  | -- | Post-incrementation expression, i.e. an expression followed by @++@.
    PostIncrement SourceSpan (Exp p)
  | -- | Post-decrementation expression, i.e. an expression followed by @--@.
    PostDecrement SourceSpan (Exp p)
  | -- | Pre-incrementation expression, i.e. an expression preceded by @++@.
    PreIncrement SourceSpan (Exp p)
  | -- | Pre-decrementation expression, i.e. an expression preceded by @--@.
    PreDecrement SourceSpan (Exp p)
  | -- | Unary plus, the promotion of the value of the expression to a primitive numeric type.
    PrePlus SourceSpan (Exp p)
  | -- | Unary minus, the promotion of the negation of the value of the expression to a primitive numeric type.
    PreMinus SourceSpan (Exp p)
  | -- | Unary bitwise complementation: note that, in all cases, @~x@ equals @(-x)-1@.
    PreBitCompl SourceSpan (Exp p)
  | -- | Logical complementation of boolean values.
    PreNot SourceSpan (Exp p)
  | -- | A cast expression converts, at run time, a value of one numeric type to a similar value of another
    --   numeric type; or confirms, at compile time, that the type of an expression is boolean; or checks,
    --   at run time, that a reference value refers to an object whose class is compatible with a specified
    --   reference type.
    Cast SourceSpan Type (Exp p)
  | -- | The application of a binary operator to two operand expressions.
    BinOp SourceSpan (Exp p) Op (Exp p)
  | -- | Testing whether the result of an expression is an instance of some reference type.
    InstanceOf SourceSpan (Exp p) RefType (Maybe Name)
  | -- | The conditional operator @? :@ uses the boolean value of one expression to decide which of two other
    --   expressions should be evaluated.
    Cond SourceSpan (Exp p) (Exp p) (Exp p)
  | -- | Assignment of the result of an expression to a variable.
    Assign SourceSpan (Lhs p) AssignOp (Exp p)
  | -- | Lambda expression
    Lambda SourceSpan (LambdaParams p) (LambdaExpression p)
  | -- | Method reference
    MethodRef SourceSpan Name MethodRefTarget
  | -- | New-style switch expression (JEP 361)
    SwitchExp SourceSpan (Exp p) (NonEmpty (SwitchExpBranch p))
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (Exp p)

deriving instance (ReadExtension p) => Read (Exp p)

deriving instance (DataExtension p) => Data (Exp p)

instance (EqualityExtension p) => Equality (Exp p) where
  eq opt (Lit l1) (Lit l2) =
    eq opt l1 l2
  eq opt (ClassLit s1 mt1) (ClassLit s2 mt2) =
    eq opt s1 s2 && eq opt mt1 mt2
  eq opt (This s1) (This s2) =
    eq opt s1 s2
  eq opt (ThisClass s1 n1) (ThisClass s2 n2) =
    eq opt s1 s2 && eq opt n1 n2
  eq opt (InstanceCreation s1 tas1 tds1 as1 mcb1) (InstanceCreation s2 tas2 tds2 as2 mcb2) =
    eq opt s1 s2 && eq opt tas1 tas2 && eq opt tds1 tds2 && eq opt as1 as2 && eq opt mcb1 mcb2
  eq opt (QualInstanceCreation s1 e1 tas1 i1 as1 mcb1) (QualInstanceCreation s2 e2 tas2 i2 as2 mcb2) =
    eq opt s1 s2 && eq opt e1 e2 && eq opt tas1 tas2 && eq opt i1 i2 && eq opt as1 as2 && eq opt mcb1 mcb2
  eq opt (ArrayCreate s1 t1 es1 int1) (ArrayCreate s2 t2 es2 int2) =
    eq opt s1 s2 && eq opt t1 t2 && eq opt es1 es2 && int1 == int2
  eq opt (ArrayCreateInit s1 t1 int1 ai1) (ArrayCreateInit s2 t2 int2 ai2) =
    eq opt s1 s2 && eq opt t1 t2 && eq opt ai1 ai2 && int1 == int2
  eq opt (FieldAccess fa1) (FieldAccess fa2) =
    eq opt fa1 fa2
  eq opt (MethodInv mi1) (MethodInv mi2) =
    eq opt mi1 mi2
  eq opt (ArrayAccess ai1) (ArrayAccess ai2) =
    eq opt ai1 ai2
  eq opt (ExpName n1) (ExpName n2) =
    eq opt n1 n2
  eq opt (PostIncrement s1 e1) (PostIncrement s2 e2) =
    eq opt s1 s2 && eq opt e1 e2
  eq opt (PostDecrement s1 e1) (PostDecrement s2 e2) =
    eq opt s1 s2 && eq opt e1 e2
  eq opt (PreIncrement s1 e1) (PreIncrement s2 e2) =
    eq opt s1 s2 && eq opt e1 e2
  eq opt (PreDecrement s1 e1) (PreDecrement s2 e2) =
    eq opt s1 s2 && eq opt e1 e2
  eq opt (PrePlus s1 e1) (PrePlus s2 e2) =
    eq opt s1 s2 && eq opt e1 e2
  eq opt (PreMinus s1 e1) (PreMinus s2 e2) =
    eq opt s1 s2 && eq opt e1 e2
  eq opt (PreBitCompl s1 e1) (PreBitCompl s2 e2) =
    eq opt s1 s2 && eq opt e1 e2
  eq opt (PreNot s1 e1) (PreNot s2 e2) =
    eq opt s1 s2 && eq opt e1 e2
  eq opt (Cast s1 t1 e1) (Cast s2 t2 e2) =
    eq opt s1 s2 && eq opt t1 t2 && eq opt e1 e2
  eq opt (BinOp s1 e11 o1 e12) (BinOp s2 e21 o2 e22) =
    eq opt s1 s2 && eq opt e11 e21 && eq opt o1 o2 && eq opt e12 e22
  eq opt (InstanceOf s1 e1 rt1 mn1) (InstanceOf s2 e2 rt2 mn2) =
    eq opt s1 s2 && eq opt e1 e2 && eq opt rt1 rt2 && eq opt mn1 mn2
  eq opt (Cond s1 e11 e12 e13) (Cond s2 e21 e22 e23) =
    eq opt s1 s2 && eq opt e11 e21 && eq opt e12 e22 && eq opt e13 e23
  eq opt (Assign s1 lhs1 ao1 e1) (Assign s2 lhs2 ao2 e2) =
    eq opt s1 s2 && eq opt lhs1 lhs2 && eq opt ao1 ao2 && eq opt e1 e2
  eq opt (Lambda s1 lp1 le1) (Lambda s2 lp2 le2) =
    eq opt s1 s2 && eq opt lp1 lp2 && eq opt le1 le2
  eq opt (MethodRef s1 n1 mrt1) (MethodRef s2 n2 mrt2) =
    eq opt s1 s2 && eq opt n1 n2 && eq opt mrt1 mrt2
  eq opt (SwitchExp s1 e1 sebs1) (SwitchExp s2 e2 sebs2) =
    eq opt s1 s2 && eq opt e1 e2 && eq opt sebs1 sebs2
  eq _ _ _ = False

instance (LocatedExtension p) => Located (Exp p) where
  sourceSpan (Lit l) = sourceSpan l
  sourceSpan (ClassLit s _) = s
  sourceSpan (This s) = s
  sourceSpan (ThisClass s _) = s
  sourceSpan (InstanceCreation s _ _ _ _) = s
  sourceSpan (QualInstanceCreation s _ _ _ _ _) = s
  sourceSpan (ArrayCreate s _ _ _) = s
  sourceSpan (ArrayCreateInit s _ _ _) = s
  sourceSpan (FieldAccess fa) = sourceSpan fa
  sourceSpan (MethodInv mi) = sourceSpan mi
  sourceSpan (ArrayAccess ai) = sourceSpan ai
  sourceSpan (ExpName n) = sourceSpan n
  sourceSpan (PostIncrement s _) = s
  sourceSpan (PostDecrement s _) = s
  sourceSpan (PreIncrement s _) = s
  sourceSpan (PreDecrement s _) = s
  sourceSpan (PrePlus s _) = s
  sourceSpan (PreMinus s _) = s
  sourceSpan (PreBitCompl s _) = s
  sourceSpan (PreNot s _) = s
  sourceSpan (Cast s _ _) = s
  sourceSpan (BinOp s _ _ _) = s
  sourceSpan (InstanceOf s _ _ _) = s
  sourceSpan (Cond s _ _ _) = s
  sourceSpan (Assign s _ _ _) = s
  sourceSpan (Lambda s _ _) = s
  sourceSpan (MethodRef s _ _) = s
  sourceSpan (SwitchExp s _ _) = s

-- | The left-hand side of an assignment expression. This operand may be a named variable, such as a local
--   variable or a field of the current object or class, or it may be a computed variable, as can result from
--   a field access or an array access.
data Lhs p
  = -- | Assign to a variable
    NameLhs Name
  | -- | Assign through a field access
    FieldLhs (FieldAccess p)
  | -- | Assign to an array
    ArrayLhs (ArrayIndex p)
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (Lhs p)

deriving instance (ReadExtension p) => Read (Lhs p)

deriving instance (DataExtension p) => Data (Lhs p)

instance (EqualityExtension p) => Equality (Lhs p) where
  eq opt (NameLhs n1) (NameLhs n2) =
    eq opt n1 n2
  eq opt (FieldLhs fa1) (FieldLhs fa2) =
    eq opt fa1 fa2
  eq opt (ArrayLhs ai1) (ArrayLhs ai2) =
    eq opt ai1 ai2
  eq _ _ _ = False

-- | Array access
data ArrayIndex p
  = -- | Index into an array
    ArrayIndex SourceSpan (Exp p) (NonEmpty (Exp p))
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (ArrayIndex p)

deriving instance (ReadExtension p) => Read (ArrayIndex p)

deriving instance (DataExtension p) => Data (ArrayIndex p)

instance (EqualityExtension p) => Equality (ArrayIndex p) where
  eq opt (ArrayIndex s1 e1 es1) (ArrayIndex s2 e2 es2) =
    eq opt s1 s2 && eq opt e1 e2 && eq opt es1 es2

instance Located (ArrayIndex p) where
  sourceSpan (ArrayIndex s _ _) = s

-- | A field access expression may access a field of an object or array, a reference to which is the value
--   of either an expression or the special keyword super.
data FieldAccess p
  = -- | Accessing a field of an object or array computed from an expression.
    PrimaryFieldAccess SourceSpan (Exp p) Ident
  | -- | Accessing a field of the superclass.
    SuperFieldAccess SourceSpan Ident
  | -- | Accessing a (static) field of a named class.
    ClassFieldAccess SourceSpan Name Ident
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (FieldAccess p)

deriving instance (ReadExtension p) => Read (FieldAccess p)

deriving instance (DataExtension p) => Data (FieldAccess p)

instance (EqualityExtension p) => Equality (FieldAccess p) where
  eq opt (PrimaryFieldAccess s1 e1 i1) (PrimaryFieldAccess s2 e2 i2) =
    eq opt s1 s2 && eq opt e1 e2 && eq opt i1 i2
  eq opt (SuperFieldAccess s1 i1) (SuperFieldAccess s2 i2) =
    eq opt s1 s2 && eq opt i1 i2
  eq opt (ClassFieldAccess s1 n1 i1) (ClassFieldAccess s2 n2 i2) =
    eq opt s1 s2 && eq opt n1 n2 && eq opt i1 i2
  eq _ _ _ = False

instance Located (FieldAccess p) where
  sourceSpan (PrimaryFieldAccess s _ _) = s
  sourceSpan (SuperFieldAccess s _) = s
  sourceSpan (ClassFieldAccess s _ _) = s

-- ¦ A lambda parameter can be a single parameter, or mulitple formal or mulitple inferred parameters
data LambdaParams p
  = LambdaSingleParam Ident
  | LambdaFormalParams [FormalParam p]
  | LambdaInferredParams [Ident]
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (LambdaParams p)

deriving instance (ReadExtension p) => Read (LambdaParams p)

deriving instance (DataExtension p) => Data (LambdaParams p)

instance (EqualityExtension p) => Equality (LambdaParams p) where
  eq opt (LambdaSingleParam i1) (LambdaSingleParam i2) =
    eq opt i1 i2
  eq opt (LambdaFormalParams fps1) (LambdaFormalParams fps2) =
    eq opt fps1 fps2
  eq opt (LambdaInferredParams is1) (LambdaInferredParams is2) =
    eq opt is1 is2
  eq _ _ _ = False

-- | Lambda expression, starting from java 8
data LambdaExpression p
  = LambdaExpression (Exp p)
  | LambdaBlock (Block p)
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (LambdaExpression p)

deriving instance (ReadExtension p) => Read (LambdaExpression p)

deriving instance (DataExtension p) => Data (LambdaExpression p)

type family XNameClassification x where
  XNameClassification Analyzed = ClassifiedName
  XNameClassification Parsed = Name

instance (EqualityExtension p) => Equality (LambdaExpression p) where
  eq opt (LambdaExpression e1) (LambdaExpression e2) =
    eq opt e1 e2
  eq opt (LambdaBlock b1) (LambdaBlock b2) =
    eq opt b1 b2
  eq _ _ _ = False

-- | A method invocation expression is used to invoke a class or instance method.
data MethodInvocation p
  = -- | Invoking a specific named method.
    MethodCall SourceSpan (Maybe (XNameClassification p)) Ident [Argument p]
  | -- | Invoking a method of a class computed from a primary expression, giving arguments for any generic type parameters.
    PrimaryMethodCall SourceSpan (Exp p) [RefType] Ident [Argument p]
  | -- | Invoking a method of the super class, giving arguments for any generic type parameters.
    SuperMethodCall SourceSpan [RefType] Ident [Argument p]
  | -- | Invoking a method of the superclass of a named class, giving arguments for any generic type parameters.
    ClassMethodCall SourceSpan Name [RefType] Ident [Argument p]
  | -- | Invoking a method of a named type, giving arguments for any generic type parameters.
    TypeMethodCall SourceSpan Name [RefType] Ident [Argument p]
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (MethodInvocation p)

deriving instance (ReadExtension p) => Read (MethodInvocation p)

deriving instance (DataExtension p) => Data (MethodInvocation p)

instance (EqualityExtension p) => Equality (MethodInvocation p) where
  eq opt (MethodCall s1 mn1 i1 as1) (MethodCall s2 mn2 i2 as2) =
    eq opt s1 s2 && eq opt mn1 mn2 && eq opt i1 i2 && eq opt as1 as2
  eq opt (PrimaryMethodCall s1 e1 rts1 i1 as1) (PrimaryMethodCall s2 e2 rts2 i2 as2) =
    eq opt s1 s2 && eq opt e1 e2 && eq opt rts1 rts2 && eq opt i1 i2 && eq opt as1 as2
  eq opt (SuperMethodCall s1 rts1 i1 as1) (SuperMethodCall s2 rts2 i2 as2) =
    eq opt s1 s2 && eq opt rts1 rts2 && eq opt i1 i2 && eq opt as1 as2
  eq opt (ClassMethodCall s1 n1 rts1 i1 as1) (ClassMethodCall s2 n2 rts2 i2 as2) =
    eq opt s1 s2 && eq opt n1 n2 && eq opt rts1 rts2 && eq opt i1 i2 && eq opt as1 as2
  eq opt (TypeMethodCall s1 n1 rts1 i1 as1) (TypeMethodCall s2 n2 rts2 i2 as2) =
    eq opt s1 s2 && eq opt n1 n2 && eq opt rts1 rts2 && eq opt i1 i2 && eq opt as1 as2
  eq _ _ _ = False

instance Located (MethodInvocation p) where
  sourceSpan (MethodCall s _ _ _) = s
  sourceSpan (PrimaryMethodCall s _ _ _ _) = s
  sourceSpan (SuperMethodCall s _ _ _) = s
  sourceSpan (ClassMethodCall s _ _ _ _) = s
  sourceSpan (TypeMethodCall s _ _ _ _) = s

-- | An array initializer may be specified in a declaration, or as part of an array creation expression, creating an
--   array and providing some initial values
data ArrayInit p
  = ArrayInit SourceSpan [VarInit p]
  deriving (Typeable, Generic)

deriving instance (ShowExtension p) => Show (ArrayInit p)

deriving instance (ReadExtension p) => Read (ArrayInit p)

deriving instance (DataExtension p) => Data (ArrayInit p)

instance (EqualityExtension p) => Equality (ArrayInit p) where
  eq opt (ArrayInit s1 vis1) (ArrayInit s2 vis2) =
    eq opt s1 s2 && eq opt vis1 vis2

instance Located (ArrayInit p) where
  sourceSpan (ArrayInit s _) = s

data MethodRefTarget
  = MethodRefIdent Ident
  | MethodRefConstructor
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality MethodRefTarget where
  eq opt (MethodRefIdent i1) (MethodRefIdent i2) =
    eq opt i1 i2
  eq _ MethodRefConstructor MethodRefConstructor = True
  eq _ _ _ = False

------------------------------------------------
-- Types

-- | There are two kinds of types in the Java programming language: primitive types and reference types.
data Type
  = PrimType PrimType
  | RefType RefType
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Type where
  eq opt (PrimType pt1) (PrimType pt2) =
    eq opt pt1 pt2
  eq opt (RefType rt1) (RefType rt2) =
    eq opt rt1 rt2
  eq _ _ _ = False

instance Located Type where
  sourceSpan (PrimType pt) = sourceSpan pt
  sourceSpan (RefType rt) = sourceSpan rt

-- | There are three kinds of reference types: class types, interface types, and array types.
--   Reference types may be parameterized with type arguments.
--   Type variables cannot be syntactically distinguished from class type identifiers,
--   and are thus represented uniformly as single ident class types.
data RefType
  = ClassRefType ClassType
  | -- | TypeVariable Ident
    ArrayType SourceSpan Type
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality RefType where
  eq opt (ClassRefType ct1) (ClassRefType ct2) =
    eq opt ct1 ct2
  eq opt (ArrayType s1 t1) (ArrayType s2 t2) =
    eq opt s1 s2 && eq opt t1 t2
  eq _ _ _ = False

instance Located RefType where
  sourceSpan (ClassRefType ct) = sourceSpan ct
  sourceSpan (ArrayType s _) = s

-- | A class or interface type consists of a type declaration specifier,
--   optionally followed by type arguments (in which case it is a parameterized type).
data ClassType
  = ClassType SourceSpan (NonEmpty (Ident, [TypeArgument]))
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality ClassType where
  eq opt (ClassType s1 ctss1) (ClassType s2 ctss2) =
    eq opt s1 s2 && eq opt ctss1 ctss2

instance Located ClassType where
  sourceSpan (ClassType s _) = s

-- | Type arguments may be either reference types or wildcards.
data TypeArgument
  = Wildcard (Maybe WildcardBound)
  | ActualType RefType
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality TypeArgument where
  eq opt (Wildcard mwb1) (Wildcard mwb2) =
    eq opt mwb1 mwb2
  eq opt (ActualType rt1) (ActualType rt2) =
    eq opt rt1 rt2
  eq _ _ _ = False

data TypeDeclSpecifier
  = TypeDeclSpecifier ClassType
  | TypeDeclSpecifierWithDiamond ClassType Ident Diamond
  | TypeDeclSpecifierUnqualifiedWithDiamond Ident Diamond
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality TypeDeclSpecifier where
  eq opt (TypeDeclSpecifier ct1) (TypeDeclSpecifier ct2) =
    eq opt ct1 ct2
  eq opt (TypeDeclSpecifierWithDiamond ct1 i1 d1) (TypeDeclSpecifierWithDiamond ct2 i2 d2) =
    eq opt ct1 ct2 && eq opt i1 i2 && eq opt d1 d2
  eq opt (TypeDeclSpecifierUnqualifiedWithDiamond i1 d1) (TypeDeclSpecifierUnqualifiedWithDiamond i2 d2) =
    eq opt i1 i2 && eq opt d1 d2
  eq _ _ _ = False

data Diamond = Diamond
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Diamond where
  eq _ _ _ = True

-- | Wildcards may be given explicit bounds, either upper (@extends@) or lower (@super@) bounds.
data WildcardBound
  = ExtendsBound RefType
  | SuperBound RefType
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality WildcardBound where
  eq opt (ExtendsBound rt1) (ExtendsBound rt2) =
    eq opt rt1 rt2
  eq opt (SuperBound rt1) (SuperBound rt2) =
    eq opt rt1 rt2
  eq _ _ _ = False

-- | A primitive type is predefined by the Java programming language and named by its reserved keyword.
data PrimType
  = BooleanT SourceSpan
  | ByteT SourceSpan
  | ShortT SourceSpan
  | IntT SourceSpan
  | LongT SourceSpan
  | CharT SourceSpan
  | FloatT SourceSpan
  | DoubleT SourceSpan
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality PrimType where
  eq opt (BooleanT s1) (BooleanT s2) = eq opt s1 s2
  eq opt (ByteT s1) (ByteT s2) = eq opt s1 s2
  eq opt (ShortT s1) (ShortT s2) = eq opt s1 s2
  eq opt (IntT s1) (IntT s2) = eq opt s1 s2
  eq opt (LongT s1) (LongT s2) = eq opt s1 s2
  eq opt (CharT s1) (CharT s2) = eq opt s1 s2
  eq opt (FloatT s1) (FloatT s2) = eq opt s1 s2
  eq opt (DoubleT s1) (DoubleT s2) = eq opt s1 s2
  eq _ _ _ = False

instance Located PrimType where
  sourceSpan (BooleanT s) = s
  sourceSpan (ByteT s) = s
  sourceSpan (ShortT s) = s
  sourceSpan (IntT s) = s
  sourceSpan (LongT s) = s
  sourceSpan (CharT s) = s
  sourceSpan (FloatT s) = s
  sourceSpan (DoubleT s) = s

-- | A literal denotes a fixed, unchanging value.
data Literal
  = Int SourceSpan Integer
  | Word SourceSpan Integer
  | Float SourceSpan Double
  | Double SourceSpan Double
  | Boolean SourceSpan Bool
  | Char SourceSpan Char
  | String SourceSpan String
  | Null SourceSpan
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Literal where
  eq opt (Int s1 n1) (Int s2 n2) =
    eq opt s1 s2 && n1 == n2
  eq opt (Word s1 n1) (Word s2 n2) =
    eq opt s1 s2 && n1 == n2
  eq opt (Float s1 x1) (Float s2 x2) =
    eq opt s1 s2 && x1 == x2
  eq opt (Double s1 x1) (Double s2 x2) =
    eq opt s1 s2 && x1 == x2
  eq opt (Boolean s1 b1) (Boolean s2 b2) =
    eq opt s1 s2 && b1 == b2
  eq opt (Char s1 c1) (Char s2 c2) =
    eq opt s1 s2 && c1 == c2
  eq opt (String s1 str1) (String s2 str2) =
    eq opt s1 s2 && str1 == str2
  eq opt (Null s1) (Null s2) =
    eq opt s1 s2
  eq _ _ _ = False

instance Located Literal where
  sourceSpan (Int s _) = s
  sourceSpan (Word s _) = s
  sourceSpan (Float s _) = s
  sourceSpan (Double s _) = s
  sourceSpan (Boolean s _) = s
  sourceSpan (Char s _) = s
  sourceSpan (String s _) = s
  sourceSpan (Null s) = s

-- | A class is generic if it declares one or more type variables. These type variables are known
--   as the type parameters of the class.
data TypeParam = TypeParam Ident [RefType]
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality TypeParam where
  eq opt (TypeParam i1 rts1) (TypeParam i2 rts2) =
    eq opt i1 i2 && eq opt rts1 rts2

-----------------------------------------------------------------------
-- Names and identifiers

-- | A single identifier.
data Ident = Ident SourceSpan String
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Ident where
  eq opt (Ident s1 str1) (Ident s2 str2) =
    eq opt s1 s2 && str1 == str2

instance Located Ident where
  sourceSpan (Ident s _) = s

-- | A name, i.e. a period-separated list of identifiers.
data Name = Name SourceSpan (NonEmpty Ident)
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Name where
  eq opt (Name s1 ids1) (Name s2 ids2) =
    eq opt s1 s2 && eq opt ids1 ids2

instance Located Name where
  sourceSpan (Name s _) = s

data ClassifiedExpressionName
  = -- a.b.c - LocalFieldAccess a [b,c]
    LocalFieldAccess SourceSpan Ident [Ident]
  | Other Name
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality ClassifiedExpressionName where
  eq opt (LocalFieldAccess s1 i1 is1) (LocalFieldAccess s2 i2 is2) =
    eq opt s1 s2 && eq opt i1 i2 && eq opt is1 is2
  eq opt (Other n1) (Other n2) =
    eq opt n1 n2
  eq _ _ _ = False

instance Located ClassifiedExpressionName where
  sourceSpan (LocalFieldAccess s _ _) = s
  sourceSpan (Other n) = sourceSpan n

data ClassifiedName
  = ExpressionName ClassifiedExpressionName
  | TypeName Name
  | Unknown Name
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality ClassifiedName where
  eq opt (ExpressionName n1) (ExpressionName n2) =
    eq opt n1 n2
  eq opt (TypeName n1) (TypeName n2) =
    eq opt n1 n2
  eq opt (Unknown n1) (Unknown n2) =
    eq opt n1 n2
  eq _ _ _ = False

instance Located ClassifiedName where
  sourceSpan (ExpressionName n) = sourceSpan n
  sourceSpan (TypeName n) = sourceSpan n
  sourceSpan (Unknown n) = sourceSpan n

----------------------------------------
-- Operators

-- | A binary infix operator.
data Op
  = Mult
  | Div
  | Rem
  | Add
  | Sub
  | LShift
  | RShift
  | RRShift
  | LThan
  | GThan
  | LThanE
  | GThanE
  | Equal
  | NotEq
  | And
  | Or
  | Xor
  | CAnd
  | COr
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality Op where
  eq _ Mult Mult = True
  eq _ Div Div = True
  eq _ Rem Rem = True
  eq _ Add Add = True
  eq _ Sub Sub = True
  eq _ LShift LShift = True
  eq _ RShift RShift = True
  eq _ RRShift RRShift = True
  eq _ LThan LThan = True
  eq _ GThan GThan = True
  eq _ LThanE LThanE = True
  eq _ GThanE GThanE = True
  eq _ Equal Equal = True
  eq _ NotEq NotEq = True
  eq _ And And = True
  eq _ Or Or = True
  eq _ Xor Xor = True
  eq _ CAnd CAnd = True
  eq _ COr COr = True
  eq _ _ _ = False

-- | An assignment operator.
data AssignOp
  = EqualA
  | MultA
  | DivA
  | RemA
  | AddA
  | SubA
  | LShiftA
  | RShiftA
  | RRShiftA
  | AndA
  | XorA
  | OrA
  deriving (Show, Read, Typeable, Generic, Data)

instance Equality AssignOp where
  eq _ EqualA EqualA = True
  eq _ MultA MultA = True
  eq _ DivA DivA = True
  eq _ RemA RemA = True
  eq _ AddA AddA = True
  eq _ SubA SubA = True
  eq _ LShiftA LShiftA = True
  eq _ RShiftA RShiftA = True
  eq _ RRShiftA RRShiftA = True
  eq _ AndA AndA = True
  eq _ XorA XorA = True
  eq _ OrA OrA = True
  eq _ _ _ = False
