{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
    Parsed,
    Analyzed,
    XNameClassification,
  )
where

import Data.Data
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
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

class (Data x, Data (XNameClassification x)) => DataExtension x

instance DataExtension Parsed

instance DataExtension Analyzed

class (Eq (XNameClassification x)) => EqExtension x

instance EqExtension Parsed

instance EqExtension Analyzed

class (Located (XNameClassification x)) => LocatedExtension x

instance LocatedExtension Parsed

instance LocatedExtension Analyzed

-----------------------------------------------------------------------
-- Packages

-- | A compilation unit is the top level syntactic goal symbol of a Java program.
data CompilationUnit p = CompilationUnit (Maybe (PackageDecl p)) [ImportDecl] [TypeDecl p]
  deriving (Typeable, Generic, Show, Data, Eq)

-- | A package declaration appears within a compilation unit to indicate the package to which the compilation unit belongs.
data PackageDecl p = PackageDecl [Modifier p] Name
  deriving (Show, Typeable, Generic, Data, Eq)

-- | An import declaration allows a static member or a named type to be referred to by a single unqualified identifier.
--   The first argument signals whether the declaration only imports static members.
--   The last argument signals whether the declaration brings all names in the named type or package, or only brings
--   a single name into scope.
data ImportDecl
  = ImportDecl SourceSpan Bool {- static? -} Name Bool {- .*? -}
  deriving (Show, Typeable, Generic, Data)

instance Eq ImportDecl where
  ImportDecl _ b11 n1 b12 == ImportDecl _ b21 n2 b22 =
    b11 == b21 && n1 == n2 && b12 == b22

instance Located ImportDecl where
  sourceSpan (ImportDecl s _ _ _) = s

-----------------------------------------------------------------------
-- Declarations

-- | A type declaration declares a class type or an interface type.
data TypeDecl p
  = ClassTypeDecl (ClassDecl p)
  | InterfaceTypeDecl (InterfaceDecl p)
  deriving (Typeable, Generic, Show, Data, Eq)

instance Located (TypeDecl p) where
  sourceSpan (ClassTypeDecl cd) = sourceSpan cd
  sourceSpan (InterfaceTypeDecl itd) = sourceSpan itd

-- | A class declaration specifies a new named reference type.
data ClassDecl p
  = ClassDecl SourceSpan [Modifier p] Ident [TypeParam p] (Maybe RefType) [RefType] (ClassBody p)
  | RecordDecl SourceSpan [Modifier p] Ident [TypeParam p] [RecordFieldDecl] [RefType] (ClassBody p)
  | EnumDecl SourceSpan [Modifier p] Ident [RefType] (EnumBody p)
  deriving (Typeable, Generic, Show, Data)

instance (EqExtension p) => Eq (ClassDecl p) where
  ClassDecl _ ms1 i1 tps1 mrt1 rts1 cb1 == ClassDecl _ ms2 i2 tps2 mrt2 rts2 cb2 =
    ms1 == ms2 && i1 == i2 && tps1 == tps2 && mrt1 == mrt2 && rts1 == rts2 && cb1 == cb2
  RecordDecl _ ms1 i1 tps1 rfds1 rts1 cb1 == RecordDecl _ ms2 i2 tps2 rfds2 rts2 cb2 =
    ms1 == ms2 && i1 == i2 && tps1 == tps2 && rfds1 == rfds2 && rts1 == rts2 && cb1 == cb2
  EnumDecl _ ms1 i1 rts1 eb1 == EnumDecl _ ms2 i2 rts2 eb2 =
    ms1 == ms2 && i1 == i2 && rts1 == rts2 && eb1 == eb2
  _ == _ = False

instance Located (ClassDecl p) where
  sourceSpan (ClassDecl s _ _ _ _ _ _) = s
  sourceSpan (RecordDecl s _ _ _ _ _ _) = s
  sourceSpan (EnumDecl s _ _ _ _) = s

-- | A class body may contain declarations of members of the class, that is,
--   fields, classes, interfaces and methods.
--   A class body may also contain instance initializers, static
--   initializers, and declarations of constructors for the class.
newtype ClassBody p = ClassBody [Decl p]
  deriving (Typeable, Generic, Show, Data, Eq)

-- | The body of an enum type may contain enum constants.
data EnumBody p = EnumBody [EnumConstant p] [Decl p]
  deriving (Typeable, Generic, Show, Data, Eq)

-- | An enum constant defines an instance of the enum type.
data EnumConstant p = EnumConstant [Modifier p] Ident [Argument p] (Maybe (ClassBody p))
  deriving (Typeable, Generic, Show, Data, Eq)

-- | An interface declaration introduces a new reference type whose members
--   are classes, interfaces, constants and abstract methods. This type has
--   no implementation, but otherwise unrelated classes can implement it by
--   providing implementations for its abstract methods.
data InterfaceDecl p
  = InterfaceDecl SourceSpan InterfaceKind [Modifier p] Ident [TypeParam p] [RefType {- extends -}] [RefType {- permits -}] (InterfaceBody p)
  deriving (Typeable, Generic, Show, Data)

instance (EqExtension p) => Eq (InterfaceDecl p) where
  InterfaceDecl _ ik1 ms1 i1 tps1 rts11 rts12 ib1 == InterfaceDecl _ ik2 ms2 i2 tps2 rts21 rts22 ib2 =
    ik1 == ik2 && ms1 == ms2 && i1 == i2 && tps1 == tps2 && rts11 == rts21 && rts12 == rts22 && ib1 == ib2

instance Located (InterfaceDecl p) where
  sourceSpan (InterfaceDecl s _ _ _ _ _ _ _) = s

-- | Interface can declare either a normal interface or an annotation
data InterfaceKind = InterfaceNormal | InterfaceAnnotation
  deriving (Show, Typeable, Generic, Data, Eq)

-- | The body of an interface may declare members of the interface.
newtype InterfaceBody p
  = InterfaceBody [MemberDecl p]
  deriving (Typeable, Generic, Show, Data, Eq)

-- | A declaration is either a member declaration, or a declaration of an
--   initializer, which may be static.
data Decl p
  = MemberDecl (MemberDecl p)
  | InitDecl SourceSpan Bool (Block p)
  deriving (Typeable, Generic, Show, Data)

instance (EqExtension p) => Eq (Decl p) where
  MemberDecl md1 == MemberDecl md2 = md1 == md2
  InitDecl _ b1 bl1 == InitDecl _ b2 bl2 = b1 == b2 && bl1 == bl2
  _ == _ = False

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
    MethodDecl SourceSpan [Modifier p] [TypeParam p] (Maybe Type) Ident [FormalParam p] [ExceptionType] (Maybe (Exp p)) (MethodBody p)
  | -- | A constructor is used in the creation of an object that is an instance of a class.
    ConstructorDecl SourceSpan [Modifier p] [TypeParam p] Ident [FormalParam p] [ExceptionType] (ConstructorBody p)
  | -- | A member class is a class whose declaration is directly enclosed in another class or interface declaration.
    MemberClassDecl (ClassDecl p)
  | -- | A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
    MemberInterfaceDecl (InterfaceDecl p)
  deriving (Typeable, Generic, Show, Data)

instance (EqExtension p) => Eq (MemberDecl p) where
  FieldDecl _ ms1 t1 vds1 == FieldDecl _ ms2 t2 vds2 =
    ms1 == ms2 && t1 == t2 && vds1 == vds2
  MethodDecl _ ms1 tps1 mt1 i1 fps1 ets1 me1 mb1 == MethodDecl _ ms2 tps2 mt2 i2 fps2 ets2 me2 mb2 =
    ms1 == ms2 && tps1 == tps2 && mt1 == mt2 && i1 == i2 && fps1 == fps2 && ets1 == ets2 && me1 == me2 && mb1 == mb2
  ConstructorDecl _ ms1 tps1 i1 fps1 ets1 cb1 == ConstructorDecl _ ms2 tps2 i2 fps2 ets2 cb2 =
    ms1 == ms2 && tps1 == tps2 && i1 == i2 && fps1 == fps2 && ets1 == ets2 && cb1 == cb2
  MemberClassDecl cd1 == MemberClassDecl cd2 =
    cd1 == cd2
  MemberInterfaceDecl id1 == MemberInterfaceDecl id2 =
    id1 == id2
  _ == _ = False

instance Located (MemberDecl p) where
  sourceSpan (FieldDecl s _ _ _) = s
  sourceSpan (MethodDecl s _ _ _ _ _ _ _ _) = s
  sourceSpan (ConstructorDecl s _ _ _ _ _ _) = s
  sourceSpan (MemberClassDecl mcd) = sourceSpan mcd
  sourceSpan (MemberInterfaceDecl mid) = sourceSpan mid

-- | A field declaration of a record
data RecordFieldDecl
  = RecordFieldDecl Type Ident
  deriving (Show, Typeable, Generic, Data)

instance Eq RecordFieldDecl where
  RecordFieldDecl t1 i1 == RecordFieldDecl t2 i2 =
    t1 == t2 && i1 == i2

-- | A declaration of a variable, which may be explicitly initialized.
data VarDecl p
  = VarDecl SourceSpan VarDeclId (Maybe (VarInit p))
  deriving (Typeable, Generic, Show, Data)

instance (EqExtension p) => Eq (VarDecl p) where
  VarDecl _ vdi1 mvi1 == VarDecl _ vdi2 mvi2 =
    vdi1 == vdi2 && mvi1 == mvi2

instance Located (VarDecl p) where
  sourceSpan (VarDecl s _ _) = s

-- | The name of a variable in a declaration, which may be an array.
data VarDeclId
  = VarId Ident
  | -- | Multi-dimensional arrays are represented by nested applications of 'VarDeclArray'.
    VarDeclArray SourceSpan VarDeclId
  deriving (Show, Typeable, Generic, Data)

instance Eq VarDeclId where
  VarId i1 == VarId i2 = i1 == i2
  VarDeclArray _ vdi1 == VarDeclArray _ vdi2 = vdi1 == vdi2
  _ == _ = False

instance Located VarDeclId where
  sourceSpan (VarId i) = sourceSpan i
  sourceSpan (VarDeclArray s _) = s

-- | Explicit initializer for a variable declaration.
data VarInit p
  = InitExp (Exp p)
  | InitArray (ArrayInit p)
  deriving (Typeable, Generic, Show, Data, Eq)

instance (LocatedExtension p) => Located (VarInit p) where
  sourceSpan (InitExp e) = sourceSpan e
  sourceSpan (InitArray ai) = sourceSpan ai

-- | A formal parameter in method declaration. The last parameter
--   for a given declaration may be marked as variable arity,
--   indicated by the boolean argument.
data FormalParam p = FormalParam SourceSpan [Modifier p] Type Bool VarDeclId
  deriving (Typeable, Generic, Show, Data)

instance (EqExtension p) => Eq (FormalParam p) where
  FormalParam _ ms1 t1 b1 vdi1 == FormalParam _ ms2 t2 b2 vdi2 =
    ms1 == ms2 && t1 == t2 && b1 == b2 && vdi1 == vdi2

instance Located (FormalParam p) where
  sourceSpan (FormalParam s _ _ _ _) = s

-- | A formal parameter in catch clauses. Unlike formal parameters in
--   method declarations, multiple types can be specified.
data CatchFormalParam p = CatchFormalParam SourceSpan (NonEmpty ([Modifier p], Type)) VarDeclId
  deriving (Typeable, Generic, Show, Data)

instance (EqExtension p) => Eq (CatchFormalParam p) where
  CatchFormalParam _ t1 vdi1 == CatchFormalParam _ t2 vdi2 =
    t1 == t2 && vdi1 == vdi2

instance Located (CatchFormalParam p) where
  sourceSpan (CatchFormalParam s _ _) = s

-- | A method body is either a block of code that implements the method or simply a
--   semicolon, indicating the lack of an implementation (modelled by 'Nothing').
newtype MethodBody p = MethodBody (Maybe (Block p))
  deriving (Typeable, Generic, Show, Data, Eq)

-- | The first statement of a constructor body may be an explicit invocation of
--   another constructor of the same class or of the direct superclass.
data ConstructorBody p = ConstructorBody (Maybe (ExplConstrInv p)) [BlockStmt p]
  deriving (Typeable, Generic, Show, Data, Eq)

-- | An explicit constructor invocation invokes another constructor of the
--   same class, or a constructor of the direct superclass, which may
--   be qualified to explicitly specify the newly created object's immediately
--   enclosing instance.
data ExplConstrInv p
  = ThisInvoke [RefType] [Argument p]
  | SuperInvoke [RefType] [Argument p]
  | PrimarySuperInvoke (Exp p) [RefType] [Argument p]
  deriving (Typeable, Generic, Show, Data, Eq)

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
  deriving (Typeable, Generic, Show, Data)

instance (EqExtension p) => Eq (Modifier p) where
  Public _ == Public _ = True
  Private _ == Private _ = True
  Protected _ == Protected _ = True
  Abstract _ == Abstract _ = True
  Final _ == Final _ = True
  Static _ == Static _ = True
  StrictFP _ == StrictFP _ = True
  Transient _ == Transient _ = True
  Volatile _ == Volatile _ = True
  Native _ == Native _ = True
  Annotation a1 == Annotation a2 = a1 == a2
  Synchronized_ _ == Synchronized_ _ = True
  Sealed _ == Sealed _ = True
  _ == _ = False

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
  deriving (Typeable, Generic, Show, Data)

instance (EqExtension p) => Eq (Annotation p) where
  NormalAnnotation _ n1 ievs1 == NormalAnnotation _ n2 ievs2 =
    n1 == n2 && ievs1 == ievs2
  SingleElementAnnotation _ n1 ev1 == SingleElementAnnotation _ n2 ev2 =
    n1 == n2 && ev1 == ev2
  MarkerAnnotation _ n1 == MarkerAnnotation _ n2 =
    n1 == n2
  _ == _ = False

instance Located (Annotation p) where
  sourceSpan (NormalAnnotation s _ _) = s
  sourceSpan (SingleElementAnnotation s _ _) = s
  sourceSpan (MarkerAnnotation s _) = s

-- | Annotations may contain  annotations or (loosely) expressions
data ElementValue p
  = EVVal (VarInit p)
  | EVAnn (Annotation p)
  deriving (Typeable, Generic, Show, Data, Eq)

instance (LocatedExtension p) => Located (ElementValue p) where
  sourceSpan (EVVal vi) = sourceSpan vi
  sourceSpan (EVAnn ann) = sourceSpan ann

-----------------------------------------------------------------------
-- Statements

-- | A block is a sequence of statements, local class declarations
--   and local variable declaration statements within braces.
data Block p = Block SourceSpan [BlockStmt p]
  deriving (Typeable, Generic, Show, Data)

instance (EqExtension p) => Eq (Block p) where
  Block _ bss1 == Block _ bss2 = bss1 == bss2

instance Located (Block p) where
  sourceSpan (Block s _) = s

-- | A block statement is either a normal statement, a local
--   class declaration or a local variable declaration.
data BlockStmt p
  = BlockStmt (Stmt p)
  | LocalClass (ClassDecl p)
  | LocalVars SourceSpan [Modifier p] Type (NonEmpty (VarDecl p))
  deriving (Typeable, Generic, Show, Data)

instance (EqExtension p) => Eq (BlockStmt p) where
  BlockStmt stmt1 == BlockStmt stmt2 =
    stmt1 == stmt2
  LocalClass cd1 == LocalClass cd2 =
    cd1 == cd2
  LocalVars _ ms1 t1 vds1 == LocalVars _ ms2 t2 vds2 =
    ms1 == ms2 && t1 == t2 && vds1 == vds2
  _ == _ = False

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
  deriving (Generic, Typeable, Show, Data)

instance (EqExtension p) => Eq (Stmt p) where
  StmtBlock b1 == StmtBlock b2 = b1 == b2
  IfThen _ e1 stmt1 == IfThen _ e2 stmt2 = e1 == e2 && stmt1 == stmt2
  IfThenElse _ e1 stmt11 stmt12 == IfThenElse _ e2 stmt21 stmt22 =
    e1 == e2 && stmt11 == stmt21 && stmt12 == stmt22
  While _ e1 stmt1 == While _ e2 stmt2 =
    e1 == e2 && stmt1 == stmt2
  BasicFor _ mfi1 me1 mes1 stmt1 == BasicFor _ mfi2 me2 mes2 stmt2 =
    mfi1 == mfi2 && me1 == me2 && mes1 == mes2 && stmt1 == stmt2
  EnhancedFor _ ms1 t1 i1 e1 stmt1 == EnhancedFor _ ms2 t2 i2 e2 stmt2 =
    ms1 == ms2 && t1 == t2 && i1 == i2 && e1 == e2 && stmt1 == stmt2
  Empty _ == Empty _ = True
  ExpStmt _ e1 == ExpStmt _ e2 = e1 == e2
  Assert _ e1 me1 == Assert _ e2 me2 = e1 == e2 && me1 == me2
  Switch _ ss1 e1 sbs1 == Switch _ ss2 e2 sbs2 =
    ss1 == ss2 && e1 == e2 && sbs1 == sbs2
  Do _ stmt1 e1 == Do _ stmt2 e2 = stmt1 == stmt2 && e1 == e2
  Break _ mi1 == Break _ mi2 = mi1 == mi2
  Continue _ mi1 == Continue _ mi2 = mi1 == mi2
  Return _ me1 == Return _ me2 = me1 == me2
  Synchronized _ e1 b1 == Synchronized _ e2 b2 = e1 == e2 && b1 == b2
  Throw _ e1 == Throw _ e2 = e1 == e2
  Try _ trs1 b1 cs1 mb1 == Try _ trs2 b2 cs2 mb2 =
    trs1 == trs2 && b1 == b2 && cs1 == cs2 && mb1 == mb2
  Labeled _ i1 stmt1 == Labeled _ i2 stmt2 = i1 == i2 && stmt1 == stmt2
  _ == _ = False

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
  deriving (Typeable, Generic, Show, Data, Eq)

data TryResource p
  = TryResourceVarDecl (ResourceDecl p)
  | TryResourceVarAccess Ident
  | TryResourceQualAccess (FieldAccess p)
  deriving (Typeable, Generic, Show, Data, Eq)

data ResourceDecl p
  = ResourceDecl [Modifier p] Type VarDeclId (VarInit p)
  deriving (Typeable, Generic, Show, Data, Eq)

-- | A block of code labelled with a @case@ or @default@ within a @switch@ statement.
data SwitchBlock p
  = SwitchBlock SourceSpan (SwitchLabel p) [BlockStmt p]
  deriving (Typeable, Generic, Show, Data)

instance (EqExtension p) => Eq (SwitchBlock p) where
  SwitchBlock _ sl1 bss1 == SwitchBlock _ sl2 bss2 =
    sl1 == sl2 && bss1 == bss2

instance Located (SwitchBlock p) where
  sourceSpan (SwitchBlock s _ _) = s

data SwitchStyle
  = SwitchOldStyle
  | SwitchNewStyle -- JEP 361
  deriving (Show, Typeable, Generic, Data, Eq)

-- | A label within a @switch@ statement.
data SwitchLabel p
  = -- | The expressions contained in the @case@ must be a 'Lit' or an @enum@ constant.
    -- The list must be non-empty.
    SwitchCase (NonEmpty (Exp p))
  | Default
  deriving (Typeable, Generic, Show, Data, Eq)

data SwitchExpBranch p
  = SwitchExpBranch (SwitchLabel p) (SwitchExpBranchBody p)
  deriving (Typeable, Generic, Show, Data, Eq)

data SwitchExpBranchBody p
  = SwitchExpBranchExp (Exp p)
  | SwitchExpBranchBlock [BlockStmt p]
  deriving (Typeable, Generic, Show, Data, Eq)

-- | Initialization code for a basic @for@ statement.
data ForInit p
  = ForLocalVars [Modifier p] Type (NonEmpty (VarDecl p))
  | ForInitExps (NonEmpty (Exp p))
  deriving (Generic, Typeable, Show, Data, Eq)

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
  deriving (Typeable, Generic, Show, Data)

instance (EqExtension p) => Eq (Exp p) where
  Lit l1 == Lit l2 = l1 == l2
  ClassLit _ mt1 == ClassLit _ mt2 = mt1 == mt2
  This _ == This _ = True
  ThisClass _ n1 == ThisClass _ n2 = n1 == n2
  InstanceCreation _ tas1 tds1 as1 mcb1 == InstanceCreation _ tas2 tds2 as2 mcb2 =
    tas1 == tas2 && tds1 == tds2 && as1 == as2 && mcb1 == mcb2
  QualInstanceCreation _ e1 tas1 i1 as1 mcb1 == QualInstanceCreation _ e2 tas2 i2 as2 mcb2 =
    e1 == e2 && tas1 == tas2 && i1 == i2 && as1 == as2 && mcb1 == mcb2
  ArrayCreate _ t1 es1 int1 == ArrayCreate _ t2 es2 int2 =
    t1 == t2 && es1 == es2 && int1 == int2
  ArrayCreateInit _ t1 int1 ai1 == ArrayCreateInit _ t2 int2 ai2 =
    t1 == t2 && ai1 == ai2 && int1 == int2
  FieldAccess fa1 == FieldAccess fa2 = fa1 == fa2
  MethodInv mi1 == MethodInv mi2 = mi1 == mi2
  ArrayAccess ai1 == ArrayAccess ai2 = ai1 == ai2
  ExpName n1 == ExpName n2 = n1 == n2
  PostIncrement _ e1 == PostIncrement _ e2 = e1 == e2
  PostDecrement _ e1 == PostDecrement _ e2 = e1 == e2
  PreIncrement _ e1 == PreIncrement _ e2 = e1 == e2
  PreDecrement _ e1 == PreDecrement _ e2 = e1 == e2
  PrePlus _ e1 == PrePlus _ e2 = e1 == e2
  PreMinus _ e1 == PreMinus _ e2 = e1 == e2
  PreBitCompl _ e1 == PreBitCompl _ e2 = e1 == e2
  PreNot _ e1 == PreNot _ e2 = e1 == e2
  Cast _ t1 e1 == Cast _ t2 e2 = t1 == t2 && e1 == e2
  BinOp _ e11 o1 e12 == BinOp _ e21 o2 e22 =
    e11 == e21 && o1 == o2 && e12 == e22
  InstanceOf _ e1 rt1 mn1 == InstanceOf _ e2 rt2 mn2 =
    e1 == e2 && rt1 == rt2 && mn1 == mn2
  Cond _ e11 e12 e13 == Cond _ e21 e22 e23 =
    e11 == e21 && e12 == e22 && e13 == e23
  Assign _ lhs1 ao1 e1 == Assign _ lhs2 ao2 e2 =
    lhs1 == lhs2 && ao1 == ao2 && e1 == e2
  Lambda _ lp1 le1 == Lambda _ lp2 le2 =
    lp1 == lp2 && le1 == le2
  MethodRef _ n1 mrt1 == MethodRef _ n2 mrt2 =
    n1 == n2 && mrt1 == mrt2
  SwitchExp _ e1 sebs1 == SwitchExp _ e2 sebs2 =
    e1 == e2 && sebs1 == sebs2
  _ == _ = False

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
  deriving (Typeable, Generic, Show, Data, Eq)

-- | Array access
data ArrayIndex p
  = -- | Index into an array
    ArrayIndex SourceSpan (Exp p) (NonEmpty (Exp p))
  deriving (Typeable, Generic, Show, Data)

instance (EqExtension p) => Eq (ArrayIndex p) where
  ArrayIndex _ e1 es1 == ArrayIndex _ e2 es2 = e1 == e2 && es1 == es2

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
  deriving (Typeable, Generic, Show, Data)

instance (EqExtension p) => Eq (FieldAccess p) where
  PrimaryFieldAccess _ e1 i1 == PrimaryFieldAccess _ e2 i2 =
    e1 == e2 && i1 == i2
  SuperFieldAccess _ i1 == SuperFieldAccess _ i2 =
    i1 == i2
  ClassFieldAccess _ n1 i1 == ClassFieldAccess _ n2 i2 =
    n1 == n2 && i1 == i2
  _ == _ = False

instance Located (FieldAccess p) where
  sourceSpan (PrimaryFieldAccess s _ _) = s
  sourceSpan (SuperFieldAccess s _) = s
  sourceSpan (ClassFieldAccess s _ _) = s

-- ¦ A lambda parameter can be a single parameter, or mulitple formal or mulitple inferred parameters
data LambdaParams p
  = LambdaSingleParam Ident
  | LambdaFormalParams [FormalParam p]
  | LambdaInferredParams [Ident]
  deriving (Typeable, Generic, Show, Data, Eq)

-- | Lambda expression, starting from java 8
data LambdaExpression p
  = LambdaExpression (Exp p)
  | LambdaBlock (Block p)
  deriving (Typeable, Generic, Show, Data, Eq)

type family XNameClassification x where
  XNameClassification Analyzed = ClassifiedName
  XNameClassification Parsed = Name

-- | A method invocation expression is used to invoke a class or instance method.
data MethodInvocation p
  = -- | Invoking a specific named method, giving arguments for any generic type parameters.
    MethodCall SourceSpan (Maybe (XNameClassification p)) [RefType] Ident [Argument p]
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

deriving instance (DataExtension p) => Data (MethodInvocation p)

instance (EqExtension p) => Eq (MethodInvocation p) where
  MethodCall _ mn1 rts1 i1 as1 == MethodCall _ mn2 rts2 i2 as2 =
    mn1 == mn2 && rts1 == rts2 && i1 == i2 && as1 == as2
  PrimaryMethodCall _ e1 rts1 i1 as1 == PrimaryMethodCall _ e2 rts2 i2 as2 =
    e1 == e2 && rts1 == rts2 && i1 == i2 && as1 == as2
  SuperMethodCall _ rts1 i1 as1 == SuperMethodCall _ rts2 i2 as2 =
    rts1 == rts2 && i1 == i2 && as1 == as2
  ClassMethodCall _ n1 rts1 i1 as1 == ClassMethodCall _ n2 rts2 i2 as2 =
    n1 == n2 && rts1 == rts2 && i1 == i2 && as1 == as2
  TypeMethodCall _ n1 rts1 i1 as1 == TypeMethodCall _ n2 rts2 i2 as2 =
    n1 == n2 && rts1 == rts2 && i1 == i2 && as1 == as2
  _ == _ = False

instance Located (MethodInvocation p) where
  sourceSpan (MethodCall s _ _ _ _) = s
  sourceSpan (PrimaryMethodCall s _ _ _ _) = s
  sourceSpan (SuperMethodCall s _ _ _) = s
  sourceSpan (ClassMethodCall s _ _ _ _) = s
  sourceSpan (TypeMethodCall s _ _ _ _) = s

-- | An array initializer may be specified in a declaration, or as part of an array creation expression, creating an
--   array and providing some initial values
data ArrayInit p
  = ArrayInit SourceSpan [VarInit p]
  deriving (Typeable, Generic, Show, Data)

instance (EqExtension p) => Eq (ArrayInit p) where
  ArrayInit _ vis1 == ArrayInit _ vis2 = vis1 == vis2

instance Located (ArrayInit p) where
  sourceSpan (ArrayInit s _) = s

data MethodRefTarget
  = MethodRefIdent Ident
  | MethodRefConstructor
  deriving (Show, Typeable, Generic, Data, Eq)

------------------------------------------------
-- Types

-- | There are two kinds of types in the Java programming language: primitive types and reference types.
data Type
  = PrimType PrimType
  | RefType RefType
  deriving (Show, Typeable, Generic, Data, Eq)

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
  deriving (Show, Typeable, Generic, Data)

instance Eq RefType where
  ClassRefType ct1 == ClassRefType ct2 = ct1 == ct2
  ArrayType _ t1 == ArrayType _ t2 = t1 == t2
  _ == _ = False

instance Located RefType where
  sourceSpan (ClassRefType ct) = sourceSpan ct
  sourceSpan (ArrayType s _) = s

-- | A class or interface type consists of a type declaration specifier,
--   optionally followed by type arguments (in which case it is a parameterized type).
data ClassType
  = ClassType SourceSpan (NonEmpty (Ident, [TypeArgument]))
  deriving (Show, Typeable, Generic, Data)

instance Eq ClassType where
  ClassType _ ctss1 == ClassType _ ctss2 = ctss1 == ctss2

instance Located ClassType where
  sourceSpan (ClassType s _) = s

-- | Type arguments may be either reference types or wildcards.
data TypeArgument
  = Wildcard (Maybe WildcardBound)
  | ActualType RefType
  deriving (Show, Typeable, Generic, Data, Eq)

data TypeDeclSpecifier
  = TypeDeclSpecifier ClassType
  | TypeDeclSpecifierWithDiamond ClassType Ident
  | TypeDeclSpecifierUnqualifiedWithDiamond Ident
  deriving (Show, Typeable, Generic, Data, Eq)

-- | Wildcards may be given explicit bounds, either upper (@extends@) or lower (@super@) bounds.
data WildcardBound
  = ExtendsBound RefType
  | SuperBound RefType
  deriving (Show, Typeable, Generic, Data, Eq)

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
  deriving (Show, Typeable, Generic, Data)

instance Eq PrimType where
  BooleanT _ == BooleanT _ = True
  ByteT _ == ByteT _ = True
  ShortT _ == ShortT _ = True
  IntT _ == IntT _ = True
  LongT _ == LongT _ = True
  CharT _ == CharT _ = True
  FloatT _ == FloatT _ = True
  DoubleT _ == DoubleT _ = True
  _ == _ = False

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
  deriving (Show, Typeable, Generic, Data)

instance Eq Literal where
  Int _ n1 == Int _ n2 = n1 == n2
  Word _ n1 == Word _ n2 = n1 == n2
  Float _ x1 == Float _ x2 = x1 == x2
  Double _ x1 == Double _ x2 = x1 == x2
  Boolean _ b1 == Boolean _ b2 = b1 == b2
  Char _ c1 == Char _ c2 = c1 == c2
  String _ str1 == String _ str2 = str1 == str2
  Null _ == Null _ = True
  _ == _ = False

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
data TypeParam p = TypeParam [Modifier p] Ident [RefType]
  deriving (Show, Typeable, Generic, Data, Eq)

-----------------------------------------------------------------------
-- Names and identifiers

-- | A single identifier.
data Ident = Ident SourceSpan String
  deriving (Show, Typeable, Generic, Data)

instance Eq Ident where
  Ident _ str1 == Ident _ str2 = str1 == str2

instance Located Ident where
  sourceSpan (Ident s _) = s

-- | A name, i.e. a period-separated list of identifiers.
data Name = Name SourceSpan (NonEmpty Ident)
  deriving (Show, Typeable, Generic, Data)

instance Eq Name where
  Name _ ids1 == Name _ ids2 = ids1 == ids2

instance Located Name where
  sourceSpan (Name s _) = s

data ClassifiedExpressionName
  = -- a.b.c - LocalFieldAccess a [b,c]
    LocalFieldAccess SourceSpan Ident [Ident]
  | Other Name
  deriving (Show, Typeable, Generic, Data)

instance Eq ClassifiedExpressionName where
  LocalFieldAccess _ i1 is1 == LocalFieldAccess _ i2 is2 = i1 == i2 && is1 == is2
  Other n1 == Other n2 = n1 == n2
  _ == _ = False

instance Located ClassifiedExpressionName where
  sourceSpan (LocalFieldAccess s _ _) = s
  sourceSpan (Other n) = sourceSpan n

data ClassifiedName
  = ExpressionName ClassifiedExpressionName
  | TypeName Name
  | Unknown Name
  deriving (Show, Typeable, Generic, Data, Eq)

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
  deriving (Show, Typeable, Generic, Data, Eq)

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
  deriving (Show, Typeable, Generic, Data, Eq)
