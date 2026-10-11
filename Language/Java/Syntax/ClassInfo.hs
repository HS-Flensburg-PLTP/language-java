module Language.Java.Syntax.ClassInfo
  ( ClassInfo (..),
    hasField,
    hasIdent,
    fromClassDecl,
  )
where

import Data.Maybe (mapMaybe)
import Language.Java.Syntax
import Language.Java.Syntax.Decl as Decl (classDecl, fieldIdents)

-- | data type used to pass down icClassInfos structure in the file
-- Vielleicht classInfo oder javaClass {ciFields: :, ciInnerClasses}
data ClassInfo = ClassInfo
  { ciFields :: [Ident],
    ciInnerClasses :: [ClassInfo],
    ciIdent :: Ident
  }
  deriving (Show)

hasField :: Ident -> ClassInfo -> Bool
hasField idnt classInfo = idnt `elem` ciFields classInfo

hasIdent :: Ident -> ClassInfo -> Bool
hasIdent idnt classInfo = idnt == ciIdent classInfo

fromClassDecl :: ClassDecl p -> ClassInfo
fromClassDecl (ClassDecl _ _ idnt _ _ _ (ClassBody decls)) =
  ClassInfo
    ((concat . mapMaybe Decl.fieldIdents) decls)
    (map fromClassDecl (mapMaybe Decl.classDecl decls))
    idnt
fromClassDecl (RecordDecl _ _ idnt _ _ _ (ClassBody decls)) =
  ClassInfo
    ((concat . mapMaybe Decl.fieldIdents) decls)
    (map fromClassDecl (mapMaybe Decl.classDecl decls))
    idnt
fromClassDecl (EnumDecl _ _ idnt _ (EnumBody cons decls)) =
  ClassInfo
    ((concat . mapMaybe Decl.fieldIdents) decls ++ map (\(EnumConstant _ idnt' _ _) -> idnt') cons)
    (map fromClassDecl (mapMaybe Decl.classDecl decls))
    idnt
