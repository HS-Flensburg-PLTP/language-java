module Language.Java.Syntax.FormalParam where

import Language.Java.Syntax (CatchFormalParam (CatchFormalParam), FormalParam (FormalParam), Ident)
import qualified Language.Java.Syntax.VarDeclId as VarDeclId

ident :: FormalParam p -> Ident
ident (FormalParam _ _ _ _ vardeclid) = VarDeclId.ident vardeclid

catchIdent :: CatchFormalParam p -> Ident
catchIdent (CatchFormalParam _ _ vardeclid) = VarDeclId.ident vardeclid
