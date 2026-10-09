module Language.Java.SourceSpan
  ( Located (..),
    SourceSpan,
    dummyPos,
    dummySourceSpan,
    posEof,
    isEof,
  )
where

import Text.Parsec.Pos (SourcePos, initialPos, newPos)

type SourceSpan = (SourcePos, SourcePos)

dummyPos :: SourcePos
dummyPos = initialPos "<input>"

dummySourceSpan :: SourceSpan
dummySourceSpan = (dummyPos, dummyPos)

posEof :: SourcePos
posEof = newPos "" 0 0

isEof :: SourcePos -> Bool
isEof pos = pos == posEof

class Located a where
  sourceSpan :: a -> SourceSpan
