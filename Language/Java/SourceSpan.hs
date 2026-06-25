module Language.Java.SourceSpan
  ( Located (..),
    SourceSpan,
    dummyLocation,
    dummySourceSpan,
    locationEof,
    isEof,
  )
where

import Text.Parsec.Pos (SourcePos, initialPos, newPos)

type SourceSpan = (SourcePos, SourcePos)

dummyLocation :: SourcePos
dummyLocation = initialPos "<input>"

dummySourceSpan :: SourceSpan
dummySourceSpan = (dummyLocation, dummyLocation)

locationEof :: SourcePos
locationEof = newPos "" 0 0

isEof :: SourcePos -> Bool
isEof loc = loc == locationEof

class Located a where
  sourceSpan :: a -> SourceSpan
