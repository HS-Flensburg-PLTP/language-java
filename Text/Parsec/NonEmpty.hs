-- | Parser combinators for working with 'NonEmpty' lists.
module Text.Parsec.NonEmpty
  ( neoptList,
    manyNonEmpty,
    sepByNonEmpty,
    trySepByNonEmpty,
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Text.Parsec (ParsecT, Stream, many, try)
import Text.Parsec.Extra (optionMap)

-- | @neoptList p@ tries to apply parser @p@.
-- If @p@ fails without consuming input, it returns an empty list,
-- otherwise it returns 'Data.List.NonEmpty.toList' the value returned by @p@.
neoptList :: (Stream s m t) => ParsecT s u m (NonEmpty a) -> ParsecT s u m [a]
neoptList = optionMap [] NonEmpty.toList

-- | @manyNonEmpty p@ applies the parser @p@ /one/ or more times.
-- Returns a 'NonEmpty' list of the returned values of @p@.
manyNonEmpty :: ParsecT s u m a -> ParsecT s u m (NonEmpty a)
manyNonEmpty p = liftA2 (:|) p (many p)

-- | @sepByNonEmpty p sep@ parses /one/ or more occurrences of @p@, separated by @sep@.
-- Returns a 'NonEmpty' list of values returned by @p@.
sepByNonEmpty :: ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (NonEmpty a)
sepByNonEmpty p sep = liftA2 (:|) p (many $ sep >> p)

-- | @trySepByNonEmpty p sep@ parses /one/ or more occurrences of @p@, separated by @sep@.
-- Returns a 'NonEmpty' list of values returned by @p@.
-- Does /not/ fail if @sep@ or @p@ fails after consuming input.
trySepByNonEmpty :: ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (NonEmpty a)
trySepByNonEmpty p sep = liftA2 (:|) p $ many $ try $ sep >> p
