-- | Parser combinators for working with 'NonEmpty' lists.
module Text.Parsec.NonEmpty
  ( optional,
    some,
    sepBy,
    trySepBy,
  )
where

import Control.Applicative (Alternative (many))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Text.Parsec (ParsecT, try)
import Text.Parsec.Extra (optionMap)

-- | @NonEmpty.optional p@ tries to apply parser @p@.
-- If @p@ fails without consuming input, it returns an empty list,
-- otherwise it returns 'Data.List.NonEmpty.toList' the value returned by @p@.
optional :: (Alternative f) => f (NonEmpty a) -> f [a]
optional = optionMap [] NonEmpty.toList

-- | @NonEmpty.some p@ applies the parser @p@ /one/ or more times.
-- Returns a 'NonEmpty' list of the returned values of @p@.
some :: (Alternative f) => f a -> f (NonEmpty a)
some p = liftA2 (:|) p (many p)

-- | @NonEmpty.sepBy p sep@ parses /one/ or more occurrences of @p@, separated by @sep@.
-- Returns a 'NonEmpty' list of values returned by @p@.
sepBy :: (Alternative f) => f a -> f sep -> f (NonEmpty a)
sepBy p sep = liftA2 (:|) p (many $ sep *> p)

-- | @NonEmpty.trySepBy p sep@ parses /one/ or more occurrences of @p@, separated by @sep@.
-- Returns a 'NonEmpty' list of values returned by @p@.
-- Does /not/ fail if @sep@ or @p@ fails after consuming input.
trySepBy :: ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (NonEmpty a)
trySepBy p sep = liftA2 (:|) p $ many $ try $ sep >> p
