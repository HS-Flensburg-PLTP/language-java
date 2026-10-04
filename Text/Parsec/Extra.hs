module Text.Parsec.Extra
  ( manyMaybes,
    optionMap,
    optionConst,
    optionBool,
  )
where

import Data.Maybe (catMaybes)
import Text.Parsec (ParsecT, Stream, many, option)

-- | @manyMaybes p@ applies the parser @p@ /zero/ or more times.
-- Returns a list of the returned 'Just' values of @p@.
manyMaybes :: ParsecT s u m (Maybe a) -> ParsecT s u m [a]
manyMaybes = fmap catMaybes . many

-- | @optionMap x f p@ tries to apply parser @p@.
-- If @p@ fails without consuming input, it returns @x@,
-- otherwise it returns @f@ the value returned by @p@.
optionMap :: (Stream s m t) => b -> (a -> b) -> ParsecT s u m a -> ParsecT s u m b
optionMap a b p = option a (b <$> p)

-- | @optionConst a b p@ tries to apply parser @p@.
-- If @p@ fails without consuming input, it returns @a@,
-- otherwise it returns @b@.
optionConst :: (Stream s m t) => b -> b -> ParsecT s u m a -> ParsecT s u m b
optionConst a b = optionMap a (const b)

-- | @optionBool p@ tries to apply parser @p@.
-- If @p@ fails without consuming input, it returns 'False',
-- otherwise it returns 'True'.
optionBool :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m Bool
optionBool = optionConst False True
