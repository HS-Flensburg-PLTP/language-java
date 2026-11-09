module Text.Parsec.Extra
  ( manyMaybes,
    optionMap,
    optionConst,
    optionBool,
  )
where

import Control.Applicative (Alternative (..))
import Data.Maybe (catMaybes)

-- | @manyMaybes p@ applies the parser @p@ /zero/ or more times.
-- Returns a list of the returned 'Just' values of @p@.
manyMaybes :: (Alternative f) => f (Maybe a) -> f [a]
manyMaybes = fmap catMaybes . many

-- | @optionMap x f p@ tries to apply parser @p@.
-- If @p@ fails without consuming input, it returns @x@,
-- otherwise it returns @f@ the value returned by @p@.
optionMap :: (Alternative f) => b -> (a -> b) -> f a -> f b
optionMap a b p = b <$> p <|> pure a

-- | @optionConst a b p@ tries to apply parser @p@.
-- If @p@ fails without consuming input, it returns @a@,
-- otherwise it returns @b@.
optionConst :: (Alternative f) => b -> b -> f a -> f b
optionConst a b = optionMap a (const b)

-- | @optionBool p@ tries to apply parser @p@.
-- If @p@ fails without consuming input, it returns 'False',
-- otherwise it returns 'True'.
optionBool :: (Alternative f) => f a -> f Bool
optionBool = optionConst False True
