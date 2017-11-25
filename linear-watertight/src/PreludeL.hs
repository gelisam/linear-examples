{-# LANGUAGE GADTs #-}
module PreludeL where

import Prelude hiding ((>>=))


-- |
-- Marks variables which can be consumed zero or multiple times, even within
-- a linear computation. So this compiles:
--
-- >>> :{
-- let f :: (a, Unrestricted b) ->. (a, b, b)
--     f (x, Unrestricted y) = (x, y, y)
-- :}
--
-- But this does not:
--
-- >>> :{
-- let f :: (a, b) ->. (a, b, b)
--     f (x, y) = (x, y, y)
-- :}
-- ...
-- ...Couldn't match expected weight ‘1’ of variable ‘y’ with actual weight ‘ω’
-- ...
data Unrestricted a where
  Unrestricted :: a -> Unrestricted a

getUnrestricted :: Unrestricted a ->. a
getUnrestricted (Unrestricted x) = x


-- |
-- let doesn't yet preserve multiplicity:
--
-- >>> :{
-- let f :: a ->. a
--     f x = let x' = x in x'
-- :}
-- ...
-- ...weight...
-- ...
--
-- so I need a lambda to pattern-match instead:
--
-- >>> :{
-- let f :: a ->. a
--     f x = x &. \x' -> x'
-- :}
--
infixl 1 &.
(&.) :: a ->. (a ->. b) ->. b
x &. f = f x


-- |
-- A variant of Functor in which the output must be used (or returned)
-- exactly once. So this compiles:
--
-- >>> :{
-- let linear :: FunctorL f => f (a, b) -> f (b, a)
--     linear = fmapL (\(x, y) -> (y, x))
-- :}
--
-- But this does not:
--
-- >>> :{
-- let notLinear :: FunctorL f => f (a, b) -> f (a, a)
--     notLinear = fmapL (\(x, y) -> (x, x))
-- :}
-- ...
-- ...Couldn't match expected weight ‘1’ of variable ‘x’ with actual weight ‘ω’
-- ...
class FunctorL f where
  fmapL :: (a ->. b) -> f a ->. f b

infixl 4 <$>.
(<$>.) :: FunctorL f => (a ->. b) -> f a ->. f b
(<$>.) = fmapL

-- |
-- A variant of Applicative in which every output must be used (or returned)
-- exactly once. So this compiles:
--
-- >>> :{
-- let linear :: ApplicativeL f => f a -> f b -> f (a, b)
--     linear fx fy = (\x y -> (x, y)) <$>. fx <*>. fy
-- :}
--
-- But this does not:
--
-- >>> :{
-- let notLinear :: ApplicativeL f => f a -> f b -> f (a, a)
--     notLinear fx fy = (\x y -> (x, x)) <$>. fx <*>. fy
-- :}
-- ...
-- ...Couldn't match expected weight ‘1’ of variable ‘x’ with actual weight ‘ω’
-- ...
infixl 4 <*>.
class FunctorL f => ApplicativeL f where
  pureL  :: a ->. f a
  (<*>.) :: f (a ->. b) ->. f a ->. f b

-- |
-- A variant of Monad in which every bound variable must be used (or returned)
-- exactly once. So this compiles:
--
-- >>> :set -XRebindableSyntax
-- >>> import PreludeL.RebindableSyntax
-- >>> :{
-- let linear :: MonadL m => m a -> (a ->. m ()) -> m a
--     linear gen consume = do
--       consumed <- gen
--       returned <- gen
--       () <- consume consumed
--       pureL returned
-- :}
--
-- But this does not:
--
-- >>> :{
-- let notLinear :: MonadL m => m a -> (a ->. m ()) -> m a
--     notLinear gen consume = do
--       consumed <- gen
--       _notConsumed <- gen
--       returned <- gen
--       () <- consume consumed
--       pureL returned
-- :}
-- ...
-- ...Couldn't match expected weight ‘1’ of variable ‘_notConsumed’ with actual weight ‘0’
-- ...
class ApplicativeL m => MonadL m where
  (>>=.) :: m a ->. (a ->. m b) ->. m b
