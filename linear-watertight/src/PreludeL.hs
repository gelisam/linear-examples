{-# LANGUAGE GADTs #-}
module PreludeL where


data Unrestricted a where
  Unrestricted :: a -> Unrestricted a

getUnrestricted :: Unrestricted a ->. a
getUnrestricted (Unrestricted x) = x


-- let doesn't yet preserve multiplicity, so I need a lambda to pattern-match.
infixl 1 &.
(&.) :: a ->. (a ->. b) ->. b
x &. f = f x


class FunctorL f where
  fmapL :: (a ->. b) -> f a ->. f b

infixl 4 <$>.
(<$>.) :: FunctorL f => (a ->. b) -> f a ->. f b
(<$>.) = fmapL

infixl 4 <*>.
class FunctorL f => ApplicativeL f where
  pureL  :: a ->. f a
  (<*>.) :: f (a ->. b) ->. f a ->. f b

class ApplicativeL m => MonadL m where
  (>>=.) :: m a ->. (a ->. m b) ->. m b
