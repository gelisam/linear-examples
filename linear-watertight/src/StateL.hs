-- |
-- A variant of the State monad in which every bound variable
-- must be used (or returned) exactly once. So this compiles:
--
-- >>> :set -XRebindableSyntax
-- >>> import PreludeL.RebindableSyntax
-- >>> :{
-- let linear :: StateL s a -> (a ->. StateL s ()) -> StateL s a
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
-- let linear :: StateL s a -> (a ->. StateL s ()) -> StateL s a
--     linear gen consume = do
--       consumed <- gen
--       _notConsumed <- gen
--       returned <- gen
--       () <- consume consumed
--       pureL returned
-- :}
-- ...
-- ...Couldn't match expected weight ‘1’ of variable ‘_notConsumed’ with actual weight ‘0’
-- ...
{-# LANGUAGE InstanceSigs, ScopedTypeVariables #-}
module StateL where

import Prelude hiding ((>>=))
import PreludeL


data StateL s a = StateL
  { unStateL :: s ->. (s, a) }

instance FunctorL (StateL s) where
  fmapL f (StateL body) = StateL $ \s
                       -> body s &. \(s', x)
                       -> (s', f x)

instance ApplicativeL (StateL s) where
  pureL x = StateL $ \s -> (s, x)
  StateL bodyF <*>. StateL bodyX = StateL $ \s
                                -> bodyF s  &. \(s', f)
                                -> bodyX s' &. \(s'', x)
                                -> (s'', f x)


instance MonadL (StateL s) where
  StateL bodyX >>=. cc = StateL $ \s
                      -> bodyX s  &. \(s', x)
                      -> cc x &. \(StateL bodyY)
                      -> bodyY s'

getL :: StateL (Unrestricted s) (Unrestricted s)
getL = StateL $ \(Unrestricted s)
    -> (Unrestricted s, Unrestricted s)

modifyL :: (s ->. s) -> StateL s ()
modifyL body = StateL $ \s
            -> (body s, ())

execStateL :: StateL s () ->. s ->. s
execStateL (StateL body) s = body s &. \(s', ())
                          -> s'
