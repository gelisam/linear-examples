-- |
-- A variant of the State monad which has a MonadL instance instead. Note that
-- it is only the bound variables which must be used linearly, the state itself
-- is unrestricted. After all, it is possible to call `getL` multiple times to
-- get multiple copies of the state:
--
-- >>> :set -XRebindableSyntax
-- >>> import PreludeL.RebindableSyntax
-- >>> :{
-- let dup1 = do
--       s1 <- getL
--       s2 <- getL
--       pureL (s1, s2)
-- :}
--
-- For this reason `getL` returns an unrestricted value, so you only need to
-- call `getL` once.
--
-- >>> :{
-- let dup2 = do
--       Unrestricted s <- getL
--       pureL (s, s)
-- :}
{-# LANGUAGE InstanceSigs, ScopedTypeVariables #-}
module StateL where

import Prelude hiding ((>>), (>>=))
import PreludeL


data StateL s a = StateL
  { unStateL :: Unrestricted s ->. (Unrestricted s, a) }

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

getL :: StateL s (Unrestricted s)
getL = StateL $ \(Unrestricted s)
    -> (Unrestricted s, Unrestricted s)

modifyL :: (s -> s) -> StateL s ()
modifyL body = StateL $ \(Unrestricted s)
            -> (Unrestricted (body s), ())

execStateL :: StateL s () ->. s -> s
execStateL (StateL body) s = body (Unrestricted s) &. \(Unrestricted s', ())
                          -> s'
