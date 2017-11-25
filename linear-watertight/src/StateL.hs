-- a variant of the State monad in which every bound variable
-- must be used exactly once
{-# LANGUAGE InstanceSigs, ScopedTypeVariables #-}
module StateL where

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
