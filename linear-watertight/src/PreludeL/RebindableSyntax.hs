module PreludeL.RebindableSyntax where

import Prelude hiding ((>>=))
import PreludeL


(>>=) :: MonadL m => m a ->. (a ->. m b) ->. m b
(>>=) = (>>=.)