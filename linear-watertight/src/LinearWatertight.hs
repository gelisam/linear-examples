module LinearWatertight where

import Test.DocTest


-- |
-- >>> :{
-- let linear :: (a, b) ->. (b, a)
--     linear (x, y) = (y, x)
-- :}
--
-- >>> :{
-- let nonlinear :: (a, b) ->. (a, a)
--     nonlinear (x, _) = (x, x)
-- :}
-- ...
-- ...error...
-- ...
runTests :: IO ()
runTests = doctest [ "linear-watertight/src/LinearWatertight.hs"
                   , "linear-watertight/src/Obj.hs"
                   ]
