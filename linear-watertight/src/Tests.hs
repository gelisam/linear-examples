module Tests where

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
-- ...Couldn't match expected weight ‘1’ of variable ‘x’ with actual weight ‘ω’
-- ...
runTests :: IO ()
runTests = doctest [ "linear-watertight/src/DataL.hs"
                   , "linear-watertight/src/NatL.hs"
                   , "linear-watertight/src/Obj.hs"
                   , "linear-watertight/src/PreludeL.hs"
                   , "linear-watertight/src/PreludeL/RebindableSyntax.hs"
                   , "linear-watertight/src/StateL.hs"
                   , "linear-watertight/src/Tests.hs"
                   , "linear-watertight/src/WatertightL.hs"
                   ]
