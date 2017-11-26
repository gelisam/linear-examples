{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, RebindableSyntax #-}
{-# OPTIONS -Wno-unused-top-binds #-}
module WatertightL
  ( ModelBuilding
  , Point, addPoint
  , Watertight3dModel, makeWatertight3dModel, renderWatertight3dModel
  ) where

import Prelude hiding ((>>), (>>=))
import Data.Monoid
import qualified Data.Sequence as Seq

import Obj
import PreludeL
import PreludeL.RebindableSyntax
import StateL


newtype ModelBuilding a = PrivateModelBuilding
  { unModelBuilding :: StateL Obj a }
  deriving (FunctorL, ApplicativeL, MonadL)

newtype Point = PrivatePoint
  { unPoint :: Int }
  deriving Show

-- |
-- >>> :set -XRebindableSyntax
-- >>> :{
-- printObj . renderWatertight3dModel . makeWatertight3dModel $ do
--   Unrestricted _ <- addPoint (1,2,3)
--   Unrestricted _ <- addPoint (4,5,6)
--   Unrestricted _ <- addPoint (7,8,9)
--   pureL ()
-- :}
-- v 1.0 2.0 3.0
-- v 4.0 5.0 6.0
-- v 7.0 8.0 9.0
newtype Watertight3dModel = PrivateWatertight3dModel
  { unWatertight3dModel :: Obj }
  deriving Show

addPoint :: Vertex -> ModelBuilding (Unrestricted Point)
addPoint v = PrivateModelBuilding $ do
  modifyL $ \obj -> obj { objVertices = objVertices obj <> Seq.singleton v }
  Unrestricted . PrivatePoint
               . length
               . objVertices
               . getUnrestricted
            <$>. getL

makeWatertight3dModel :: ModelBuilding () -> Watertight3dModel
makeWatertight3dModel = PrivateWatertight3dModel
                      . flip execStateL (Obj mempty mempty)
                      . unModelBuilding

renderWatertight3dModel :: Watertight3dModel -> Obj
renderWatertight3dModel = unWatertight3dModel
