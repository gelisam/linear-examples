{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs #-}
{-# OPTIONS -Wno-unused-top-binds #-}
module WatertightL
  ( ModelBuilding
  , Point, addPoint
  , Watertight3dModel, makeWatertight3dModel, renderWatertight3dModel
  ) where

import Control.Monad.Trans.State
import Data.Monoid
import qualified Data.Sequence as Seq

import Obj


newtype ModelBuilding a = PrivateModelBuilding
  { unModelBuilding :: State Obj a }
  deriving (Functor, Applicative, Monad)

newtype Point = PrivatePoint
  { unPoint :: Int }
  deriving Show

-- |
-- >>> :{
-- printObj . renderWatertight3dModel . makeWatertight3dModel $ do
--   _ <- addPoint (1,2,3)
--   _ <- addPoint (4,5,6)
--   _ <- addPoint (7,8,9)
--   pure ()
-- :}
-- v 1.0 2.0 3.0
-- v 4.0 5.0 6.0
-- v 7.0 8.0 9.0
newtype Watertight3dModel = PrivateWatertight3dModel
  { unWatertight3dModel :: Obj }
  deriving Show

addPoint :: Vertex -> ModelBuilding Point
addPoint v = PrivateModelBuilding $ do
  modify $ \obj -> obj { objVertices = objVertices obj <> Seq.singleton v }
  PrivatePoint . length . objVertices <$> get

makeWatertight3dModel :: ModelBuilding () -> Watertight3dModel
makeWatertight3dModel = PrivateWatertight3dModel
                      . flip execState (Obj mempty mempty)
                      . unModelBuilding

renderWatertight3dModel :: Watertight3dModel -> Obj
renderWatertight3dModel = unWatertight3dModel
