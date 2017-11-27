-- |
-- Unlike Objs, Watertight3dModels are guaranteed to be watertight, that is, all
-- edges are bound by exactly two faces, so there cannot be any hole in the
-- surface.
--
-- This is accomplished by guaranteeing that each edge is used exactly twice,
-- which is in turn accomplished by generating two co-edges at a time and using
-- linear types to make sure that each one is used by exactly one face. So the
-- following compiles:
--
-- >>> :set -XRebindableSyntax
-- >>> :{
-- printObj . renderWatertight3dModel . makeWatertight3dModel $ do
--   Unrestricted pA <- addPoint (1,2,3)
--   Unrestricted pB <- addPoint (4,5,6)
--   Unrestricted pC <- addPoint (7,8,9)
--   (coedgeAB, coedgeBA) <- addCoEdges pA pB
--   (coedgeBC, coedgeCB) <- addCoEdges pB pC
--   (coedgeCA, coedgeAC) <- addCoEdges pC pA
--   addFace [coedgeAB, coedgeBC, coedgeCA]
--   addFace [coedgeAC, coedgeCB, coedgeBA]
-- :}
-- v 1.0 2.0 3.0
-- v 4.0 5.0 6.0
-- v 7.0 8.0 9.0
-- f 1 2 3
-- f 1 3 2
--
-- While the following does not:
--
-- >>> :set -XRebindableSyntax
-- >>> :{
-- printObj . renderWatertight3dModel . makeWatertight3dModel $ do
--   Unrestricted pA <- addPoint (1,2,3)
--   Unrestricted pB <- addPoint (4,5,6)
--   Unrestricted pC <- addPoint (7,8,9)
--   (coedgeAB, coedgeBA) <- addCoEdges pA pB
--   (coedgeBC, coedgeCB) <- addCoEdges pB pC
--   (coedgeCA, coedgeAC) <- addCoEdges pC pA
--   addFace [coedgeAB, coedgeBC, coedgeCA]
-- :}
-- ...
-- ...Couldn't match expected weight ‘1’ of variable ‘coedgeBA’ with actual weight ‘0’
-- ...
{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, RebindableSyntax #-}
module WatertightL
  ( ModelBuilding
  , Point, addPoint
  , CoEdge, addCoEdges
  , addFace
  , Watertight3dModel, makeWatertight3dModel, renderWatertight3dModel
  ) where

import Prelude hiding ((>>), (>>=))
import Data.Monoid
import qualified Data.Sequence as Seq

import DataL
import NatL
import Obj
import PreludeL
import PreludeL.RebindableSyntax
import StateL


newtype ModelBuilding a = PrivateModelBuilding
  { unModelBuilding :: StateL Obj a }
  deriving (FunctorL, ApplicativeL, MonadL)

newtype Point = PrivatePoint
  { unPoint :: Nat }
  deriving (DataL, Eq, Show)

data CoEdge = PrivateCoEdge
  { coEdgePoint1 :: Point
  , coEdgePoint2 :: Point
  }
  deriving Show

newtype Watertight3dModel = PrivateWatertight3dModel
  { unWatertight3dModel :: Obj }
  deriving Show

addPoint :: Vertex -> ModelBuilding (Unrestricted Point)
addPoint v = PrivateModelBuilding $ do
  modifyL $ \obj -> obj { objVertices = objVertices obj <> Seq.singleton v }
  Unrestricted . PrivatePoint
               . natLength
               . objVertices
               . getUnrestricted
            <$>. getL

addCoEdges :: Point -> Point -> ModelBuilding (CoEdge, CoEdge)
addCoEdges p1 p2 = pureL (PrivateCoEdge p1 p2, PrivateCoEdge p2 p1)

-- no DataL instance because we can't make it private to this module
unrestrictCoEdge :: CoEdge ->. Unrestricted CoEdge
unrestrictCoEdge (PrivateCoEdge x y) = unrestrict x &. \(Unrestricted x')
                                    -> unrestrict y &. \(Unrestricted y')
                                    -> Unrestricted (PrivateCoEdge x' y')

addFace :: [CoEdge] ->. ModelBuilding ()
addFace coedges
    = unrestrictList unrestrictCoEdge coedges &. \(Unrestricted coedges')
   -> PrivateModelBuilding $ do
        let points1 = map coEdgePoint1 coedges'
            points2 = map coEdgePoint2 coedges'
            offsetPoints1 = take (length points1) . drop 1 . cycle $ points1
            face = map (natToInt . unPoint) points1
        case offsetPoints1 == points2 of
          True ->
            modifyL $ \obj -> obj
                    { objFaces = objFaces obj <> Seq.singleton face }
          False ->
            error $ "consecutive coedge points should match: " ++ show (points1, points2, offsetPoints1, face)

makeWatertight3dModel :: ModelBuilding () -> Watertight3dModel
makeWatertight3dModel = PrivateWatertight3dModel
                      . flip execStateL (Obj mempty mempty)
                      . unModelBuilding

renderWatertight3dModel :: Watertight3dModel -> Obj
renderWatertight3dModel = unWatertight3dModel
