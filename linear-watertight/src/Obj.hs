module Obj where

import Data.Foldable
import Data.Sequence (Seq)
import Text.Printf

-- $setup
-- >>> import qualified Data.Sequence as Seq


-- the standard .obj format for 3D meshes,
-- does not guarantee that the model is watertight
type Vertex = (Float, Float, Float)
type Face = [Int]
data Obj = Obj
  { objVertices :: Seq Vertex
  , objFaces    :: Seq Face
  }
  deriving Show

-- |
-- >>> printObj $ Obj (Seq.fromList [(1,2,3),(4,5,6),(7,8,9)]) (Seq.fromList [[1,2,3]])
-- v 1.0 2.0 3.0
-- v 4.0 5.0 6.0
-- v 7.0 8.0 9.0
-- f 1 2 3
printObj :: Obj -> IO ()
printObj (Obj vertices faces) = do
  for_ vertices $ \(x,y,z) -> do
    printf "v %f %f %f\n" x y z
  for_ faces $ \indices -> do
    printf "f"
    for_ indices $ \index -> do
      printf " %d" index
    printf "\n"
