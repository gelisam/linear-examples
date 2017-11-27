# linear-examples

The goal is to write a few example libraries using [tweag's implementation of linear types for GHC](https://github.com/tweag/ghc/tree/linear-types#readme), in order to fuel the [discussion page about the linear types proposal](https://github.com/ghc-proposals/ghc-proposals/pull/91#issuecomment-346721678).

So far, there is only one example, a Haskell implementation of [my blog post about watertight 3D models](https://www.spiria.com/en/blog/desktop-software/making-non-manifold-models-unrepresentable). In a nutshell, the goal is to prevent holes in 3D surfaces by making sure that every edge is bound by exactly two faces. This is accomplished by guaranteeing that each edge is used exactly twice, which is in turn accomplished by generating two co-edges at a time and using linear types to make sure that each one is used by exactly one face. So the following compiles:

    makeWatertight3dModel $ do
      Unrestricted pA <- addPoint (1,2,3)
      Unrestricted pB <- addPoint (4,5,6)
      Unrestricted pC <- addPoint (7,8,9)
      (coedgeAB, coedgeBA) <- addCoEdges pA pB
      (coedgeBC, coedgeCB) <- addCoEdges pB pC
      (coedgeCA, coedgeAC) <- addCoEdges pC pA
      addFace [coedgeAB, coedgeBC, coedgeCA]
      addFace [coedgeAC, coedgeCB, coedgeBA]

But if the last line is commented out, the compiler complains that many co-edges are never used, which is exactly what we want since that means the corresponding edges are bordering a hole in the surface.
