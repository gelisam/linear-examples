-- Types which consist only of data, no functions, can be consumed by
-- pattern-matching down to their leaves and reconstructed unrestricted.
{-# LANGUAGE LinearTypes, ScopedTypeVariables #-}
module DataL where

import PreludeL


class DataL a where
  unrestrict :: a #-> Unrestricted a

skip :: DataL a => a -> ()
skip x = unrestrict x &. \(Unrestricted _)
      -> ()

dup :: DataL a => a #-> (a, a)
dup x = unrestrict x &. \(Unrestricted x')
     -> (x', x')


instance DataL () where
  unrestrict () = Unrestricted ()

instance DataL (Unrestricted a) where
  unrestrict (Unrestricted x) = Unrestricted (Unrestricted x)


unrestrictPair :: (a #-> Unrestricted a)
               -> (b #-> Unrestricted b)
               -> (a, b) #-> Unrestricted (a, b)
unrestrictPair unrestrictX unrestrictY (x, y)
    = unrestrictX x &. \(Unrestricted x')
   -> unrestrictY y &. \(Unrestricted y')
   -> Unrestricted (x', y')

instance (DataL a, DataL b) => DataL (a, b) where
  unrestrict = unrestrictPair unrestrict unrestrict


unrestrictEither :: (a #-> Unrestricted a)
                 -> (b #-> Unrestricted b)
                 -> Either a b #-> Unrestricted (Either a b)
unrestrictEither unrestrictX _ (Left  x) = unrestrictX x &. \(Unrestricted x')
                                        -> Unrestricted (Left  x')
unrestrictEither _ unrestrictY (Right y) = unrestrictY y &. \(Unrestricted y')
                                        -> Unrestricted (Right y')

instance (DataL a, DataL b) => DataL (Either a b) where
  unrestrict = unrestrictEither unrestrict unrestrict


unrestrictList :: forall a
                . (a #-> Unrestricted a)
               -> [a] #-> Unrestricted [a]
unrestrictList unrestrictX = go
  where
    go :: [a] #-> Unrestricted [a]
    go []     = Unrestricted []
    go (x:xs) = unrestrictX x &. \(Unrestricted x')
             -> go xs         &. \(Unrestricted xs')
             -> Unrestricted (x':xs')

instance DataL a => DataL [a] where
  unrestrict = unrestrictList unrestrict
