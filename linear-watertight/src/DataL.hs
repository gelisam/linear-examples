-- Types which consist only of data, no functions, can be consumed by
-- pattern-matching down to their leaves and reconstructed unrestricted.
module DataL where

import PreludeL


class DataL a where
  unrestrict :: a ->. Unrestricted a

skip :: DataL a => a -> ()
skip x = unrestrict x &. \(Unrestricted _)
      -> ()

dup :: DataL a => a ->. (a, a)
dup x = unrestrict x &. \(Unrestricted x')
     -> (x', x')


instance DataL () where
  unrestrict () = Unrestricted ()

instance (DataL a, DataL b) => DataL (a, b) where
  unrestrict (x, y) = unrestrict x &. \(Unrestricted x')
                   -> unrestrict y &. \(Unrestricted y')
                   -> Unrestricted (x', y')

instance (DataL a, DataL b) => DataL (Either a b) where
  unrestrict (Left  x) = unrestrict x &. \(Unrestricted x')
                      -> Unrestricted (Left  x')
  unrestrict (Right x) = unrestrict x &. \(Unrestricted x')
                      -> Unrestricted (Right x')
