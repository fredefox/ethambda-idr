{-# Language GADTs
  , LambdaCase
  , FlexibleContexts
  , ConstraintKinds
  , StandaloneDeriving
#-}
module Position where

import Ethambda.System
import Ethambda.Position (position)

-- "a"
-- pos: a
a :: Type Int
a = Var 0

-- "b"
-- pos: b
b :: Type Int
b = Var 1

-- "a -> a"
-- neg: a; pos: a
i :: Type Int
i = Fun a a

-- "a -> b -> a"
-- neg: a, b; pos: a
k :: Type Int
k = a `Fun` (b `Fun` a)

-- "i -> b"
-- uninhabited
void :: Type Int
void = Fun i b

-- "b -> i"
point :: Type Int
point = Fun b i

c :: Type Int
c = Var 2

-- "void -> c"
f0 :: Type Int
f0 = Fun void c

-- "c -> void"
f1 :: Type Int
f1 = Fun c void

allTs :: [Type Int]
allTs = [ a, b, i, k, void, point, c, f0, f1 ]

main :: IO ()
main = mapM_ go allTs
  where
  go t = do
    print t
    print $ position t
    putStrLn ""
