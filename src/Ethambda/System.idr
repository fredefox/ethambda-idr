-- | The core type system.
{-# Language GADTs
  , LambdaCase
  , FlexibleContexts
  , ConstraintKinds
  , StandaloneDeriving
#-}
module Ethambda.System (Type(Var,Fun)) where

import Ethambda.Common ((<.>), (<.))

data Type a where
  Var :: a -> Type a
  Fun :: Type a -> Type a -> Type a

instance Show a => Show (Type a) where
  show = \case
    Var a -> show a
    Fun a b -> mbrackets (show a) <.> "->" <.> show b
      where
      mbrackets = case a of
        Var{} -> id
        Fun{} -> brackets
      brackets s = "(" <> s <> ")"
