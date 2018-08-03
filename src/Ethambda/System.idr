-- | The core type system.
module Ethambda.System

import Ethambda.Common using ((<.>), (<.))

public export
data Tp : Type -> Type where
  Var : a -> Tp a
  Fun : Tp a -> Tp a -> Tp a

Show a => Show (Tp a) where
  show t = case t of
    -- Var a => ?foo
    Var a => show a
    Fun a0 b0 => mbrackets a0 <+> "→" <+> show b0
  where
    brackets : String -> String
    brackets s = "(" <+> s <+> ")"
    mbrackets : Show a => Tp a -> String
    mbrackets a = case a of
      Var _   => neutral
      Fun _ _ => brackets (show a)
