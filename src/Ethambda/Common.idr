module Ethambda.Common

import Eth.Prelude

infixr 3 <.
infixr 3 <.>

export
(<.>) : String -> String -> String
v <.> w =
  if isNullStr v
  then v
  else v ++ " " ++ " " ++ w

export
ShowS : Type
ShowS = String -> String

export
(<.) : String -> ShowS -> ShowS
s <. c = (s <.>) . c

  
