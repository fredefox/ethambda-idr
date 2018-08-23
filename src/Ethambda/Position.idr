module Ethambda.Position

import Eth.Prelude -- using (bool)
import Prelude.Bool -- using (bool)
import Data.List -- using (intercalate)
import Prelude.Monad -- using ((>=>))
import Control.Monad.Identity
import Control.Monad.Writer -- using
--  (MonadWriter, WriterT, Writer, execWriterT, execWriter, tell)
import Control.Monad.Reader -- using
--  (MonadReader, ReaderT, Reader, runReaderT, runReader, ask, local)

import Ethambda.Common -- using ((<.>), (<.))
import Ethambda.System -- using (Type(Var, Fun))


-- * The 'PosNeg' type.

record PosNeg a where
  constructor PN
  negatives : List a
  positives : List a

stuff : Show a => String -> List a -> String
stuff ttl l = case l of
  [] => neutral
  _  => ttl <> ": " <> unwords (intercalate (pure ",") (map (pure . show) l))

Show a => Show (PosNeg a) where
  show (PN neg pos)
    =   "PN"
    <.> "neg"
    <.> "pos"

Semigroup (PosNeg a) where
  (PN p n) <+> (PN p' n') = PN (p <> p') (n <> n')

Monoid (PosNeg a) where
  neutral = PN neutral neutral

pos : a -> PosNeg a
pos a = PN neutral (pure a)

neg : a -> PosNeg a
neg a = PN (pure a) neutral


-- * Core algorithm.
AppM : Type -> (Type -> Type) -> Type
AppM a m =
  ( MonadWriter (PosNeg a) m
  , MonadReader Bool m
  )

App : Type -> Type -> Type
App a x = WriterT (PosNeg a) (Reader Bool) x

-- implementation Functor (ReaderT ?a ?m) where


-- execApp : App a x -> PosNeg a
execApp : WriterT (PosNeg a) (ReaderT Bool Identity) x -> PosNeg a
execApp = (`runReader` True) . execWriterT

-- tellPos : AppM a m => a -> m ()
tellPos : MonadWriter (PosNeg a) m => a -> m ()
-- tellPos = tell . pos
tellPos = tell . pos

-- tellNeg : AppM a m => a -> m ()
-- tellNeg = tell . neg
tellNeg : MonadWriter (PosNeg a) m => a -> m ()
tellNeg = tell . neg

-- tellIt : AppM a m => a -> m ()
-- tellIt a = ask >>= bool (tellNeg a) (tellPos a)
tellIt
  : MonadWriter (PosNeg a) m
  => MonadReader Bool m
  => a -> m ()
tellIt a = ask >>= bool (tellNeg a) (tellPos a)

-- positionM : AppM a m => Type a -> m ()
positionM
  : MonadWriter (PosNeg a) n
  => MonadReader Bool m
  => Tp a -> m ()
positionM tp = case tp of
  Var a => tellIt a
  Fun t u => do
    positionM u
    local not $ positionM t

-- export
-- position : Type a -> PosNeg a
-- position = execApp . positionM
