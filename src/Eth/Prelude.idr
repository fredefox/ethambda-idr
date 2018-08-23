{-| A custom prelude -}
module Eth.Prelude

import Prelude
import Prelude.Bool -- using (bool)
import Data.List -- using (intercalate)
import Prelude.Monad -- using ((>=>))
import public Control.Monad.Identity -- using
import Control.Monad.Writer -- using
--  (MonadWriter, WriterT, Writer, execWriterT, execWriter, tell)
import Control.Monad.Reader -- using
--  (MonadReader, ReaderT, Reader, runReaderT, runReader, ask, local)

infixl 6 <>

export
isNullStr : String -> Bool
isNullStr = (== neutral)

export
(<>) : Monoid m => m -> m -> m
(<>) = (<+>)

export
bool : a -> a -> Bool -> a
bool x y b = case b of
  True => x
  False => y
 
export
execWriterT : Functor m => WriterT w m a -> m w
execWriterT = map snd . runWriterT

-- works : WriterT w (ReaderT r Identity) _ -> ReaderT r Identity w
-- works = execWriterT @{ }

doesnt : WriterT w (Reader r) _ -> Reader r w
doesnt = execWriterT
  -- using ReaderTFunctor


export
execWriter : Writer w a -> w
execWriter = runIdentity . execWriterT

export
runReader : Reader r a -> r -> a
runReader m = runIdentity . runReaderT m

-- M : Type -> Type -> Type -> Type
-- M w r a = WriterT w (Reader r) a

-- unM : M w r a -> w -> r -> a
-- unM = ?foo
