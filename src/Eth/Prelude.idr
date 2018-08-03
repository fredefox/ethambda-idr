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
execWriterT : Functor m => WriterT w m a -> m w
execWriterT = (map snd) . runWriterT

export
execWriter : Writer w a -> w
execWriter = runIdentity . execWriterT

export
runReader : Reader r a -> r -> a
runReader m = runIdentity . runReaderT m
