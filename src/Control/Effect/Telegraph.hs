{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Telegraph where

import Control.Concurrent
import Control.Effect
import Control.Effect.Error
import Control.Effect.ErrorIO
import Control.Effect.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client
import qualified Network.HTTP.Client as C
import Network.HTTP.Client.MultipartFormData

data Http :: Effect where
  HttpLbs :: Request -> Http m (Response LBS.ByteString)
  GenBoundary :: Http m ByteString

data Telegraph :: Effect where
  TakeTS :: Telegraph m TS
  ReadTS :: Telegraph m TS
  PutTS :: TS -> Telegraph m ()

type Telegraph' = Bundle '[Telegraph, Http, Error HttpException]

type Http' = Bundle '[Http, Error HttpException]

data TelegraphH

instance
  Effs '[Embed IO, Reader (MVar TS)] m =>
  Handler TelegraphH Telegraph m
  where
  effHandler = \case
    TakeTS -> ask >>= embed . takeMVar
    ReadTS -> ask >>= embed . readMVar
    PutTS ts -> do
      ref <- ask
      embed $ putMVar ref ts

type TelegraphC = InterpretC TelegraphH Telegraph

telegraph ::
  Effs '[Embed IO, Reader (MVar TS)] m =>
  TelegraphC m a ->
  m a
telegraph = interpretViaHandler
{-# INLINE telegraph #-}

data HttpH

instance
  Effs '[Embed IO, Reader Manager] m =>
  Handler HttpH Http m
  where
  effHandler = \case
    HttpLbs req -> ask >>= embed . C.httpLbs req
    GenBoundary -> embed webkitBoundary

type HttpC = InterpretC HttpH Http

-- | Interpret an @Http@ effect
http ::
  Effs '[Embed IO, Reader Manager] m =>
  HttpC m a ->
  m a
http = interpretViaHandler
{-# INLINE http #-}

errorToErrorIOAsExc ::
  (Exception e, Effs '[ErrorIO, Embed IO] m) =>
  InterpretErrorC e m a ->
  m a
errorToErrorIOAsExc main =
  interpret (\(Throw e) -> throwIO e) $
    interpret (\(Catch m h) -> m `catchIO` h) $
      runComposition main
{-# INLINE errorToErrorIOAsExc #-}

errorToIOAsExc ::
  (Exception e, Eff (Embed IO) m, MonadCatch m) =>
  ErrorToIOC e m a ->
  m a
errorToIOAsExc m =
  errorIOToIO $
    errorToErrorIOAsExc $
      introUnderMany $
        runComposition m
{-# INLINE errorToIOAsExc #-}

takeTS :: Eff Telegraph m => m TS
takeTS = send TakeTS

readTS :: Eff Telegraph m => m TS
readTS = send ReadTS

putTS :: Eff Telegraph m => TS -> m ()
putTS !s = send (PutTS s)

httpLbs :: Effs '[Http, Error HttpException] m => Request -> m (Response LBS.ByteString)
httpLbs !r = send (HttpLbs r)

genBoundary :: Eff Http m => m ByteString
genBoundary = send GenBoundary

-- | Telegraph state
data TS = TS
  { accessToken :: Text,
    shortName :: Text,
    authorName :: Text,
    authorUrl :: Text
  }
  deriving (Show, Eq, Generic)
