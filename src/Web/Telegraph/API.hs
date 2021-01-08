{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The telegraph API.
-- Every function that runs in 'MonadTelegraph' might throw a 'TelegraphError'.
module Web.Telegraph.API
  ( -- ** Types
    Telegraph (..),
    MonadTelegraph (..),
    AccountInfo (..),

    -- ** Interpreting 'MonadTelegraph'
    TelegraphT (..),
    runTelegraph,
    runTelegraph',

    -- ** Type Synonyms
    HasHttpCap,

    -- ** Account related APIs
    editAccountInfo,
    getAccountInfo,
    revokeAccessToken,
    createPage,
    editPage,
    getPageList,

    -- ** Account independent APIs
    createAccount,
    getAccountInfo',
    getPage,
    getTotalViews,

    -- ** Image uploading API
    uploadImageFromFile,
    uploadImageFromFiles,
    ImgStream (..),
    uploadImageStreaming,
    uploadImagesStreaming,
    uploadParts,
  )
where

import Conduit
  ( ConduitT,
    sourceHandle,
  )
import Control.Concurrent
import Control.Exception (throwIO)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson (eitherDecode, encode, object, (.=))
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Deriving.Aeson
import Deriving.Aeson.Stock
import Network.HTTP.Client.Conduit
import Network.HTTP.Client.MultipartFormData
import System.IO
import Web.Telegraph.Types
import Prelude as P

type HasHttpCap env m = (MonadIO m, HasHttpManager env, MonadReader env m)

class MonadThrow m => MonadTelegraph m where
  takeTelegraph :: m Telegraph
  readTelegraph :: m Telegraph
  putTelegraph :: Telegraph -> m ()

newtype TelegraphT m a = TelegraphT {runTelegraphT :: ReaderT (MVar Telegraph) m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadTrans,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadBase b,
      MonadBaseControl b
    )

instance MonadReader r m => MonadReader r (TelegraphT m) where
  ask = lift ask
  local f m = do
    ref <- TelegraphT ask
    lift $ local f $ flip runReaderT ref $ runTelegraphT m

instance (MonadThrow m, MonadIO m) => MonadTelegraph (TelegraphT m) where
  takeTelegraph = TelegraphT ask >>= liftIO . takeMVar
  readTelegraph = TelegraphT ask >>= liftIO . readMVar
  putTelegraph t = TelegraphT ask >>= \ref -> liftIO $ putMVar ref t

instance
  {-# OVERLAPPABLE #-}
  ( MonadTelegraph m,
    MonadTrans f,
    MonadThrow (f m)
  ) =>
  MonadTelegraph (f m)
  where
  takeTelegraph = lift takeTelegraph
  readTelegraph = lift readTelegraph
  putTelegraph = lift . putTelegraph

data Telegraph = Telegraph
  { accessToken :: Text,
    shortName :: Text,
    authorName :: Text,
    authorUrl :: Text
  }
  deriving (Show, Eq, Generic)

data AccountInfo = AccountInfo
  { shortName :: Text,
    authorName :: Text,
    authorUrl :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Snake AccountInfo

-- | Use this method to create a new Telegraph account
createAccount :: HasHttpCap env m => AccountInfo -> m (Result Account)
createAccount !a = postAeson "https://api.telegra.ph/createAccount" a

-- | Use this method to update information about this Telegraph account
editAccountInfo :: (HasHttpCap env m, MonadTelegraph m, MonadMask m) => AccountInfo -> m ()
editAccountInfo AccountInfo {..} =
  bracketOnError
    takeTelegraph
    putTelegraph
    $ \t@Telegraph {accessToken} -> do
      let o =
            object
              [ "access_token" .= accessToken,
                "short_name" .= shortName,
                "author_name" .= authorName,
                "author_url" .= authorUrl
              ]
      r <- postAeson "https://api.telegra.ph/editAccountInfo" o
      case r of
        Error e -> do
          putTelegraph t
          throwM $ APICallFailure e
        Result Account {} -> do
          let t' = Telegraph {..}
          putTelegraph t'

-- | Use this method to get information about this Telegraph account
getAccountInfo :: (HasHttpCap env m, MonadTelegraph m) => m Account
getAccountInfo = do
  Telegraph {accessToken} <- readTelegraph
  r <- getAccountInfo' accessToken
  case r of
    Error e -> throwM $ APICallFailure e
    Result a -> pure a

getAccountInfo' :: HasHttpCap env m => Text -> m (Result Account)
getAccountInfo' accessToken = postAeson "https://api.telegra.ph/getAccountInfo" o
  where
    fields :: [Text]
    fields = ["short_name", "author_name", "author_url", "auth_url", "page_count"]
    o =
      object
        [ "access_token" .= accessToken,
          "fields" .= fields
        ]

-- | Use this method to revoke access_token and generate a new one
revokeAccessToken :: (HasHttpCap env m, MonadTelegraph m, MonadMask m) => m Account
revokeAccessToken =
  bracketOnError
    takeTelegraph
    putTelegraph
    $ \Telegraph {..} -> do
      let o = object ["access_token" .= accessToken]
      r <- postAeson "https://api.telegra.ph/revokeAccessToken" o
      case r of
        Error e -> throwM $ APICallFailure e
        Result a@Account {accessToken = accessToken'} -> do
          let t' = Telegraph {accessToken = fromJust accessToken', ..}
          putTelegraph t'
          pure a

data CreatePage = CreatePage
  { accessToken :: Text,
    title :: Text,
    authorName :: Maybe Text,
    authorUrl :: Maybe Text,
    content :: [Node],
    returnContent :: Bool
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Snake CreatePage

-- | Use this method to create a new Telegraph page
createPage ::
  (HasHttpCap env m, MonadTelegraph m) =>
  -- | title
  Text ->
  -- | content
  [Node] ->
  m Page
createPage title content = do
  Telegraph {..} <- readTelegraph
  let o =
        object
          [ "access_token" .= accessToken,
            "title" .= title,
            "author_name" .= authorName,
            "author_url" .= authorUrl,
            "content" .= content
          ]
  r <- postAeson "https://api.telegra.ph/createPage" o
  case r of
    Error e -> throwM $ APICallFailure e
    Result p -> pure p

-- | Use this method to edit an existing Telegraph page
editPage ::
  (HasHttpCap env m, MonadTelegraph m) =>
  -- | path
  Text ->
  -- | title
  Text ->
  -- | content
  [Node] ->
  m Page
editPage path title content = do
  Telegraph {..} <- readTelegraph
  let o =
        object
          [ "access_token" .= accessToken,
            "path" .= path,
            "title" .= title,
            "author_name" .= authorName,
            "author_url" .= authorUrl,
            "content" .= content
          ]
  r <- postAeson "https://api.telegra.ph/editPage" o
  case r of
    Error e -> throwM $ APICallFailure e
    Result p -> pure p

-- | Use this method to get a Telegraph page
getPage :: HasHttpCap env m => Text -> m (Result Page)
getPage path = do
  let o =
        object
          [ "path" .= path,
            "return_content" .= True
          ]
  postAeson "https://api.telegra.ph/getPage" o

-- | Use this method to get a list of pages belonging to this Telegraph account
getPageList ::
  (HasHttpCap env m, MonadTelegraph m) =>
  -- | offset
  Int ->
  -- | limit (0 - 200)
  Int ->
  m PageList
getPageList offset limit = do
  Telegraph {..} <- readTelegraph
  let o =
        object
          [ "access_token" .= accessToken,
            "offset" .= offset,
            "limit" .= limit
          ]
  r <- postAeson "https://api.telegra.ph/getPageList" o
  case r of
    Error e -> throwM $ APICallFailure e
    Result p -> pure p

-- | Use this method to get the total number of views for a Telegraph article
getTotalViews :: HasHttpCap env m => Text -> m (Result PageViews)
getTotalViews path = postAeson "https://api.telegra.ph/getViews" o
  where
    o = object ["path" .= path]

--------------------------------------------------
-- Upload API

uploadParts :: HasHttpCap env m => [PartM m] -> m UploadResult
uploadParts parts = do
  let initReq = parseRequest_ "POST https://telegra.ph/upload"
  boundary <- liftIO webkitBoundary
  req <- formDataBodyWithBoundary boundary parts initReq
  resp <- httpLbs req
  case eitherDecode (responseBody resp) of
    Left e -> P.error ("impossible: json decode failure: " ++ e)
    Right r -> pure r

-- | Upload a image from a filepath to Telegraph
uploadImageFromFile :: (HasHttpCap env m, MonadMask m) => FilePath -> m UploadResult
uploadImageFromFile fp =
  evalContT $ do
    src <- withSourceFile fp
    let body = requestBodySourceChunked src
        part = partFileRequestBody "file" fp body
    lift $ uploadParts [part]

-- | Upload a list of images to Telegraph. The resulting list of images will be in the same order
uploadImageFromFiles :: (HasHttpCap env m, MonadMask m) => [FilePath] -> m UploadResult
uploadImageFromFiles fps =
  evalContT $ do
    srcs <- traverse withSourceFile fps
    let bodies = map requestBodySourceChunked srcs
        parts = zipWith (\fp -> partFileRequestBody (pack fp) fp) fps bodies
    lift $ uploadParts parts

data ImgStream = ImgStream
  { -- | an image stream needs a filename
    name :: Text,
    stream :: forall i n. MonadIO n => ConduitT i ByteString n ()
  }

imgStream2Part :: Applicative m => ImgStream -> PartM m
imgStream2Part ImgStream {..} = partFileRequestBody name (unpack name) body
  where
    body = requestBodySourceChunked stream

-- | Upload a image stream to Telegraph
uploadImageStreaming :: HasHttpCap env m => ImgStream -> m UploadResult
uploadImageStreaming imgs = uploadParts [imgStream2Part imgs]

-- | Upload a list of image streams to Telegraph. The resulting list of images
uploadImagesStreaming :: HasHttpCap env m => [ImgStream] -> m UploadResult
uploadImagesStreaming imgss = uploadParts $ map imgStream2Part imgss

--------------------------------------------------
-- Utils
postAeson :: (ToJSON a, FromJSON b, HasHttpCap env m) => String -> a -> m b
postAeson url c = do
  let req =
        (parseRequest_ url)
          { method = "POST",
            requestBody = RequestBodyLBS $ encode c,
            requestHeaders =
              [ ("content-type", "application/json"),
                ("accept", "application/json")
              ]
          }
  resp <- httpLbs req
  case eitherDecode (responseBody resp) of
    Left e -> P.error ("impossible: json decode failure: " ++ e)
    Right r -> pure r

-- | interprets 'TelegraphT' using the access token of an existing account
runTelegraph :: HasHttpCap env m => Text -> TelegraphT m a -> m a
runTelegraph accessToken m = do
  r <- getAccountInfo' accessToken
  case r of
    Error e -> liftIO $ throwIO $ APICallFailure e
    Result Account {shortName, authorName, authorUrl} -> do
      let t = Telegraph {..}
      ref <- liftIO $ newMVar t
      m
        & runTelegraphT
        & flip runReaderT ref

-- | Create a new account and interprets 'TelegraphT' using that account
runTelegraph' :: HasHttpCap env m => AccountInfo -> TelegraphT m a -> m a
runTelegraph' acc m = do
  r <- createAccount acc
  case r of
    Error e -> liftIO $ throwIO $ APICallFailure e
    Result Account {shortName, authorName, authorUrl, accessToken = accessToken'} -> do
      let t = Telegraph {accessToken = fromJust accessToken', ..}
      ref <- liftIO $ newMVar t
      m
        & runTelegraphT
        & flip runReaderT ref

evalContT :: Applicative m => ContT r m r -> m r
evalContT m = runContT m pure
{-# INLINE evalContT #-}

withSourceFile :: (MonadMask m, MonadIO m, MonadIO n) => FilePath -> ContT r m (ConduitT i ByteString n ())
withSourceFile fp = ContT $ \k ->
  bracket
    (liftIO $ openBinaryFile fp ReadMode)
    (liftIO . hClose)
    (k . sourceHandle)
