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
    AccountInfo (..),
    TS (..),

    -- ** Effects
    Telegraph (..),
    Http (..),
    Telegraph',
    Http',

    -- ** Interpreters
    runTelegraph,
    runTelegraph',

    -- *** Error Interpreters
    errorToIOAsExc,
    errorToErrorIOAsExc,

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

    -- ** Interpreter primitives
    TelegraphToIOC,
    TelegraphC,
    HttpC,
    TelegraphH,
    HttpH,
    telegraph,
    http,
  )
where

import Conduit
  ( ConduitT,
    sourceHandle,
  )
import Control.Concurrent
import Control.Effect
import Control.Effect.Bracket
import Control.Effect.Error
import Control.Effect.Reader
import Control.Effect.Telegraph
import Control.Monad.Cont
import Data.Aeson (eitherDecode, encode, object, (.=))
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Deriving.Aeson
import Deriving.Aeson.Stock
import Network.HTTP.Client (HttpException, Manager, Request (..), RequestBody (..), Response (..), parseRequest_)
import Network.HTTP.Client.Conduit (requestBodySourceChunked)
import Network.HTTP.Client.MultipartFormData
import System.IO
import Web.Telegraph.Types hiding (error)

data AccountInfo = AccountInfo
  { shortName :: Text,
    authorName :: Text,
    authorUrl :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Snake AccountInfo

-- | Use this method to create a new Telegraph account
createAccount :: Eff Http' m => AccountInfo -> m (Result Account)
createAccount !a = postAeson "https://api.telegra.ph/createAccount" a

-- | Use this method to update information about this Telegraph account
editAccountInfo :: (Effs '[Telegraph', Bracket, Throw TelegraphError] m) => AccountInfo -> m ()
editAccountInfo AccountInfo {..} =
  bracketOnError
    takeTS
    putTS
    $ \t@TS {accessToken} -> do
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
          putTS t
          throw $ APICallFailure e
        Result Account {} -> do
          let t' = TS {..}
          putTS t'

-- | Use this method to get information about this Telegraph account
getAccountInfo :: Effs '[Telegraph', Throw TelegraphError] m => m Account
getAccountInfo = do
  TS {accessToken} <- readTS
  processResult =<< getAccountInfo' accessToken

getAccountInfo' :: Eff Http' m => Text -> m (Result Account)
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
revokeAccessToken :: (Effs '[Telegraph', Bracket, Error TelegraphError] m) => m Account
revokeAccessToken =
  bracketOnError
    takeTS
    putTS
    $ \TS {..} -> do
      let o = object ["access_token" .= accessToken]
      a@Account {accessToken = accessToken'} <- processResult =<< postAeson "https://api.telegra.ph/revokeAccessToken" o
      let t' = TS {accessToken = fromJust accessToken', ..}
      putTS t'
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
  (Effs '[Telegraph', Error TelegraphError] m) =>
  -- | title
  Text ->
  -- | content
  [Node] ->
  m Page
createPage title content = do
  TS {..} <- readTS
  let o =
        object
          [ "access_token" .= accessToken,
            "title" .= title,
            "author_name" .= authorName,
            "author_url" .= authorUrl,
            "content" .= content
          ]
  processResult =<< postAeson "https://api.telegra.ph/createPage" o

-- | Use this method to edit an existing Telegraph page
editPage ::
  (Effs '[Telegraph', Throw TelegraphError] m) =>
  -- | path
  Text ->
  -- | title
  Text ->
  -- | content
  [Node] ->
  m Page
editPage path title content = do
  TS {..} <- readTS
  let o =
        object
          [ "access_token" .= accessToken,
            "path" .= path,
            "title" .= title,
            "author_name" .= authorName,
            "author_url" .= authorUrl,
            "content" .= content
          ]
  processResult =<< postAeson "https://api.telegra.ph/editPage" o

-- | Use this method to get a Telegraph page
getPage :: Eff Http' m => Text -> m (Result Page)
getPage path = do
  let o =
        object
          [ "path" .= path,
            "return_content" .= True
          ]
  postAeson "https://api.telegra.ph/getPage" o

-- | Use this method to get a list of pages belonging to this Telegraph account
getPageList ::
  (Effs '[Telegraph', Throw TelegraphError] m) =>
  -- | offset
  Int ->
  -- | limit (0 - 200)
  Int ->
  m PageList
getPageList offset limit = do
  TS {..} <- readTS
  let o =
        object
          [ "access_token" .= accessToken,
            "offset" .= offset,
            "limit" .= limit
          ]
  processResult =<< postAeson "https://api.telegra.ph/getPageList" o

-- | Use this method to get the total number of views for a Telegraph article
getTotalViews :: Eff Http' m => Text -> m (Result PageViews)
getTotalViews path = postAeson "https://api.telegra.ph/getViews" o
  where
    o = object ["path" .= path]

--------------------------------------------------
-- Upload API

uploadParts :: Eff Telegraph' m => [PartM m] -> m UploadResult
uploadParts parts = do
  let initReq = parseRequest_ "POST https://telegra.ph/upload"
  boundary <- genBoundary
  req <- formDataBodyWithBoundary boundary parts initReq
  resp <- httpLbs req
  case eitherDecode (responseBody resp) of
    Left e -> error ("impossible: json decode failure: " ++ e)
    Right r -> pure r

-- | Upload a image from a filepath to Telegraph
uploadImageFromFile :: (Effs '[Telegraph', Bracket, Embed IO] m) => FilePath -> m UploadResult
uploadImageFromFile fp =
  evalContT $ do
    src <- withSourceFile fp
    let body = requestBodySourceChunked src
        part = partFileRequestBody "file" fp body
    lift $ uploadParts [part]

-- | Upload a list of images to Telegraph. The resulting list of images will be in the same order
uploadImageFromFiles :: (Effs '[Telegraph', Bracket, Embed IO] m) => [FilePath] -> m UploadResult
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
uploadImageStreaming :: Eff Telegraph' m => ImgStream -> m UploadResult
uploadImageStreaming imgs = uploadParts [imgStream2Part imgs]

-- | Upload a list of image streams to Telegraph. The resulting list of images
uploadImagesStreaming :: Eff Telegraph' m => [ImgStream] -> m UploadResult
uploadImagesStreaming imgss = uploadParts $ map imgStream2Part imgss

--------------------------------------------------
-- Utils
postAeson :: (ToJSON a, FromJSON b, Eff Http' m) => String -> a -> m b
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
    Left e -> error ("impossible: json decode failure: " ++ e)
    Right r -> pure r

type TelegraphToIOC =
  CompositionC
    '[ TelegraphC,
       ReaderC (MVar TS),
       HttpC
     ]

runTelegraph ::
  (Effs '[Embed IO, Reader Manager, Error HttpException, Throw TelegraphError] m, Threaders '[ReaderThreads] m p) =>
  Text ->
  TelegraphToIOC m a ->
  m a
runTelegraph accessToken m =
  http $ do
    Account {shortName, authorName, authorUrl} <- processResult =<< getAccountInfo' accessToken
    ref <- embed $ newMVar TS {..}
    runReader ref $ telegraph $ runComposition m

runTelegraph' ::
  (Effs '[Embed IO, Reader Manager, Error HttpException, Throw TelegraphError] m, Threaders '[ReaderThreads] m p) =>
  AccountInfo ->
  TelegraphToIOC m a ->
  m a
runTelegraph' acc m =
  http $ do
    Account {shortName, authorName, authorUrl, accessToken = accessToken'} <- processResult =<< createAccount acc
    ref <- embed $ newMVar TS {accessToken = fromJust accessToken', ..}
    runReader ref $ telegraph $ runComposition m

processResult :: Eff (Throw TelegraphError) m => Result a -> m a
processResult (Error e) = throw $ APICallFailure e
processResult (Result r) = pure r

evalContT :: Applicative m => ContT r m r -> m r
evalContT m = runContT m pure
{-# INLINE evalContT #-}

withSourceFile :: (Effs '[Embed IO, Bracket] m, MonadIO n) => FilePath -> ContT r m (ConduitT i ByteString n ())
withSourceFile fp = ContT $ \k ->
  bracket
    (embed $ openBinaryFile fp ReadMode)
    (embed . hClose)
    (k . sourceHandle)
