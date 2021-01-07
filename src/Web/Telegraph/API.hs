{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}

module Web.Telegraph.API
  ( -- ** Types
    Telegraph (..),
    MonadTelegraph (..),
    AccountInfo (..),

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
  )
where

import Conduit
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson (eitherDecode, encode, object, (.=))
import Data.Maybe
import Data.Text (Text)
import Deriving.Aeson
import Deriving.Aeson.Stock
import Network.HTTP.Client.Conduit
import Web.Telegraph.Types
import Prelude as P

type HasHttpCap env m = (MonadIO m, HasHttpManager env, MonadReader env m)

class MonadThrow m => MonadTelegraph m where
  takeTelegraph :: m Telegraph
  readTelegraph :: m Telegraph
  putTelegraph :: Telegraph -> m ()

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

createAccount :: HasHttpCap env m => AccountInfo -> m (Result Account)
createAccount !a = postAeson "https://api.telegra.ph/createAccount" a

data EditAccountInfo = EditAccountInfo
  { accessToken :: Text,
    shortName :: Text,
    authorName :: Text,
    authorUrl :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Snake EditAccountInfo

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

revokeAccessToken :: (HasHttpCap env m, MonadTelegraph m, MonadMask m) => m Account
revokeAccessToken =
  bracketOnError
    takeTelegraph
    putTelegraph
    $ \t@Telegraph {..} -> do
      let o = object ["access_token" .= accessToken]
      r <- postAeson "https://api.telegra.ph/revokeAccessToken" o
      case r of
        Error e -> do
          putTelegraph t
          throwM $ APICallFailure e
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

createPage :: (HasHttpCap env m, MonadTelegraph m) => Text -> [Node] -> m Page
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

editPage :: (HasHttpCap env m, MonadTelegraph m) => Text -> Text -> [Node] -> m Page
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

getPage :: HasHttpCap env m => Text -> m (Result Page)
getPage path = do
  let o =
        object
          [ "path" .= path,
            "return_content" .= True
          ]
  postAeson "https://api.telegra.ph/getPage" o

getPageList :: (HasHttpCap env m, MonadTelegraph m) => Int -> Int -> m PageList
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

getTotalViews :: HasHttpCap env m => Text -> m (Result PageViews)
getTotalViews path = postAeson "https://api.telegra.ph/getViews" o
  where
    o = object ["path" .= path]

-------------------
-- Utils
postAeson :: (ToJSON a, FromJSON b, HasHttpCap env m) => String -> a -> m b
postAeson url c = do
  let req = (parseRequest_ url) {method = "POST", requestBody = RequestBodyLBS $ encode c}
  resp <- httpLbs req
  case eitherDecode (responseBody resp) of
    Left e -> P.error ("impossible: json decode failure: " ++ e)
    Right r -> pure r
