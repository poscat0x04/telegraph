{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Web.Telegraph.Types where

import Control.Exception
import Data.Aeson hiding (Result (..))
import Data.Text (Text, unpack)
import Deriving.Aeson
import Deriving.Aeson.Stock

data Account = Account
  { shortName :: {-# UNPACK #-} Text,
    authorName :: {-# UNPACK #-} Text,
    authorUrl :: {-# UNPACK #-} Text,
    accessToken :: Maybe Text,
    authUrl :: Maybe Text,
    pageCount :: Maybe Int
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier CamelToSnake, OmitNothingFields] Account

data PageList = PageList
  { totalCount :: {-# UNPACK #-} Int,
    pages :: [Page]
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Snake PageList

data Page = Page
  { path :: {-# UNPACK #-} Text,
    url :: {-# UNPACK #-} Text,
    title :: {-# UNPACK #-} Text,
    description :: {-# UNPACK #-} Text,
    authorName :: Maybe Text,
    authorUrl :: Maybe Text,
    imageUrl :: Maybe Text,
    content :: Maybe [Node],
    views :: {-# UNPACK #-} Int,
    canEdit :: Bool
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier CamelToSnake, OmitNothingFields] Page

newtype PageViews = PageViews {views :: Int}
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Vanilla PageViews

data Node
  = Content {-# UNPACK #-} Text
  | Element {-# UNPACK #-} NodeElement
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[SumUntaggedValue] Node

data NodeElement = NodeElement
  { tag :: {-# UNPACK #-} Text,
    attrs :: [(Text, [Text])],
    children :: [Node]
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Vanilla NodeElement

data Result a
  = Error {-# UNPACK #-} Text
  | Result a
  deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (Result a) where
  parseJSON = withObject "telegra.ph api call result" $ \o -> do
    ok <- o .: "ok"
    if ok
      then Result <$> o .: "result"
      else Error <$> o .: "error"

newtype Image = Image {src :: Text}
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Vanilla Image

data UploadResult
  = UploadError {error :: {-# UNPACK #-} Text}
  | Sources [Image]
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[SumUntaggedValue] UploadResult

newtype TelegraphError
  = -- | An api call has failed, we cannot distinguish between minor errors (such as illformed author urls)
    -- and much serious errors, such as invalid accessTokens, so we always throw exceptions
    APICallFailure Text
  deriving newtype (Eq)
  deriving anyclass (Exception)

instance Show TelegraphError where
  show (APICallFailure e) = "API call failed with error: " ++ unpack e
