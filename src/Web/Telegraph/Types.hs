{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type definitions, note that all fields are strict
module Web.Telegraph.Types where

import Control.Exception
import Data.Aeson hiding (Result (..))
import Data.Maybe
import Data.Text (Text, unpack)
import Deriving.Aeson
import Deriving.Aeson.Stock
import Generic.Data.Surgery
import Optics.TH

-- | A Telegraph account
data Account = Account
  { -- | Account name, helps users with several accounts remember which they are currently using
    --
    -- Displayed to the user above the "Edit/Publish" button on Telegra.ph, other users don't see this name
    shortName :: {-# UNPACK #-} Text,
    -- | Default author name used when creating new articles
    authorName :: {-# UNPACK #-} Text,
    -- | Profile link, opened when users click on the author's name below the title
    --
    -- Can be any link, not necessarily to a Telegram profile or channel
    authorUrl :: {-# UNPACK #-} Text,
    -- | Access token of the Telegraph account
    accessToken :: Maybe Text,
    -- | URL to authorize a browser on telegra.ph and connect it to a Telegraph account
    --
    -- This URL is valid for only one use and for 5 minutes only
    authUrl :: Maybe Text,
    -- | Number of pages belonging to the Telegraph account
    pageCount :: Maybe Int
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier CamelToSnake, OmitNothingFields] Account

-- | A list of Telegraph articles belonging to an account
--
-- Most recently created articles first
data PageList = PageList
  { -- | Total number of pages belonging to the target Telegraph account
    totalCount :: {-# UNPACK #-} Int,
    -- | Requested pages of the target Telegraph account
    pages :: [Page]
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Snake PageList

-- | A page on Telegraph
data Page = Page
  { -- | Path to the page
    path :: {-# UNPACK #-} Text,
    -- | URL of the page
    url :: {-# UNPACK #-} Text,
    -- | Title of the page
    title :: {-# UNPACK #-} Text,
    -- | Description of the page
    description :: {-# UNPACK #-} Text,
    -- | Name of the author, displayed below the title
    authorName :: Maybe Text,
    -- | rofile link, opened when users click on the author's name below the title
    --
    -- Can be any link, not necessarily to a Telegram profile or channel
    authorUrl :: Maybe Text,
    -- | Image URL of the page
    imageUrl :: Maybe Text,
    -- | Content of the page
    content :: Maybe [Node],
    -- | Number of page views for the page
    views :: {-# UNPACK #-} Int,
    -- | True, if the target Telegraph account can edit the page
    canEdit :: Maybe Bool
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier CamelToSnake, OmitNothingFields] Page

-- | The number of page views for a Telegraph article
newtype PageViews = PageViews {views :: Int}
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Vanilla PageViews

-- | A DOM Node
data Node
  = Content {-# UNPACK #-} Text
  | Element {-# UNPACK #-} NodeElement
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[SumUntaggedValue] Node

-- | A DOM elemen node
data NodeElement = NodeElement
  { -- | Name of the DOM element
    --
    -- Available tags: @a@, @aside@, @b@, @blockquote@, @br@, @code@, @em@, @figcaption@, @figure@,
    -- @h3@, @h4@, @hr@, @i@, @iframe@, @img@, @li@, @ol@, @p@, @pre@, @s@, @strong@, @u@, @ul@, @video@
    tag :: {-# UNPACK #-} Text,
    -- | Attributes of the DOM element
    --
    -- Key of object represents name of attribute, value represents value of attribute
    --
    -- Available attributes: @href@, @src@
    attrs :: [(Text, [Text])],
    -- | List of child nodes for the DOM element
    children :: [Node]
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via Vanilla NodeElement

instance FromJSON NodeElement where
  parseJSON =
    fmap
      ( fromORLazy
          . modifyRField @"attrs" (fromMaybe [])
          . modifyRField @"children" (fromMaybe [])
          . toOR'
      )
      . genericParseJSON defaultOptions

-- | The result of an API call
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

-- | An image uploaded to Telegraph
newtype Image = Image
  { -- | The path to the image
    src :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Vanilla Image

-- | The result of an image upload
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

makeFieldLabelsWith noPrefixFieldLabels ''Account
makeFieldLabelsWith noPrefixFieldLabels ''PageList
makeFieldLabelsWith noPrefixFieldLabels ''Page
makeFieldLabelsWith noPrefixFieldLabels ''PageViews
makeFieldLabelsWith noPrefixFieldLabels ''NodeElement
makeFieldLabelsWith noPrefixFieldLabels ''Image
makeFieldLabelsWith noPrefixFieldLabels ''UploadResult

makePrismLabels ''Result
makePrismLabels ''Node
makePrismLabels ''UploadResult
