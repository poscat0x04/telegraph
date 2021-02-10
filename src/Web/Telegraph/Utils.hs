module Web.Telegraph.Utils where

import Data.Aeson

sumUntagged :: Options
sumUntagged = defaultOptions {sumEncoding = UntaggedValue}

snake :: Options
snake = defaultOptions {fieldLabelModifier = camelTo2 '_', omitNothingFields = True}
