-- |
-- Module      : Text.XHtmlCombinators.Attributes
-- Copyright   : (c) Alasdair Armstrong 2010
-- License     : BSD-style
-- Maintainer  : alasdair.armstrong@googlemail.com
-- Stability   : experimental
-- Portability : GHC

module Text.XHtmlCombinators.Attributes.Internal
    ( emptyAttr
    , intAttr
    , textAttr
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Text.XHtmlCombinators.Internal

emptyAttr :: Text -> Attr
emptyAttr name = Attr name name

intAttr :: Text -> Int -> Attr
intAttr name = Attr name . T.pack . show

textAttr :: Text -> Text -> Attr
textAttr name = Attr name