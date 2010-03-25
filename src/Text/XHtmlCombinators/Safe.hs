-- |
-- Module      : Text.XHtmlCombinators.Safe
-- Copyright   : (c) Alasdair Armstrong 2010
-- License     : BSD-style
-- Maintainer  : alasdair.armstrong@googlemail.com
-- Stability   : experimental
-- Portability : GHC

module Text.XHtmlCombinators.Safe
    ( module Text.XHtmlCombinators
    , text
    , title', title
    , base', base
    , meta', meta
    , style', style
    , script', script
    , bdo', bdo
    , map', map_
    , area', area
    , form', form
    , optgroup', optgroup
    , option', option
    , textarea', textarea
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Text.XHtmlCombinators.Escape
import qualified Text.XHtmlCombinators as X
import Text.XHtmlCombinators hiding 
    ( text
    , title', title
    , base', base
    , meta', meta
    , style', style
    , script', script
    , bdo', bdo
    , img', img
    , map', map_
    , area', area
    , form', form
    , optgroup', optgroup
    , option', option
    , textarea', textarea
    )

-- TODO: Comment these explaining how escaping is applied.

text :: (Functor t, Monad t, CData c) => Text -> XHtmlT t c
text = X.text . escape

title' :: (Functor t, Monad t) => Attrs -> Text -> XHtmlT t HeadContent
title' attrs = X.title' attrs . escape

title :: (Functor t, Monad t) => Text -> XHtmlT t HeadContent
title = title' []

base' :: (Functor t, Monad t) => Text -> Attrs -> XHtmlT t HeadContent
base' = X.base' . escapeAttr 

base :: (Functor t, Monad t) => Text -> XHtmlT t HeadContent
base = flip base' []

meta' :: (Functor t, Monad t) => Text -> Attrs -> XHtmlT t HeadContent
meta' = X.meta' . escapeAttr

meta :: (Functor t, Monad t) => Text -> XHtmlT t HeadContent
meta = flip meta' []

style' :: (Functor t, Monad t) => Text -> Attrs -> Text -> XHtmlT t HeadContent
style' type_ attrs css = X.style' (escapeAttr type_) attrs (escapeCSS css)

style :: (Functor t, Monad t) => Text -> Text -> XHtmlT t HeadContent
style = flip style' []

script' :: (Functor t, Monad t) => Text -> Attrs -> Text -> XHtmlT t HeadContent
script' type_ attrs js = X.script' (escapeAttr type_) attrs (escapeJavaScript js)

script :: (Functor t, Monad t) => Text -> Text -> XHtmlT t HeadContent
script = flip script' []

bdo' :: (Functor t, Monad t, Inline c) => Text -> Attrs -> XHtmlT t InlineContent -> XHtmlT t c
bdo' dir = X.bdo' (escapeAttr dir)

bdo :: (Functor t, Monad t, Inline c) => Text -> XHtmlT t InlineContent -> XHtmlT t c
bdo = flip bdo' []

map' :: (Functor t, Monad t, Flow c) => Text -> Attrs -> XHtmlT t MapContent -> XHtmlT t c
map' id = X.map' (escapeAttr id)

map_ :: (Functor t, Monad t, Flow c) => Text -> XHtmlT t MapContent -> XHtmlT t c
map_ = flip map' []

area' :: (Functor t, Monad t) => Text -> Attrs -> XHtmlT t MapContent
area' alt = X.area' (escapeAttr alt)

area :: (Functor t, Monad t) => Text -> XHtmlT t MapContent
area = flip area' []

form' :: (Functor t, Monad t, Block c) => Text -> Attrs -> XHtmlT t FlowContent -> XHtmlT t c
form' action = X.form' (escapeAttr action)

form :: (Functor t, Monad t, Block c) => Text -> XHtmlT t FlowContent -> XHtmlT t c
form = flip form' []

optgroup' :: (Functor t, Monad t) => Text -> Attrs -> XHtmlT t OptionContent -> XHtmlT t OptionContent
optgroup' label = X.optgroup' (escapeAttr label)

optgroup :: (Functor t, Monad t) => Text -> XHtmlT t OptionContent -> XHtmlT t OptionContent
optgroup = flip optgroup' []

option' :: (Functor t, Monad t) => Attrs -> Text -> XHtmlT t OptionContent
option' attrs = option' attrs . escape 

option :: (Functor t, Monad t) => Text -> XHtmlT t OptionContent
option = option' []

textarea' :: (Functor t, Monad t, Inline c) => Int -> Int -> Attrs -> Text -> XHtmlT t c
textarea' rows cols attrs = X.textarea' rows cols attrs . escape

textarea :: (Functor t, Monad t, Inline c) => Int -> Int -> Text -> XHtmlT t c
textarea  rows cols = textarea' rows cols []