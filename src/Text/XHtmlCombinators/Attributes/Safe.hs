{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Text.XHtmlCombinators.Attributes
-- Copyright   : (c) Andy Gill, and the Oregon Graduate Institute of Science and Technology 1999-2001
--               (c) Bjorn Bringert 2004-2006
--               (c) Alasdair Armstrong 2010
-- License     : BSD-style
-- Maintainer  : alasdair.armstrong@googlemail.com
-- Stability   : experimental
-- Portability : GHC

module Text.XHtmlCombinators.Attributes.Safe where

import Data.Char (isAsciiLower)

import Data.Text (Text)
import qualified Data.Text as T

import Text.XHtmlCombinators.Internal (Attr (Attr))
import Text.XHtmlCombinators.Escape
import Text.XHtmlCombinators.Attributes.Internal.Safe

attr :: Text -> Text -> Attr
attr name = Attr (T.filter isAsciiLower name) . escapeAttr

-- the following was copied almost verbatim from Bjorn Bringert's xhtml library.
-- See: http://hackage.haskell.org/package/xhtml

-- TODO: Not all attributes are currently available.

action :: Text -> Attr
align :: Text -> Attr
alt :: Text -> Attr
altcode :: Text -> Attr
archive :: Text -> Attr
base :: Text -> Attr
border :: Int -> Attr
bordercolor :: Text -> Attr
cellpadding :: Int -> Attr
cellspacing :: Int -> Attr
checked :: Attr
codebase :: Text -> Attr
cols :: Text -> Attr
colspan :: Int -> Attr
content :: Text -> Attr
coords :: Text -> Attr
disabled :: Attr
enctype :: Text -> Attr
height :: Text -> Attr
href :: Text -> Attr
hreflang :: Text -> Attr
httpEquiv :: Text -> Attr
id_ :: Text -> Attr
ismap :: Attr
lang :: Text -> Attr
maxlength :: Int -> Attr
method :: Text -> Attr
multiple :: Attr
name :: Text -> Attr
nohref :: Attr
rel :: Text -> Attr
rev :: Text -> Attr
rows :: Text -> Attr
rowspan :: Int -> Attr
rules :: Text -> Attr
selected :: Attr
shape :: Text -> Attr
size :: Text -> Attr
src :: Text -> Attr
class_ :: Text -> Attr
for :: Text -> Attr
style :: Text -> Attr
type_ :: Text -> Attr
title :: Text -> Attr
usemap :: Text -> Attr
valign :: Text -> Attr
value :: Text -> Attr
width :: Text -> Attr

action      =  textAttr "action"
align       =  textAttr "align"
alt         =  textAttr "alt"
altcode     =  textAttr "altcode"
archive     =  textAttr "archive"
base        =  textAttr "base"
border      =   intAttr "border"
bordercolor =  textAttr "bordercolor"
cellpadding =   intAttr "cellpadding"
cellspacing =   intAttr "cellspacing"
checked     = emptyAttr "checked"
codebase    =  textAttr "codebase"
cols        =  textAttr "cols"
colspan     =   intAttr "colspan"
content     =  textAttr "content"
coords      =  textAttr "coords"
disabled    = emptyAttr "disabled"
enctype     =  textAttr "enctype"
height      =  textAttr "height"
href        =  textAttr "href"
hreflang    =  textAttr "hreflang"
httpEquiv   =  textAttr "http-equiv"
id_         =  textAttr "id"
ismap       = emptyAttr "ismap"
lang        =  textAttr "lang"
maxlength   =   intAttr "maxlength"
method      =  textAttr "method"
multiple    = emptyAttr "multiple"
name        =  textAttr "name"
nohref      = emptyAttr "nohref"
rel         =  textAttr "rel"
rev         =  textAttr "rev"
rows        =  textAttr "rows"
rowspan     =   intAttr "rowspan"
rules       =  textAttr "rules"
selected    = emptyAttr "selected"
shape       =  textAttr "shape"
size        =  textAttr "size"
src         =  textAttr "src"
class_      =  textAttr "class"
for         =  textAttr "for"
--style       =  textAttr "style"
type_       =  textAttr "type"
title       =  textAttr "title"
usemap      =  textAttr "usemap"
valign      =  textAttr "valign"
value       =  textAttr "value"
width       =  textAttr "width"

-- |
-- Currently data input into the style tag is not
-- escaped.
style = Attr "style"