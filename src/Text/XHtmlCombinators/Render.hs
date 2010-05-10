{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Text.XHtmlCombinators.Render
-- Copyright   : (c) Alasdair Armstrong 2010
-- License     : BSD-style
-- Maintainer  : alasdair.armstrong@googlemail.com
-- Stability   : experimental
-- Portability : GHC

module Text.XHtmlCombinators.Render where

import Control.Applicative hiding (empty)
import Data.Foldable
import Data.Functor.Identity
import qualified Data.Sequence as Seq

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import Text.XHtmlCombinators.Internal

data Escaper e = Escaper
    { escapeAttr :: Attr -> Attr
    , escapeText :: Text -> Text
    , childEscaper :: Text -> Escaper e
    , encoder :: Text -> e
    }

unsafe :: Escaper Text
unsafe = Escaper
    { escapeAttr = id
    , escapeText = id
    , childEscaper = const unsafe
    , encoder = id
    }

lt = T.singleton '<'
gt = T.singleton '>'
space = T.singleton ' '
lts = T.pack "</"
nl = T.singleton '\n'

renderAttrs :: Escaper e -> Attrs -> Text
renderAttrs esc [] = T.empty
renderAttrs esc attrs = 
    T.concat $ space : fmap (renderAttr . escapeAttr esc) attrs

renderAttr :: Attr -> Text
renderAttr (Attr key val) = T.concat [key, "=\"", val, "\""]

renderNode :: Escaper e -> Node -> Text
renderNode esc (TextNode t) = t
renderNode esc (Node name rattrs attrs c)
    | Seq.null c = T.concat [lt, name, a, b, gt, lts, name, gt]
    | otherwise = T.concat 
        [ lt, name, a, b, gt
        , fold (renderNode (childEscaper esc name) <$> c)
        , lts, name, gt
        ]
  where 
    a = renderAttrs esc rattrs
    b = renderAttrs esc attrs

-- | Quickly render a xhtml page to text.
-- 
-- This function will render the entire page on a single line, which
-- is somewhat unreadable. On the plus side, it's relatively fast.
renderT :: (Functor t, Monad t, Content c) => Escaper e -> XHtmlT t c -> t e
renderT esc page = do
    content <- execXHtmlMT page
    let txt = fold (renderNode esc . toContent <$> content)
    return (encoder esc txt)

render :: Content c => Escaper e -> XHtml c -> e
render esc = runIdentity . renderT esc
