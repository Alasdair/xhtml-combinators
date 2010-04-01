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

{-
lt = T.singleton '<'
gt = T.singleton '>'
space = T.singleton ' '
lts = T.pack "</"
nl = T.singleton '\n'

renderAttrs :: Attrs -> Text
renderAttrs [] = T.empty
renderAttrs attrs = T.concat (space : fmap renderAttr attrs)

renderNode (TextNode t) = t
renderNode (Node name rattrs attrs c)
    | Seq.null c = T.concat [lt, name, a, b, gt, lts, name, gt]
    | otherwise = T.concat 
        [lt, name, a, b, gt, fold (fmap renderNode c), lts, name, gt]
    where a = renderAttrs rattrs
          b = renderAttrs attrs

-- | Quickly render a xhtml page to text.
-- 
-- This function will render the entire page on a single line, which
-- is somewhat unreadable. On the plus side, it's relatively fast.
renderT :: (Functor t, Monad t, Content c) => XHtmlT t c -> t Text
renderT page = do
    content <- execXHtml page
    return (fold $ renderNode . toContent <$> content)

-- | Renders a pretty xhtml page with readable indentation.
-- 
-- What we do is turn the document from 'Text' into a 'String',
-- parse that string with 'Text.XML.Light', then use 'Text.XML.Light''s
-- pretty printing function to render it, before finally packing it again.
-- It probably goes without saying, but this function is /incredibly/ inefficient!
-- 
-- Also, 'Text.XML.Light' will render the document as proper XML, which is 
-- fine only if you're not trying to pass of your page as text/html.
renderPrettyT :: (Functor t, Monad t, Content c) => XHtmlT t c -> t Text
renderPrettyT page = do
    content <- renderT page
    return (T.pack . unlines . fmap XML.ppContent . XML.parseXML . T.unpack $ content)

render :: Content c => XHtml c -> Text
render = runIdentity . renderT

renderPretty :: Content c => XHtml c -> Text
renderPretty = runIdentity . renderPrettyT
-}

-- -----------------------------------------------------------------------------
--  New renderer
-- -----------------------------------------------------------------------------

{-
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
-}

