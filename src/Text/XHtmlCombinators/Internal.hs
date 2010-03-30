{-# LANGUAGE PatternGuards #-}

-- |
-- Module      : Text.XHtmlCombinators.Internal
-- Copyright   : (c) Alasdair Armstrong 2010
-- License     : BSD-style
-- Maintainer  : alasdair.armstrong@googlemail.com
-- Stability   : experimental
-- Portability : GHC

module Text.XHtmlCombinators.Internal where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Functor.Identity
import Data.Sequence
import qualified Data.Sequence as Seq

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsString (..))

type Attrs = [Attr]

data Attr = Attr Text Text deriving (Show)

data Node = Node Text Attrs Attrs (Seq Node)
          | TextNode Text
          deriving (Show)

class CData c where
    cdata :: Text -> c

text :: (Functor t, Monad t, CData c) => Text -> XHtmlT t c
text = tellS . cdata

class Content e where
    toContent :: e -> Node

newtype Root = Root { rootToNode :: Node }

instance Content Root where toContent = rootToNode

type XHtmlMT t x a = WriterT (Seq x) t a
type XHtmlT t x = XHtmlMT t x ()

type XHtml x = XHtmlT Identity x

first :: Seq a -> a
first seq | x :< _ <- viewl seq = x

textNode :: Text -> Attrs -> Attrs -> Text -> Node
textNode name rattrs attrs content = 
    Node name rattrs attrs (singleton (TextNode content))

emptyNode :: Text -> Attrs -> Attrs -> Node
emptyNode name rattrs attrs = Node name rattrs attrs Seq.empty
                              
node :: (Functor t, Monad t, Content e) => Text -> Attrs -> Attrs -> XHtmlT t e -> t Node
node name rattrs attrs content = 
    Node name rattrs attrs . fmap toContent <$> execXHtml content

tellS :: Monad t => x -> XHtmlT t x
tellS = tell . singleton

execXHtml :: Monad t => XHtmlT t x -> t (Seq x)
execXHtml = execWriterT

tellTextNode :: Monad t => (Node -> e) -> Text -> Attrs -> Attrs -> Text -> XHtmlT t e 
tellTextNode c name rattrs attrs = tellS . c . textNode name rattrs attrs

tellEmptyNode :: Monad t => (Node -> e) -> Text -> Attrs -> Attrs -> XHtmlT t e 
tellEmptyNode c name rattrs = tellS . c . emptyNode name rattrs

tellNode :: (Functor t, Monad t, Content a) 
         => (Node -> b) -> Text -> Attrs -> Attrs -> XHtmlT t a -> XHtmlT t b
tellNode c name rattrs attrs = tellS . c <=< lift . node name rattrs attrs

empty :: Monad t => XHtmlT t x
empty = return ()
