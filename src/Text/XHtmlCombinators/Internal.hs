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
import Data.Monoid
import Data.Sequence
import qualified Data.Sequence as Seq

import Data.Text (Text)
import qualified Data.Text as T

-- The XHtmlMT monad transformer

newtype XHtmlMT x t a = XHtmlMT
    { unXHtmlMT :: WriterT (Seq x) t a }

instance Functor t => Functor (XHtmlMT x t) where
    fmap f (XHtmlMT w) = XHtmlMT $ fmap f w

instance (Functor t, Monad t) => Applicative (XHtmlMT x t) where
    -- Could define better instance that would
    -- just require Applicative t =>
    pure = return

    (<*>) = ap

instance Monad t => Monad (XHtmlMT x t) where
    return = XHtmlMT . return

    (XHtmlMT w) >>= f = XHtmlMT $ w >>= unXHtmlMT . f

instance MonadTrans (XHtmlMT x) where
    lift = XHtmlMT . lift

tellS :: Monad t => x -> XHtmlMT x t ()
tellS = XHtmlMT . tell . singleton

execXHtmlMT :: Monad t => XHtmlMT x t () -> t (Seq x)
execXHtmlMT = execWriterT . unXHtmlMT

-- Some handy aliases

type XHtmlT t x = XHtmlMT x t ()

type XHtml x = XHtmlT Identity x

empty :: Monad t => XHtmlT t x
empty = return ()

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

textNode :: Text -> Attrs -> Attrs -> Text -> Node
textNode name rattrs attrs content = 
    Node name rattrs attrs (singleton (TextNode content))

emptyNode :: Text -> Attrs -> Attrs -> Node
emptyNode name rattrs attrs = Node name rattrs attrs Seq.empty

node :: (Functor t, Monad t, Content e) => Text -> Attrs -> Attrs -> XHtmlT t e -> t Node
node name rattrs attrs content = do
    nodes <- fmap toContent <$> execXHtmlMT content
    return $ Node name rattrs attrs nodes

tellTextNode :: Monad t => (Node -> e) -> Text -> Attrs -> Attrs -> Text -> XHtmlT t e 
tellTextNode c name rattrs attrs = tellS . c . textNode name rattrs attrs

tellEmptyNode :: Monad t => (Node -> e) -> Text -> Attrs -> Attrs -> XHtmlT t e 
tellEmptyNode c name rattrs = tellS . c . emptyNode name rattrs

tellNode :: (Functor t, Monad t, Content a) 
         => (Node -> b) -> Text -> Attrs -> Attrs -> XHtmlT t a -> XHtmlT t b
tellNode c name rattrs attrs content = do
    n <- lift $ node name rattrs attrs content
    tellS (c n)

first :: Seq a -> a
first seq | x :< _ <- viewl seq = x