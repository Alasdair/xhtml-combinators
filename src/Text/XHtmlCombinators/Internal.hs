{-# LANGUAGE PatternGuards, BangPatterns #-}
module Text.XHtmlCombinators.Internal where

import Control.Applicative
import Control.Monad.Writer
import Data.Sequence
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T

type Attrs = [Attr]

data Attr = Attr Text Text deriving (Show)

data Node = Node Text Attrs Attrs (Seq Node)
          | TextNode Text
          | WithAttrs Attrs
          deriving (Show)

first :: Seq a -> a
first seq | x :< _ <- viewl seq = x

textNode :: Text -> Attrs -> Attrs -> Text -> Node
textNode name rattrs attrs content = 
    Node name rattrs attrs (singleton (TextNode content))

emptyNode :: Text -> Attrs -> Attrs -> Node
emptyNode name rattrs attrs = Node name rattrs attrs Seq.empty
                              
node :: Content e => Text -> Attrs -> Attrs -> XHtml e -> Node
node name rattrs attrs content = 
    Node name rattrs attrs (toContent <$> execXHtml content)

class Content e where
    toContent :: e -> Node

type XHtmlM x a = Writer (Seq x) a
type XHtml x = XHtmlM x ()

tellS :: x -> XHtml x
tellS = tell . singleton

execXHtml :: XHtml x -> Seq x
execXHtml = snd . runWriter

tellTextNode :: (Node -> e) -> Text -> Attrs -> Attrs -> Text -> XHtml e 
tellTextNode c name rattrs attrs = tellS . c . textNode name rattrs attrs

tellEmptyNode :: (Node -> e) -> Text -> Attrs -> Attrs -> XHtml e 
tellEmptyNode c name rattrs = tellS . c . emptyNode name rattrs

tellNode :: Content a => (Node -> b) -> Text -> Attrs -> Attrs -> XHtml a -> XHtml b 
tellNode c name rattrs attrs = tellS . c . node name rattrs attrs

empty :: XHtml x
empty = return ()
