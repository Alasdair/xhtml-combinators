{-# LANGUAGE OverloadedStrings #-}
module Text.XHtmlCombinators.Frameset
    ( XHtml1
    , XHtml2
    , XHtml3
    , XHtml4
    , XHtml5
    , XHtml6
    , XHtml7
    , XHtml8
    , XHtml9
    , XHtml10
    , XHtml11
    , XHtml12
    , XHtml13
    , XHtml14
    , XHtml15
    , XHtml16
    , XHtml17
    , HtmlContent
    , HeadContent
    , BlockContent
    , FramesetContent
    , NoframesContent
    , InlineContent
    , ListContent
    , DlContent
    , AddressContent
    , PreContent
    , AContent
    , ObjectContent
    , MapContent
    , FormContent
    , SelectContent
    , OptgroupContent
    , FieldsetContent
    , ButtonContent
    , TableContent
    , TableSectionContent
    , ColgroupContent
    , TrContent
    , html', html
    , head', head_
    , title', title
    , style', style
    , script', script
    , option', option
    , textarea', textarea
    , base', base
    , meta', meta
    , link', link
    , frame', frame
    , hr', hr
    , br', br
    , basefont', basefont
    , param', param
    , img', img
    , area', area
    , input', input
    , isindex', isindex
    , col', col
    , noscript', noscript
    , iframe', iframe
    , body', body
    , div', div_
    , li', li
    , dd', dd
    , blockquote', blockquote
    , center', center
    , ins', ins
    , del', del
    , th', th
    , td', td
    , frameset', frameset
    , noframes', noframes
    , p', p
    , h1', h1
    , h2', h2
    , h3', h3
    , h4', h4
    , h5', h5
    , h6', h6
    , dt', dt
    , span', span_
    , bdo', bdo
    , em', em
    , strong', strong
    , dfn', dfn
    , code', code
    , samp', samp
    , kbd', kbd
    , var', var
    , cite', cite
    , abbr', abbr
    , acronym', acronym
    , q', q
    , sub', sub
    , sup', sup
    , tt', tt
    , i', i
    , b', b
    , big', big
    , small', small
    , u', u
    , s', s
    , strike', strike
    , font', font
    , label', label
    , legend', legend
    , caption', caption
    , ul', ul
    , ol', ol
    , menu', menu
    , dir', dir
    , dl', dl
    , address', address
    , pre', pre
    , a', a
    , object', object
    , applet', applet
    , map', map_
    , form', form
    , select', select
    , optgroup', optgroup
    , fieldset', fieldset
    , button', button
    , table', table
    , thead', thead
    , tfoot', tfoot
    , tbody', tbody
    , colgroup', colgroup
    , tr', tr
    ) where

import Data.Text (Text)

import Text.XHtmlCombinators.Internal
import qualified Text.XHtmlCombinators.Frameset.Attributes as A

class XHtml1 c where
    xhtml1 :: Node -> c

instance XHtml1 HeadContent where xhtml1 = Head
instance XHtml1 BlockContent where xhtml1 = Block
instance XHtml1 InlineContent where xhtml1 = Inline
instance XHtml1 AddressContent where xhtml1 = Address
instance XHtml1 PreContent where xhtml1 = Pre
instance XHtml1 AContent where xhtml1 = A
instance XHtml1 ObjectContent where xhtml1 = Object
instance XHtml1 MapContent where xhtml1 = Map
instance XHtml1 FormContent where xhtml1 = Form
instance XHtml1 FieldsetContent where xhtml1 = Fieldset
instance XHtml1 ButtonContent where xhtml1 = Button

class XHtml2 c where
    xhtml2 :: Node -> c

instance XHtml2 SelectContent where xhtml2 = Select
instance XHtml2 OptgroupContent where xhtml2 = Optgroup

class XHtml3 c where
    xhtml3 :: Node -> c

instance XHtml3 BlockContent where xhtml3 = Block
instance XHtml3 InlineContent where xhtml3 = Inline
instance XHtml3 AddressContent where xhtml3 = Address
instance XHtml3 PreContent where xhtml3 = Pre
instance XHtml3 AContent where xhtml3 = A
instance XHtml3 ObjectContent where xhtml3 = Object
instance XHtml3 FormContent where xhtml3 = Form
instance XHtml3 FieldsetContent where xhtml3 = Fieldset

class XHtml4 c where
    xhtml4 :: Node -> c

instance XHtml4 BlockContent where xhtml4 = Block
instance XHtml4 ObjectContent where xhtml4 = Object
instance XHtml4 MapContent where xhtml4 = Map
instance XHtml4 FormContent where xhtml4 = Form
instance XHtml4 FieldsetContent where xhtml4 = Fieldset
instance XHtml4 ButtonContent where xhtml4 = Button

class XHtml5 c where
    xhtml5 :: Node -> c

instance XHtml5 BlockContent where xhtml5 = Block
instance XHtml5 InlineContent where xhtml5 = Inline
instance XHtml5 AddressContent where xhtml5 = Address
instance XHtml5 PreContent where xhtml5 = Pre
instance XHtml5 AContent where xhtml5 = A
instance XHtml5 ObjectContent where xhtml5 = Object
instance XHtml5 FormContent where xhtml5 = Form
instance XHtml5 FieldsetContent where xhtml5 = Fieldset
instance XHtml5 ButtonContent where xhtml5 = Button

class XHtml6 c where
    xhtml6 :: Node -> c

instance XHtml6 BlockContent where xhtml6 = Block
instance XHtml6 InlineContent where xhtml6 = Inline
instance XHtml6 AddressContent where xhtml6 = Address
instance XHtml6 AContent where xhtml6 = A
instance XHtml6 ObjectContent where xhtml6 = Object
instance XHtml6 FormContent where xhtml6 = Form
instance XHtml6 FieldsetContent where xhtml6 = Fieldset
instance XHtml6 ButtonContent where xhtml6 = Button

class XHtml7 c where
    xhtml7 :: Node -> c

instance XHtml7 HeadContent where xhtml7 = Head
instance XHtml7 BlockContent where xhtml7 = Block
instance XHtml7 ObjectContent where xhtml7 = Object
instance XHtml7 MapContent where xhtml7 = Map
instance XHtml7 FormContent where xhtml7 = Form
instance XHtml7 FieldsetContent where xhtml7 = Fieldset

class XHtml8 c where
    xhtml8 :: Node -> c

instance XHtml8 TableContent where xhtml8 = Table
instance XHtml8 ColgroupContent where xhtml8 = Colgroup

class XHtml9 c where
    xhtml9 :: Node -> c

instance XHtml9 BlockContent where xhtml9 = Block
instance XHtml9 InlineContent where xhtml9 = Inline
instance XHtml9 AddressContent where xhtml9 = Address
instance XHtml9 AContent where xhtml9 = A
instance XHtml9 ObjectContent where xhtml9 = Object
instance XHtml9 FormContent where xhtml9 = Form
instance XHtml9 FieldsetContent where xhtml9 = Fieldset

class XHtml10 c where
    xhtml10 :: Node -> c

instance XHtml10 BlockContent where xhtml10 = Block
instance XHtml10 InlineContent where xhtml10 = Inline
instance XHtml10 AddressContent where xhtml10 = Address
instance XHtml10 PreContent where xhtml10 = Pre
instance XHtml10 AContent where xhtml10 = A
instance XHtml10 ObjectContent where xhtml10 = Object
instance XHtml10 MapContent where xhtml10 = Map
instance XHtml10 FormContent where xhtml10 = Form
instance XHtml10 FieldsetContent where xhtml10 = Fieldset
instance XHtml10 ButtonContent where xhtml10 = Button

class XHtml11 c where
    xhtml11 :: Node -> c

instance XHtml11 HtmlContent where xhtml11 = Html
instance XHtml11 FramesetContent where xhtml11 = Frameset

class XHtml12 c where
    xhtml12 :: Node -> c

instance XHtml12 BlockContent where xhtml12 = Block
instance XHtml12 AddressContent where xhtml12 = Address
instance XHtml12 ObjectContent where xhtml12 = Object
instance XHtml12 MapContent where xhtml12 = Map
instance XHtml12 FormContent where xhtml12 = Form
instance XHtml12 FieldsetContent where xhtml12 = Fieldset
instance XHtml12 ButtonContent where xhtml12 = Button

class XHtml13 c where
    xhtml13 :: Node -> c

instance XHtml13 BlockContent where xhtml13 = Block
instance XHtml13 InlineContent where xhtml13 = Inline
instance XHtml13 AddressContent where xhtml13 = Address
instance XHtml13 PreContent where xhtml13 = Pre
instance XHtml13 ObjectContent where xhtml13 = Object
instance XHtml13 FormContent where xhtml13 = Form
instance XHtml13 FieldsetContent where xhtml13 = Fieldset

class XHtml14 c where
    xhtml14 :: Node -> c

instance XHtml14 HeadContent where xhtml14 = Head
instance XHtml14 BlockContent where xhtml14 = Block
instance XHtml14 InlineContent where xhtml14 = Inline
instance XHtml14 AddressContent where xhtml14 = Address
instance XHtml14 AContent where xhtml14 = A
instance XHtml14 ObjectContent where xhtml14 = Object
instance XHtml14 FormContent where xhtml14 = Form
instance XHtml14 FieldsetContent where xhtml14 = Fieldset
instance XHtml14 ButtonContent where xhtml14 = Button

class XHtml15 c where
    xhtml15 :: Node -> c

instance XHtml15 BlockContent where xhtml15 = Block
instance XHtml15 ObjectContent where xhtml15 = Object
instance XHtml15 MapContent where xhtml15 = Map
instance XHtml15 FieldsetContent where xhtml15 = Fieldset

class XHtml16 c where
    xhtml16 :: Node -> c

instance XHtml16 BlockContent where xhtml16 = Block
instance XHtml16 ObjectContent where xhtml16 = Object
instance XHtml16 MapContent where xhtml16 = Map
instance XHtml16 FormContent where xhtml16 = Form
instance XHtml16 FieldsetContent where xhtml16 = Fieldset

class XHtml17 c where
    xhtml17 :: Node -> c

instance XHtml17 TableContent where xhtml17 = Table
instance XHtml17 TableSectionContent where xhtml17 = TableSection

newtype HtmlContent = Html { htmlToNode :: Node }

instance Content HtmlContent where
    toContent = htmlToNode

html' :: (Functor t, Monad t) => Attrs -> XHtmlT t HtmlContent -> XHtmlT t Root
html' = tellNode Root "html" []

html :: (Functor t, Monad t) => XHtmlT t HtmlContent -> XHtmlT t Root
html = tellNode Root "html" [] []

newtype HeadContent = Head { headToNode :: Node }

instance Content HeadContent where
    toContent = headToNode

head' :: (Functor t, Monad t) => Attrs -> XHtmlT t HeadContent -> XHtmlT t HtmlContent
head' = tellNode Html "head" []

head_ :: (Functor t, Monad t) => XHtmlT t HeadContent -> XHtmlT t HtmlContent
head_ = tellNode Html "head" [] []

title' :: (Functor t, Monad t) => Attrs -> Text -> XHtmlT t HeadContent
title' = tellTextNode Head "title" []

title :: (Functor t, Monad t) => Text -> XHtmlT t HeadContent
title = tellTextNode Head "title" [] []

style' :: (Functor t, Monad t)
       => Text -- ^ Required type attribute.
       -> Attrs -> Text -> XHtmlT t HeadContent
style' a = tellTextNode Head "style" [A.type_ a]

style :: (Functor t, Monad t)
      => Text -- ^ Required type attribute.
      -> Text -> XHtmlT t HeadContent
style a = tellTextNode Head "style" [A.type_ a] []

script' :: (Functor t, Monad t, XHtml1 c)
        => Text -- ^ Required type attribute.
        -> Attrs -> Text -> XHtmlT t c
script' a = tellTextNode xhtml1 "script" [A.type_ a]

script :: (Functor t, Monad t, XHtml1 c)
       => Text -- ^ Required type attribute.
       -> Text -> XHtmlT t c
script a = tellTextNode xhtml1 "script" [A.type_ a] []

option' :: (Functor t, Monad t, XHtml2 c) => Attrs -> Text -> XHtmlT t c
option' = tellTextNode xhtml2 "option" []

option :: (Functor t, Monad t, XHtml2 c) => Text -> XHtmlT t c
option = tellTextNode xhtml2 "option" [] []

textarea' :: (Functor t, Monad t, XHtml3 c)
          => Text -- ^ Required rows attribute.
          -> Text -- ^ Required cols attribute.
          -> Attrs -> Text -> XHtmlT t c
textarea' a b = tellTextNode xhtml3 "textarea" [A.rows a, A.cols b]

textarea :: (Functor t, Monad t, XHtml3 c)
         => Text -- ^ Required rows attribute.
         -> Text -- ^ Required cols attribute.
         -> Text -> XHtmlT t c
textarea a b = tellTextNode xhtml3 "textarea" [A.rows a, A.cols b] []

base' :: (Functor t, Monad t) => Attrs -> XHtmlT t HeadContent
base' = tellEmptyNode Head "base" []

base :: (Functor t, Monad t) => XHtmlT t HeadContent
base = tellEmptyNode Head "base" [] []

meta' :: (Functor t, Monad t)
      => Text -- ^ Required content attribute.
      -> Attrs -> XHtmlT t HeadContent
meta' a = tellEmptyNode Head "meta" [A.content a]

meta :: (Functor t, Monad t)
     => Text -- ^ Required content attribute.
     -> XHtmlT t HeadContent
meta a = tellEmptyNode Head "meta" [A.content a] []

link' :: (Functor t, Monad t) => Attrs -> XHtmlT t HeadContent
link' = tellEmptyNode Head "link" []

link :: (Functor t, Monad t) => XHtmlT t HeadContent
link = tellEmptyNode Head "link" [] []

frame' :: (Functor t, Monad t) => Attrs -> XHtmlT t FramesetContent
frame' = tellEmptyNode Frameset "frame" []

frame :: (Functor t, Monad t) => XHtmlT t FramesetContent
frame = tellEmptyNode Frameset "frame" [] []

hr' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t c
hr' = tellEmptyNode xhtml4 "hr" []

hr :: (Functor t, Monad t, XHtml4 c) => XHtmlT t c
hr = tellEmptyNode xhtml4 "hr" [] []

br' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t c
br' = tellEmptyNode xhtml5 "br" []

br :: (Functor t, Monad t, XHtml5 c) => XHtmlT t c
br = tellEmptyNode xhtml5 "br" [] []

basefont' :: (Functor t, Monad t, XHtml6 c)
          => Text -- ^ Required size attribute.
          -> Attrs -> XHtmlT t c
basefont' a = tellEmptyNode xhtml6 "basefont" [A.size a]

basefont :: (Functor t, Monad t, XHtml6 c)
         => Text -- ^ Required size attribute.
         -> XHtmlT t c
basefont a = tellEmptyNode xhtml6 "basefont" [A.size a] []

param' :: (Functor t, Monad t)
       => Text -- ^ Required name attribute.
       -> Attrs -> XHtmlT t ObjectContent
param' a = tellEmptyNode Object "param" [A.name a]

param :: (Functor t, Monad t)
      => Text -- ^ Required name attribute.
      -> XHtmlT t ObjectContent
param a = tellEmptyNode Object "param" [A.name a] []

img' :: (Functor t, Monad t, XHtml6 c)
     => Text -- ^ Required src attribute.
     -> Text -- ^ Required alt attribute.
     -> Attrs -> XHtmlT t c
img' a b = tellEmptyNode xhtml6 "img" [A.src a, A.alt b]

img :: (Functor t, Monad t, XHtml6 c)
    => Text -- ^ Required src attribute.
    -> Text -- ^ Required alt attribute.
    -> XHtmlT t c
img a b = tellEmptyNode xhtml6 "img" [A.src a, A.alt b] []

area' :: (Functor t, Monad t)
      => Text -- ^ Required alt attribute.
      -> Attrs -> XHtmlT t MapContent
area' a = tellEmptyNode Map "area" [A.alt a]

area :: (Functor t, Monad t)
     => Text -- ^ Required alt attribute.
     -> XHtmlT t MapContent
area a = tellEmptyNode Map "area" [A.alt a] []

input' :: (Functor t, Monad t, XHtml3 c) => Attrs -> XHtmlT t c
input' = tellEmptyNode xhtml3 "input" []

input :: (Functor t, Monad t, XHtml3 c) => XHtmlT t c
input = tellEmptyNode xhtml3 "input" [] []

isindex' :: (Functor t, Monad t, XHtml7 c) => Attrs -> XHtmlT t c
isindex' = tellEmptyNode xhtml7 "isindex" []

isindex :: (Functor t, Monad t, XHtml7 c) => XHtmlT t c
isindex = tellEmptyNode xhtml7 "isindex" [] []

col' :: (Functor t, Monad t, XHtml8 c) => Attrs -> XHtmlT t c
col' = tellEmptyNode xhtml8 "col" []

col :: (Functor t, Monad t, XHtml8 c) => XHtmlT t c
col = tellEmptyNode xhtml8 "col" [] []

newtype BlockContent = Block { blockToNode :: Node }

instance Content BlockContent where
    toContent = blockToNode

instance CData BlockContent where
    cdata = Block . TextNode

noscript' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t BlockContent -> XHtmlT t c
noscript' = tellNode xhtml4 "noscript" []

noscript :: (Functor t, Monad t, XHtml4 c) => XHtmlT t BlockContent -> XHtmlT t c
noscript = tellNode xhtml4 "noscript" [] []

iframe' :: (Functor t, Monad t, XHtml9 c) => Attrs -> XHtmlT t BlockContent -> XHtmlT t c
iframe' = tellNode xhtml9 "iframe" []

iframe :: (Functor t, Monad t, XHtml9 c) => XHtmlT t BlockContent -> XHtmlT t c
iframe = tellNode xhtml9 "iframe" [] []

body' :: (Functor t, Monad t) => Attrs -> XHtmlT t BlockContent -> XHtmlT t NoframesContent
body' = tellNode Noframes "body" []

body :: (Functor t, Monad t) => XHtmlT t BlockContent -> XHtmlT t NoframesContent
body = tellNode Noframes "body" [] []

div' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t BlockContent -> XHtmlT t c
div' = tellNode xhtml4 "div" []

div_ :: (Functor t, Monad t, XHtml4 c) => XHtmlT t BlockContent -> XHtmlT t c
div_ = tellNode xhtml4 "div" [] []

li' :: (Functor t, Monad t) => Attrs -> XHtmlT t BlockContent -> XHtmlT t ListContent
li' = tellNode List "li" []

li :: (Functor t, Monad t) => XHtmlT t BlockContent -> XHtmlT t ListContent
li = tellNode List "li" [] []

dd' :: (Functor t, Monad t) => Attrs -> XHtmlT t BlockContent -> XHtmlT t DlContent
dd' = tellNode Dl "dd" []

dd :: (Functor t, Monad t) => XHtmlT t BlockContent -> XHtmlT t DlContent
dd = tellNode Dl "dd" [] []

blockquote' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t BlockContent -> XHtmlT t c
blockquote' = tellNode xhtml4 "blockquote" []

blockquote :: (Functor t, Monad t, XHtml4 c) => XHtmlT t BlockContent -> XHtmlT t c
blockquote = tellNode xhtml4 "blockquote" [] []

center' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t BlockContent -> XHtmlT t c
center' = tellNode xhtml4 "center" []

center :: (Functor t, Monad t, XHtml4 c) => XHtmlT t BlockContent -> XHtmlT t c
center = tellNode xhtml4 "center" [] []

ins' :: (Functor t, Monad t, XHtml10 c) => Attrs -> XHtmlT t BlockContent -> XHtmlT t c
ins' = tellNode xhtml10 "ins" []

ins :: (Functor t, Monad t, XHtml10 c) => XHtmlT t BlockContent -> XHtmlT t c
ins = tellNode xhtml10 "ins" [] []

del' :: (Functor t, Monad t, XHtml10 c) => Attrs -> XHtmlT t BlockContent -> XHtmlT t c
del' = tellNode xhtml10 "del" []

del :: (Functor t, Monad t, XHtml10 c) => XHtmlT t BlockContent -> XHtmlT t c
del = tellNode xhtml10 "del" [] []

th' :: (Functor t, Monad t) => Attrs -> XHtmlT t BlockContent -> XHtmlT t TrContent
th' = tellNode Tr "th" []

th :: (Functor t, Monad t) => XHtmlT t BlockContent -> XHtmlT t TrContent
th = tellNode Tr "th" [] []

td' :: (Functor t, Monad t) => Attrs -> XHtmlT t BlockContent -> XHtmlT t TrContent
td' = tellNode Tr "td" []

td :: (Functor t, Monad t) => XHtmlT t BlockContent -> XHtmlT t TrContent
td = tellNode Tr "td" [] []

newtype FramesetContent = Frameset { framesetToNode :: Node }

instance Content FramesetContent where
    toContent = framesetToNode

frameset' :: (Functor t, Monad t, XHtml11 c) => Attrs -> XHtmlT t FramesetContent -> XHtmlT t c
frameset' = tellNode xhtml11 "frameset" []

frameset :: (Functor t, Monad t, XHtml11 c) => XHtmlT t FramesetContent -> XHtmlT t c
frameset = tellNode xhtml11 "frameset" [] []

newtype NoframesContent = Noframes { noframesToNode :: Node }

instance Content NoframesContent where
    toContent = noframesToNode

noframes' :: (Functor t, Monad t) => Attrs -> XHtmlT t NoframesContent -> XHtmlT t FramesetContent
noframes' = tellNode Frameset "noframes" []

noframes :: (Functor t, Monad t) => XHtmlT t NoframesContent -> XHtmlT t FramesetContent
noframes = tellNode Frameset "noframes" [] []

newtype InlineContent = Inline { inlineToNode :: Node }

instance Content InlineContent where
    toContent = inlineToNode

instance CData InlineContent where
    cdata = Inline . TextNode

p' :: (Functor t, Monad t, XHtml12 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
p' = tellNode xhtml12 "p" []

p :: (Functor t, Monad t, XHtml12 c) => XHtmlT t InlineContent -> XHtmlT t c
p = tellNode xhtml12 "p" [] []

h1' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
h1' = tellNode xhtml4 "h1" []

h1 :: (Functor t, Monad t, XHtml4 c) => XHtmlT t InlineContent -> XHtmlT t c
h1 = tellNode xhtml4 "h1" [] []

h2' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
h2' = tellNode xhtml4 "h2" []

h2 :: (Functor t, Monad t, XHtml4 c) => XHtmlT t InlineContent -> XHtmlT t c
h2 = tellNode xhtml4 "h2" [] []

h3' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
h3' = tellNode xhtml4 "h3" []

h3 :: (Functor t, Monad t, XHtml4 c) => XHtmlT t InlineContent -> XHtmlT t c
h3 = tellNode xhtml4 "h3" [] []

h4' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
h4' = tellNode xhtml4 "h4" []

h4 :: (Functor t, Monad t, XHtml4 c) => XHtmlT t InlineContent -> XHtmlT t c
h4 = tellNode xhtml4 "h4" [] []

h5' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
h5' = tellNode xhtml4 "h5" []

h5 :: (Functor t, Monad t, XHtml4 c) => XHtmlT t InlineContent -> XHtmlT t c
h5 = tellNode xhtml4 "h5" [] []

h6' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
h6' = tellNode xhtml4 "h6" []

h6 :: (Functor t, Monad t, XHtml4 c) => XHtmlT t InlineContent -> XHtmlT t c
h6 = tellNode xhtml4 "h6" [] []

dt' :: (Functor t, Monad t) => Attrs -> XHtmlT t InlineContent -> XHtmlT t DlContent
dt' = tellNode Dl "dt" []

dt :: (Functor t, Monad t) => XHtmlT t InlineContent -> XHtmlT t DlContent
dt = tellNode Dl "dt" [] []

span' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
span' = tellNode xhtml5 "span" []

span_ :: (Functor t, Monad t, XHtml5 c) => XHtmlT t InlineContent -> XHtmlT t c
span_ = tellNode xhtml5 "span" [] []

bdo' :: (Functor t, Monad t, XHtml5 c)
     => Text -- ^ Required dir attribute.
     -> Attrs -> XHtmlT t InlineContent -> XHtmlT t c
bdo' a = tellNode xhtml5 "bdo" [A.dir a]

bdo :: (Functor t, Monad t, XHtml5 c)
    => Text -- ^ Required dir attribute.
    -> XHtmlT t InlineContent -> XHtmlT t c
bdo a = tellNode xhtml5 "bdo" [A.dir a] []

em' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
em' = tellNode xhtml5 "em" []

em :: (Functor t, Monad t, XHtml5 c) => XHtmlT t InlineContent -> XHtmlT t c
em = tellNode xhtml5 "em" [] []

strong' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
strong' = tellNode xhtml5 "strong" []

strong :: (Functor t, Monad t, XHtml5 c) => XHtmlT t InlineContent -> XHtmlT t c
strong = tellNode xhtml5 "strong" [] []

dfn' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
dfn' = tellNode xhtml5 "dfn" []

dfn :: (Functor t, Monad t, XHtml5 c) => XHtmlT t InlineContent -> XHtmlT t c
dfn = tellNode xhtml5 "dfn" [] []

code' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
code' = tellNode xhtml5 "code" []

code :: (Functor t, Monad t, XHtml5 c) => XHtmlT t InlineContent -> XHtmlT t c
code = tellNode xhtml5 "code" [] []

samp' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
samp' = tellNode xhtml5 "samp" []

samp :: (Functor t, Monad t, XHtml5 c) => XHtmlT t InlineContent -> XHtmlT t c
samp = tellNode xhtml5 "samp" [] []

kbd' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
kbd' = tellNode xhtml5 "kbd" []

kbd :: (Functor t, Monad t, XHtml5 c) => XHtmlT t InlineContent -> XHtmlT t c
kbd = tellNode xhtml5 "kbd" [] []

var' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
var' = tellNode xhtml5 "var" []

var :: (Functor t, Monad t, XHtml5 c) => XHtmlT t InlineContent -> XHtmlT t c
var = tellNode xhtml5 "var" [] []

cite' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
cite' = tellNode xhtml5 "cite" []

cite :: (Functor t, Monad t, XHtml5 c) => XHtmlT t InlineContent -> XHtmlT t c
cite = tellNode xhtml5 "cite" [] []

abbr' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
abbr' = tellNode xhtml5 "abbr" []

abbr :: (Functor t, Monad t, XHtml5 c) => XHtmlT t InlineContent -> XHtmlT t c
abbr = tellNode xhtml5 "abbr" [] []

acronym' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
acronym' = tellNode xhtml5 "acronym" []

acronym :: (Functor t, Monad t, XHtml5 c) => XHtmlT t InlineContent -> XHtmlT t c
acronym = tellNode xhtml5 "acronym" [] []

q' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
q' = tellNode xhtml5 "q" []

q :: (Functor t, Monad t, XHtml5 c) => XHtmlT t InlineContent -> XHtmlT t c
q = tellNode xhtml5 "q" [] []

sub' :: (Functor t, Monad t, XHtml6 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
sub' = tellNode xhtml6 "sub" []

sub :: (Functor t, Monad t, XHtml6 c) => XHtmlT t InlineContent -> XHtmlT t c
sub = tellNode xhtml6 "sub" [] []

sup' :: (Functor t, Monad t, XHtml6 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
sup' = tellNode xhtml6 "sup" []

sup :: (Functor t, Monad t, XHtml6 c) => XHtmlT t InlineContent -> XHtmlT t c
sup = tellNode xhtml6 "sup" [] []

tt' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
tt' = tellNode xhtml5 "tt" []

tt :: (Functor t, Monad t, XHtml5 c) => XHtmlT t InlineContent -> XHtmlT t c
tt = tellNode xhtml5 "tt" [] []

i' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
i' = tellNode xhtml5 "i" []

i :: (Functor t, Monad t, XHtml5 c) => XHtmlT t InlineContent -> XHtmlT t c
i = tellNode xhtml5 "i" [] []

b' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
b' = tellNode xhtml5 "b" []

b :: (Functor t, Monad t, XHtml5 c) => XHtmlT t InlineContent -> XHtmlT t c
b = tellNode xhtml5 "b" [] []

big' :: (Functor t, Monad t, XHtml6 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
big' = tellNode xhtml6 "big" []

big :: (Functor t, Monad t, XHtml6 c) => XHtmlT t InlineContent -> XHtmlT t c
big = tellNode xhtml6 "big" [] []

small' :: (Functor t, Monad t, XHtml6 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
small' = tellNode xhtml6 "small" []

small :: (Functor t, Monad t, XHtml6 c) => XHtmlT t InlineContent -> XHtmlT t c
small = tellNode xhtml6 "small" [] []

u' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
u' = tellNode xhtml5 "u" []

u :: (Functor t, Monad t, XHtml5 c) => XHtmlT t InlineContent -> XHtmlT t c
u = tellNode xhtml5 "u" [] []

s' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
s' = tellNode xhtml5 "s" []

s :: (Functor t, Monad t, XHtml5 c) => XHtmlT t InlineContent -> XHtmlT t c
s = tellNode xhtml5 "s" [] []

strike' :: (Functor t, Monad t, XHtml5 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
strike' = tellNode xhtml5 "strike" []

strike :: (Functor t, Monad t, XHtml5 c) => XHtmlT t InlineContent -> XHtmlT t c
strike = tellNode xhtml5 "strike" [] []

font' :: (Functor t, Monad t, XHtml6 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
font' = tellNode xhtml6 "font" []

font :: (Functor t, Monad t, XHtml6 c) => XHtmlT t InlineContent -> XHtmlT t c
font = tellNode xhtml6 "font" [] []

label' :: (Functor t, Monad t, XHtml3 c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
label' = tellNode xhtml3 "label" []

label :: (Functor t, Monad t, XHtml3 c) => XHtmlT t InlineContent -> XHtmlT t c
label = tellNode xhtml3 "label" [] []

legend' :: (Functor t, Monad t) => Attrs -> XHtmlT t InlineContent -> XHtmlT t FieldsetContent
legend' = tellNode Fieldset "legend" []

legend :: (Functor t, Monad t) => XHtmlT t InlineContent -> XHtmlT t FieldsetContent
legend = tellNode Fieldset "legend" [] []

caption' :: (Functor t, Monad t) => Attrs -> XHtmlT t InlineContent -> XHtmlT t TableContent
caption' = tellNode Table "caption" []

caption :: (Functor t, Monad t) => XHtmlT t InlineContent -> XHtmlT t TableContent
caption = tellNode Table "caption" [] []

newtype ListContent = List { listToNode :: Node }

instance Content ListContent where
    toContent = listToNode

ul' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t ListContent -> XHtmlT t c
ul' = tellNode xhtml4 "ul" []

ul :: (Functor t, Monad t, XHtml4 c) => XHtmlT t ListContent -> XHtmlT t c
ul = tellNode xhtml4 "ul" [] []

ol' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t ListContent -> XHtmlT t c
ol' = tellNode xhtml4 "ol" []

ol :: (Functor t, Monad t, XHtml4 c) => XHtmlT t ListContent -> XHtmlT t c
ol = tellNode xhtml4 "ol" [] []

menu' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t ListContent -> XHtmlT t c
menu' = tellNode xhtml4 "menu" []

menu :: (Functor t, Monad t, XHtml4 c) => XHtmlT t ListContent -> XHtmlT t c
menu = tellNode xhtml4 "menu" [] []

dir' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t ListContent -> XHtmlT t c
dir' = tellNode xhtml4 "dir" []

dir :: (Functor t, Monad t, XHtml4 c) => XHtmlT t ListContent -> XHtmlT t c
dir = tellNode xhtml4 "dir" [] []

newtype DlContent = Dl { dlToNode :: Node }

instance Content DlContent where
    toContent = dlToNode

dl' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t DlContent -> XHtmlT t c
dl' = tellNode xhtml4 "dl" []

dl :: (Functor t, Monad t, XHtml4 c) => XHtmlT t DlContent -> XHtmlT t c
dl = tellNode xhtml4 "dl" [] []

newtype AddressContent = Address { addressToNode :: Node }

instance Content AddressContent where
    toContent = addressToNode

instance CData AddressContent where
    cdata = Address . TextNode

address' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t AddressContent -> XHtmlT t c
address' = tellNode xhtml4 "address" []

address :: (Functor t, Monad t, XHtml4 c) => XHtmlT t AddressContent -> XHtmlT t c
address = tellNode xhtml4 "address" [] []

newtype PreContent = Pre { preToNode :: Node }

instance Content PreContent where
    toContent = preToNode

instance CData PreContent where
    cdata = Pre . TextNode

pre' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t PreContent -> XHtmlT t c
pre' = tellNode xhtml4 "pre" []

pre :: (Functor t, Monad t, XHtml4 c) => XHtmlT t PreContent -> XHtmlT t c
pre = tellNode xhtml4 "pre" [] []

newtype AContent = A { aToNode :: Node }

instance Content AContent where
    toContent = aToNode

instance CData AContent where
    cdata = A . TextNode

a' :: (Functor t, Monad t, XHtml13 c) => Attrs -> XHtmlT t AContent -> XHtmlT t c
a' = tellNode xhtml13 "a" []

a :: (Functor t, Monad t, XHtml13 c) => XHtmlT t AContent -> XHtmlT t c
a = tellNode xhtml13 "a" [] []

newtype ObjectContent = Object { objectToNode :: Node }

instance Content ObjectContent where
    toContent = objectToNode

instance CData ObjectContent where
    cdata = Object . TextNode

object' :: (Functor t, Monad t, XHtml14 c) => Attrs -> XHtmlT t ObjectContent -> XHtmlT t c
object' = tellNode xhtml14 "object" []

object :: (Functor t, Monad t, XHtml14 c) => XHtmlT t ObjectContent -> XHtmlT t c
object = tellNode xhtml14 "object" [] []

applet' :: (Functor t, Monad t, XHtml6 c)
        => Text -- ^ Required width attribute.
        -> Text -- ^ Required height attribute.
        -> Attrs -> XHtmlT t ObjectContent -> XHtmlT t c
applet' a b = tellNode xhtml6 "applet" [A.width a, A.height b]

applet :: (Functor t, Monad t, XHtml6 c)
       => Text -- ^ Required width attribute.
       -> Text -- ^ Required height attribute.
       -> XHtmlT t ObjectContent -> XHtmlT t c
applet a b = tellNode xhtml6 "applet" [A.width a, A.height b] []

newtype MapContent = Map { mapToNode :: Node }

instance Content MapContent where
    toContent = mapToNode

map' :: (Functor t, Monad t, XHtml6 c)
     => Text -- ^ Required id attribute.
     -> Attrs -> XHtmlT t MapContent -> XHtmlT t c
map' a = tellNode xhtml6 "map" [A.id a]

map_ :: (Functor t, Monad t, XHtml6 c)
     => Text -- ^ Required id attribute.
     -> XHtmlT t MapContent -> XHtmlT t c
map_ a = tellNode xhtml6 "map" [A.id a] []

newtype FormContent = Form { formToNode :: Node }

instance Content FormContent where
    toContent = formToNode

instance CData FormContent where
    cdata = Form . TextNode

form' :: (Functor t, Monad t, XHtml15 c)
      => Text -- ^ Required action attribute.
      -> Attrs -> XHtmlT t FormContent -> XHtmlT t c
form' a = tellNode xhtml15 "form" [A.action a]

form :: (Functor t, Monad t, XHtml15 c)
     => Text -- ^ Required action attribute.
     -> XHtmlT t FormContent -> XHtmlT t c
form a = tellNode xhtml15 "form" [A.action a] []

newtype SelectContent = Select { selectToNode :: Node }

instance Content SelectContent where
    toContent = selectToNode

select' :: (Functor t, Monad t, XHtml3 c) => Attrs -> XHtmlT t SelectContent -> XHtmlT t c
select' = tellNode xhtml3 "select" []

select :: (Functor t, Monad t, XHtml3 c) => XHtmlT t SelectContent -> XHtmlT t c
select = tellNode xhtml3 "select" [] []

newtype OptgroupContent = Optgroup { optgroupToNode :: Node }

instance Content OptgroupContent where
    toContent = optgroupToNode

optgroup' :: (Functor t, Monad t)
          => Text -- ^ Required label attribute.
          -> Attrs -> XHtmlT t OptgroupContent -> XHtmlT t SelectContent
optgroup' a = tellNode Select "optgroup" [A.label a]

optgroup :: (Functor t, Monad t)
         => Text -- ^ Required label attribute.
         -> XHtmlT t OptgroupContent -> XHtmlT t SelectContent
optgroup a = tellNode Select "optgroup" [A.label a] []

newtype FieldsetContent = Fieldset { fieldsetToNode :: Node }

instance Content FieldsetContent where
    toContent = fieldsetToNode

instance CData FieldsetContent where
    cdata = Fieldset . TextNode

fieldset' :: (Functor t, Monad t, XHtml16 c) => Attrs -> XHtmlT t FieldsetContent -> XHtmlT t c
fieldset' = tellNode xhtml16 "fieldset" []

fieldset :: (Functor t, Monad t, XHtml16 c) => XHtmlT t FieldsetContent -> XHtmlT t c
fieldset = tellNode xhtml16 "fieldset" [] []

newtype ButtonContent = Button { buttonToNode :: Node }

instance Content ButtonContent where
    toContent = buttonToNode

instance CData ButtonContent where
    cdata = Button . TextNode

button' :: (Functor t, Monad t, XHtml3 c) => Attrs -> XHtmlT t ButtonContent -> XHtmlT t c
button' = tellNode xhtml3 "button" []

button :: (Functor t, Monad t, XHtml3 c) => XHtmlT t ButtonContent -> XHtmlT t c
button = tellNode xhtml3 "button" [] []

newtype TableContent = Table { tableToNode :: Node }

instance Content TableContent where
    toContent = tableToNode

table' :: (Functor t, Monad t, XHtml4 c) => Attrs -> XHtmlT t TableContent -> XHtmlT t c
table' = tellNode xhtml4 "table" []

table :: (Functor t, Monad t, XHtml4 c) => XHtmlT t TableContent -> XHtmlT t c
table = tellNode xhtml4 "table" [] []

newtype TableSectionContent = TableSection { tablesectionToNode :: Node }

instance Content TableSectionContent where
    toContent = tablesectionToNode

thead' :: (Functor t, Monad t) => Attrs -> XHtmlT t TableSectionContent -> XHtmlT t TableContent
thead' = tellNode Table "thead" []

thead :: (Functor t, Monad t) => XHtmlT t TableSectionContent -> XHtmlT t TableContent
thead = tellNode Table "thead" [] []

tfoot' :: (Functor t, Monad t) => Attrs -> XHtmlT t TableSectionContent -> XHtmlT t TableContent
tfoot' = tellNode Table "tfoot" []

tfoot :: (Functor t, Monad t) => XHtmlT t TableSectionContent -> XHtmlT t TableContent
tfoot = tellNode Table "tfoot" [] []

tbody' :: (Functor t, Monad t) => Attrs -> XHtmlT t TableSectionContent -> XHtmlT t TableContent
tbody' = tellNode Table "tbody" []

tbody :: (Functor t, Monad t) => XHtmlT t TableSectionContent -> XHtmlT t TableContent
tbody = tellNode Table "tbody" [] []

newtype ColgroupContent = Colgroup { colgroupToNode :: Node }

instance Content ColgroupContent where
    toContent = colgroupToNode

colgroup' :: (Functor t, Monad t) => Attrs -> XHtmlT t ColgroupContent -> XHtmlT t TableContent
colgroup' = tellNode Table "colgroup" []

colgroup :: (Functor t, Monad t) => XHtmlT t ColgroupContent -> XHtmlT t TableContent
colgroup = tellNode Table "colgroup" [] []

newtype TrContent = Tr { trToNode :: Node }

instance Content TrContent where
    toContent = trToNode

tr' :: (Functor t, Monad t, XHtml17 c) => Attrs -> XHtmlT t TrContent -> XHtmlT t c
tr' = tellNode xhtml17 "tr" []

tr :: (Functor t, Monad t, XHtml17 c) => XHtmlT t TrContent -> XHtmlT t c
tr = tellNode xhtml17 "tr" [] []