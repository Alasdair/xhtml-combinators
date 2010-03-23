module Text.XHtmlCombinators
    ( -- * Types
      -- ** XHtml
      XHtmlMT, XHtmlT, XHtml
    , Page
    , Content
    , Attrs, Attr (Attr)
      -- ** Element Types
    , TopLevelContent
    , HeadContent
    , Flow, FlowContent
    , Block, BlockContent
    , Inline, InlineContent
      -- *** Misc Element Types
    , ObjectContent
    , MapContent
      -- *** List Element Types
    , ListContent, DefinitionListContent
      -- *** Field Element Types
    , FieldSetContent
      -- *** Table Element Types
    , Table1Content, Table2Content, Table3Content
    , TableColContent
      -- * Combinators
    , html', html
    , text, empty
      -- ** Document Head
    , head', head_
    , title', title
    , base', base
    , meta', meta
    , link', link
    , style', style
    , script', script
    , noscript', noscript
      -- ** Document Body
    , body', body
    , div', div_
      -- ** Paragraphs
    , p', p
      -- ** Headings
    , h1', h1, h2', h2, h3', h3
    , h4', h4, h5', h5, h6', h6
      -- ** Lists
    , ul', ul
    , ol', ol
    , li', li
      -- *** Definition Lists
    , dl', dl
    , dt', dt
    , dd', dd
      -- ** Address
    , address', address
      -- ** Horizontal Rule
    , hr', hr
      -- ** Preformatted Text
    , pre', pre
      -- ** Block-like Quotes
    , blockquote', blockquote
      -- ** Inserted/Deleted Text
    , ins', ins, del', del
      -- ** The Anchor Element
    , a', a
      -- ** Inline Elements
    , span', span_
    , bdo', bdo
    , br', br
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
    , tt', tt
    , i', i
    , b', b
    , big', big
    , small', small
      -- ** Object
    , object', object, param', param
      -- ** Images
    , img', img 
      -- ** Client-side image maps
    , map', map_
    , area', area
      -- ** Forms
    , form', form
    , label', label
    , input', input
    , select', select
    , optgroup', optgroup
    , option', option
    , textarea', textarea
    , fieldset', fieldset
    , legend', legend
    , button', button
      -- ** Tables
    , table', table
    , caption', caption
    , thead', thead
    , tfoot', tfoot
    , tbody', tbody
    , colgroup', colgroup
    , col', col
    , tr', tr
    , th', th
    , td', td
      -- * Rendering
    , render
    ) where

import Text.XHtmlCombinators.Combinators
import Text.XHtmlCombinators.Internal
import Text.XHtmlCombinators.Render