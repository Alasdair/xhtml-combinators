> {-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

> -- |
> -- Module      : Text.XHtmlCombinators.Combinators
> -- Copyright   : (c) Alasdair Armstrong 2010
> -- License     : BSD-style
> -- Maintainer  : alasdair.armstrong@googlemail.com
> -- Stability   : experimental
> -- Portability : GHC
>
> module Text.XHtmlCombinators.Combinators where
>
> import Control.Applicative hiding (empty)
> import Data.Sequence (Seq)
> import qualified Data.Sequence as Seq
>
> import Data.Text (Text)
> import qualified Data.Text as T
>
> import Text.XHtmlCombinators.Internal

<!--
   Extensible HTML version 1.0 Strict DTD

   This is the same as HTML 4 Strict except for
   changes due to the differences between XML and SGML.

   Namespace = http://www.w3.org/1999/xhtml

   For further information, see: http://www.w3.org/TR/xhtml1

   Copyright (c) 1998-2002 W3C (MIT, INRIA, Keio),
   All Rights Reserved. 

   This DTD module is identified by the PUBLIC and SYSTEM identifiers:

   PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
   SYSTEM "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"

   $Revision: 1.1 $
   $Date: 2002/08/01 13:56:03 $

-->

> -- ================ Character mnemonic entities =========================-->

<!ENTITY % HTMLlat1 PUBLIC
   "-//W3C//ENTITIES Latin 1 for XHTML//EN"
   "xhtml-lat1.ent">
%HTMLlat1;

<!ENTITY % HTMLsymbol PUBLIC
   "-//W3C//ENTITIES Symbols for XHTML//EN"
   "xhtml-symbol.ent">
%HTMLsymbol;

<!ENTITY % HTMLspecial PUBLIC
   "-//W3C//ENTITIES Special for XHTML//EN"
   "xhtml-special.ent">
%HTMLspecial;

> -- ================== Imported Names ====================================-->

<!ENTITY % ContentType "CDATA">
    <!-- media type, as per [RFC2045] -->

<!ENTITY % ContentTypes "CDATA">
    <!-- comma-separated list of media types, as per [RFC2045] -->

<!ENTITY % Charset "CDATA">
    <!-- a character encoding, as per [RFC2045] -->

<!ENTITY % Charsets "CDATA">
    <!-- a space separated list of character encodings, as per [RFC2045] -->

<!ENTITY % LanguageCode "NMTOKEN">
    <!-- a language code, as per [RFC3066] -->

<!ENTITY % Character "CDATA">
    <!-- a single character, as per section 2.2 of [XML] -->

<!ENTITY % Number "CDATA">
    <!-- one or more digits -->

<!ENTITY % LinkTypes "CDATA">
    <!-- space-separated list of link types -->

<!ENTITY % MediaDesc "CDATA">
    <!-- single or comma-separated list of media descriptors -->

<!ENTITY % URI "CDATA">
    <!-- a Uniform Resource Identifier, see [RFC2396] -->

<!ENTITY % UriList "CDATA">
    <!-- a space separated list of Uniform Resource Identifiers -->

<!ENTITY % Datetime "CDATA">
    <!-- date and time information. ISO date format -->

<!ENTITY % Script "CDATA">
    <!-- script expression -->

<!ENTITY % StyleSheet "CDATA">
    <!-- style sheet data -->

<!ENTITY % Text "CDATA">
    <!-- used for titles etc. -->

<!ENTITY % Length "CDATA">
    <!-- nn for pixels or nn% for percentage length -->

<!ENTITY % MultiLength "CDATA">
    <!-- pixel, percentage, or relative -->

<!ENTITY % Pixels "CDATA">
    <!-- integer representing length in pixels -->

<!-- these are used for image maps -->

<!ENTITY % Shape "(rect|circle|poly|default)">

<!ENTITY % Coords "CDATA">
    <!-- comma separated list of lengths -->

> -- =================== Generic Attributes ===============================-->

<!-- core attributes common to most elements
  id       document-wide unique id
  class    space separated list of classes
  style    associated style info
  title    advisory title/amplification
-->
<!ENTITY % coreattrs
 "id          ID             #IMPLIED
  class       CDATA          #IMPLIED
  style       %StyleSheet;   #IMPLIED
  title       %Text;         #IMPLIED"
  >

<!-- internationalization attributes
  lang        language code (backwards compatible)
  xml:lang    language code (as per XML 1.0 spec)
  dir         direction for weak/neutral text
-->
<!ENTITY % i18n
 "lang        %LanguageCode; #IMPLIED
  xml:lang    %LanguageCode; #IMPLIED
  dir         (ltr|rtl)      #IMPLIED"
  >

<!-- attributes for common UI events
  onclick     a pointer button was clicked
  ondblclick  a pointer button was double clicked
  onmousedown a pointer button was pressed down
  onmouseup   a pointer button was released
  onmousemove a pointer was moved onto the element
  onmouseout  a pointer was moved away from the element
  onkeypress  a key was pressed and released
  onkeydown   a key was pressed down
  onkeyup     a key was released
-->
<!ENTITY % events
 "onclick     %Script;       #IMPLIED
  ondblclick  %Script;       #IMPLIED
  onmousedown %Script;       #IMPLIED
  onmouseup   %Script;       #IMPLIED
  onmouseover %Script;       #IMPLIED
  onmousemove %Script;       #IMPLIED
  onmouseout  %Script;       #IMPLIED
  onkeypress  %Script;       #IMPLIED
  onkeydown   %Script;       #IMPLIED
  onkeyup     %Script;       #IMPLIED"
  >

<!-- attributes for elements that can get the focus
  accesskey   accessibility key character
  tabindex    position in tabbing order
  onfocus     the element got the focus
  onblur      the element lost the focus
-->
<!ENTITY % focus
 "accesskey   %Character;    #IMPLIED
  tabindex    %Number;       #IMPLIED
  onfocus     %Script;       #IMPLIED
  onblur      %Script;       #IMPLIED"
  >

<!ENTITY % attrs "%coreattrs; %i18n; %events;">

> -- =================== Text Elements ====================================-->

<!ENTITY % special.pre
   "br | span | bdo | map">

<!ENTITY % special
   "%special.pre; | object | img ">

<!ENTITY % fontstyle "tt | i | b | big | small ">

<!ENTITY % phrase "em | strong | dfn | code | q |
                   samp | kbd | var | cite | abbr | acronym | sub | sup ">

<!ENTITY % inline.forms "input | select | textarea | label | button">

<!-- these can occur at block or inline level -->
<!ENTITY % misc.inline "ins | del | script">

<!-- these can only occur at block level -->
<!ENTITY % misc "noscript | %misc.inline;">

<!ENTITY % inline "a | %special; | %fontstyle; | %phrase; | %inline.forms;">

<!-- %Inline; covers inline or "text-level" elements -->
<!ENTITY % Inline "(#PCDATA | %inline; | %misc.inline;)*">

> class CData c where
>     cdata :: Text -> c
>
> text :: (Functor t, Monad t, CData c) => Text -> XHtmlT t c
> text = tellS . cdata
>
> newtype InlineContent = Inline { inlineToNode :: Node }
>
> instance Content InlineContent where 
>     toContent = inlineToNode
>
> instance CData InlineContent where
>     cdata = Inline . TextNode
> 
> class Inline c where
>     inline :: Node -> c
>
> instance Inline InlineContent where inline = Inline
> instance Inline FlowContent where inline = Flow

> -- ================== Block level elements ==============================-->

<!ENTITY % heading "h1|h2|h3|h4|h5|h6">
<!ENTITY % lists "ul | ol | dl">
<!ENTITY % blocktext "pre | hr | blockquote | address">

<!ENTITY % block
     "p | %heading; | div | %lists; | %blocktext; | fieldset | table">

<!ENTITY % Block "(%block; | form | %misc;)*">

> newtype BlockContent = Block { blockToNode :: Node }
>
> instance Content BlockContent where 
>     toContent = blockToNode
>
> class Block c where
>     block :: Node -> c
>
> instance Block BlockContent where block = Block
> instance Block FlowContent where block = Flow

<!-- %Flow; mixes block and inline and is used for list items etc. -->
<!ENTITY % Flow "(#PCDATA | %block; | form | %inline; | %misc;)*">

> newtype FlowContent = Flow { flowToNode :: Node }
>
> instance Content FlowContent where 
>     toContent = flowToNode
>
> instance CData FlowContent where
>     cdata = Flow . TextNode
>
> class Flow c where
>     flow :: Node -> c
>
> instance Flow BlockContent where flow = Block
> instance Flow InlineContent where flow = Inline
> instance Flow FlowContent where flow = Flow

> -- ================== Content models for exclusions =====================-->

<!-- a elements use %Inline; excluding a -->

<!ENTITY % a.content
   "(#PCDATA | %special; | %fontstyle; | %phrase; | %inline.forms; | %misc.inline;)*">

<!-- pre uses %Inline excluding big, small, sup or sup -->

<!ENTITY % pre.content
   "(#PCDATA | a | %fontstyle; | %phrase; | %special.pre; | %misc.inline;
      | %inline.forms;)*">

<!-- form uses %Block; excluding form -->

<!ENTITY % form.content "(%block; | %misc;)*">

<!-- button uses %Flow; but excludes a, form and form controls -->

<!ENTITY % button.content
   "(#PCDATA | p | %heading; | div | %lists; | %blocktext; |
    table | %special; | %fontstyle; | %phrase; | %misc;)*">

> -- ================ Document Structure ==================================-->

<!-- the namespace URI designates the document profile -->

<!ELEMENT html (head, body)>
<!ATTLIST html
  %i18n;
  id          ID             #IMPLIED
  xmlns       %URI;          #FIXED 'http://www.w3.org/1999/xhtml'
  >

> newtype Page = Page { pageToNode :: Node }
>
> newtype TopLevelContent = TopLevel { topLevelToNode :: Node }
>
> instance Content Page where toContent = pageToNode
>
> instance Content TopLevelContent where 
>     toContent = topLevelToNode

> xhtml10strict :: Text
> xhtml10strict = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\
>                 \ \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
>
> doctype :: (Functor t, Monad t) => XHtmlT t Page
> doctype = tellS . Page . TextNode $ xhtml10strict
>
> xmlDec :: (Functor t, Monad t) => XHtmlT t Page
> xmlDec = tellS . Page . TextNode $ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
>
> html' :: (Functor t, Monad t)
>       => Bool -- ^ True for XML declaration, false to omit.
>       -> Attrs -> XHtmlT t TopLevelContent -> XHtmlT t Page
> html' useXmlDec attrs x = do
>     if useXmlDec then xmlDec else empty
>     doctype 
>     tellNode Page "html" [Attr "xmlns" "http://www.w3.org/1999/xhtml"] attrs x
>
> html :: (Functor t, Monad t) => Bool -> XHtmlT t TopLevelContent -> XHtmlT t Page
> html useXmlDec = html' useXmlDec []

> -- ================ Document Head =======================================-->

<!ENTITY % head.misc "(script|style|meta|link|object)*">

> newtype HeadContent = Head { headToNode :: Node }
>
> instance Content HeadContent where 
>     toContent = headToNode

<!-- content model is %head.misc; combined with a single
     title and an optional base element in any order -->

<!ELEMENT head (%head.misc;,
     ((title, %head.misc;, (base, %head.misc;)?) |
      (base, %head.misc;, (title, %head.misc;))))>

<!ATTLIST head
  %i18n;
  id          ID             #IMPLIED
  profile     %URI;          #IMPLIED
  >

> head' :: (Functor t, Monad t) => Attrs -> XHtmlT t HeadContent -> XHtmlT t TopLevelContent
> head' = tellNode TopLevel "head" []
>
> head_ :: (Functor t, Monad t) => XHtmlT t HeadContent -> XHtmlT t TopLevelContent
> head_ = head' []

<!-- The title element is not considered part of the flow of text.
       It should be displayed, for example as the page header or
       window title. Exactly one title is required per document.
    -->
<!ELEMENT title (#PCDATA)>
<!ATTLIST title 
  %i18n;
  id          ID             #IMPLIED
  >

> title' :: (Functor t, Monad t) => Attrs -> Text -> XHtmlT t HeadContent
> title' = tellTextNode Head "title" []
>
> title :: (Functor t, Monad t) => Text -> XHtmlT t HeadContent
> title = title' []

<!-- document base URI -->

<!ELEMENT base EMPTY>
<!ATTLIST base
  href        %URI;          #REQUIRED
  id          ID             #IMPLIED
  >

> base' :: (Functor t, Monad t) => Text -> Attrs -> XHtmlT t HeadContent
> base' href = tellEmptyNode Head "base" [Attr "href" href]
>
> base :: (Functor t, Monad t) => Text -> XHtmlT t HeadContent
> base = flip base' []

<!-- generic metainformation -->
<!ELEMENT meta EMPTY>
<!ATTLIST meta
  %i18n;
  id          ID             #IMPLIED
  http-equiv  CDATA          #IMPLIED
  name        CDATA          #IMPLIED
  content     CDATA          #REQUIRED
  scheme      CDATA          #IMPLIED
  >

> meta' :: (Functor t, Monad t) => Text -- ^ Required content attribute.
>       -> Attrs -> XHtmlT t HeadContent
> meta' content = tellEmptyNode Head "meta" [Attr "content" content]
>
> meta :: (Functor t, Monad t) => Text -> XHtmlT t HeadContent
> meta = flip meta' []

<!--
  Relationship values can be used in principle:

   a) for document specific toolbars/menus when used
      with the link element in document head e.g.
        start, contents, previous, next, index, end, help
   b) to link to a separate style sheet (rel="stylesheet")
   c) to make a link to a script (rel="script")
   d) by stylesheets to control how collections of
      html nodes are rendered into printed documents
   e) to make a link to a printable version of this document
      e.g. a PostScript or PDF version (rel="alternate" media="print")
-->

<!ELEMENT link EMPTY>
<!ATTLIST link
  %attrs;
  charset     %Charset;      #IMPLIED
  href        %URI;          #IMPLIED
  hreflang    %LanguageCode; #IMPLIED
  type        %ContentType;  #IMPLIED
  rel         %LinkTypes;    #IMPLIED
  rev         %LinkTypes;    #IMPLIED
  media       %MediaDesc;    #IMPLIED
  >

> link' :: (Functor t, Monad t) => Attrs -> XHtmlT t HeadContent
> link' = tellEmptyNode Head "link" []
>
> link :: (Functor t, Monad t) => XHtmlT t HeadContent
> link = link' []
> -- ^ 'link' is a bit useless without any attributes, but it's 
> -- included anyway for consistency reasons. As are several
> -- other similar elements.

<!-- style info, which may include CDATA sections -->
<!ELEMENT style (#PCDATA)>
<!ATTLIST style
  %i18n;
  id          ID             #IMPLIED
  type        %ContentType;  #REQUIRED
  media       %MediaDesc;    #IMPLIED
  title       %Text;         #IMPLIED
  xml:space   (preserve)     #FIXED 'preserve'
  >

> style' :: (Functor t, Monad t) => Text -- ^ Required type attribute.
>        -> Attrs -> Text -> XHtmlT t HeadContent
> style' type_ = tellTextNode Head "style" [Attr "type" type_]
>
> style :: (Functor t, Monad t) => Text -> Text -> XHtmlT t HeadContent
> style = flip style' []

<!-- script statements, which may include CDATA sections -->
<!ELEMENT script (#PCDATA)>
<!ATTLIST script
  id          ID             #IMPLIED
  charset     %Charset;      #IMPLIED
  type        %ContentType;  #REQUIRED
  src         %URI;          #IMPLIED
  defer       (defer)        #IMPLIED
  xml:space   (preserve)     #FIXED 'preserve'
  >

> script' :: (Functor t, Monad t) 
>         => Text -- ^ Required type attribute.
>         -> Attrs -> Text -> XHtmlT t HeadContent
> script' type_ = tellTextNode Head "script" [Attr "type" type_]
>
> script :: (Functor t, Monad t) => Text -> Text -> XHtmlT t HeadContent
> script = flip script' []

<!-- alternate content container for non script-based rendering -->

<!ELEMENT noscript %Block;>
<!ATTLIST noscript
  %attrs;
  >

> noscript' :: (Functor t, Monad t, Block c) => Attrs -> XHtmlT t BlockContent -> XHtmlT t c
> noscript' = tellNode block "noscript" []
>
> noscript :: (Functor t, Monad t, Block c) => XHtmlT t BlockContent -> XHtmlT t c
> noscript = noscript' []

> -- =================== Document Body ====================================-->

<!ELEMENT body %Block;>
<!ATTLIST body
  %attrs;
  onload          %Script;   #IMPLIED
  onunload        %Script;   #IMPLIED
  >

> body' :: (Functor t, Monad t) => Attrs -> XHtmlT t BlockContent -> XHtmlT t TopLevelContent
> body' = tellNode TopLevel "body" []
>
> body :: (Functor t, Monad t) => XHtmlT t BlockContent -> XHtmlT t TopLevelContent
> body = body' [] 

<!ELEMENT div %Flow;>  <!-- generic language/style container -->
<!ATTLIST div
  %attrs;
  >

> div' :: (Functor t, Monad t, Block c) => Attrs -> XHtmlT t FlowContent -> XHtmlT t c
> div' = tellNode block "div" []
> 
> div_ :: (Functor t, Monad t, Block c) => XHtmlT t FlowContent -> XHtmlT t c
> div_ = div' []

> -- =================== Paragraphs =======================================-->

<!ELEMENT p %Inline;>
<!ATTLIST p
  %attrs;
  >

> p' :: (Functor t, Monad t, Block c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> p' = tellNode block "p" []
>
> p :: (Functor t, Monad t, Block c) => XHtmlT t InlineContent -> XHtmlT t c
> p = p' []

> -- =================== Headings =========================================-->

<!--
  There are six levels of headings from h1 (the most important)
  to h6 (the least important).
-->

<!ELEMENT h1  %Inline;>
<!ATTLIST h1
   %attrs;
   >

> h1' :: (Functor t, Monad t, Block c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> h1' = tellNode block "h1" []
>
> h1 :: (Functor t, Monad t, Block c) => XHtmlT t InlineContent -> XHtmlT t c
> h1 = h1' []

<!ELEMENT h2 %Inline;>
<!ATTLIST h2
   %attrs;
   >

> h2' :: (Functor t, Monad t, Block c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> h2' = tellNode block "h2" []
>
> h2 :: (Functor t, Monad t, Block c) => XHtmlT t InlineContent -> XHtmlT t c
> h2 = h2' []

<!ELEMENT h3 %Inline;>
<!ATTLIST h3
   %attrs;
   >

> h3' :: (Functor t, Monad t, Block c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> h3' = tellNode block "h3" []
>
> h3 :: (Functor t, Monad t, Block c) => XHtmlT t InlineContent -> XHtmlT t c
> h3 = h3' []

<!ELEMENT h4 %Inline;>
<!ATTLIST h4
   %attrs;
   >

> h4' :: (Functor t, Monad t, Block c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> h4' = tellNode block "h4" []
>
> h4 :: (Functor t, Monad t, Block c) => XHtmlT t InlineContent -> XHtmlT t c
> h4 = h4' []

<!ELEMENT h5 %Inline;>
<!ATTLIST h5
   %attrs;
   >

> h5' :: (Functor t, Monad t, Block c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> h5' = tellNode block "h5" []
>
> h5 :: (Functor t, Monad t, Block c) => XHtmlT t InlineContent -> XHtmlT t c
> h5 = h5' []

<!ELEMENT h6 %Inline;>
<!ATTLIST h6
   %attrs;
   >

> h6' :: (Functor t, Monad t, Block c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> h6' = tellNode block "h6" []
>
> h6 :: (Functor t, Monad t, Block c) => XHtmlT t InlineContent -> XHtmlT t c
> h6 = h6' []

> -- =================== Lists ============================================-->

<!-- Unordered list -->

> newtype ListContent = List { listToNode :: Node }
>
> instance Content ListContent where
>     toContent = listToNode

<!ELEMENT ul (li)+>
<!ATTLIST ul
  %attrs;
  >

> ul' :: (Functor t, Monad t, Block c) => Attrs -> XHtmlT t ListContent -> XHtmlT t c
> ul' = tellNode block "ul" []
>
> ul :: (Functor t, Monad t, Block c) => XHtmlT t ListContent -> XHtmlT t c
> ul = ul' []

<!-- Ordered (numbered) list -->

<!ELEMENT ol (li)+>
<!ATTLIST ol
  %attrs;
  >

> ol' :: (Functor t, Monad t, Block c) => Attrs -> XHtmlT t ListContent -> XHtmlT t c
> ol' = tellNode block "ol" []
>
> ol :: (Functor t, Monad t, Block c) => XHtmlT t ListContent -> XHtmlT t c
> ol = ol' []

<!-- list item -->

<!ELEMENT li %Flow;>
<!ATTLIST li
  %attrs;
  >

> li' :: (Functor t, Monad t) => Attrs -> XHtmlT t FlowContent -> XHtmlT t ListContent
> li' = tellNode List "li" []
>
> li :: (Functor t, Monad t) => XHtmlT t FlowContent -> XHtmlT t ListContent
> li = li' []

<!-- definition lists - dt for term, dd for its definition -->

> newtype DefinitionListContent = 
>     DefinitionList { definitionListToNode :: Node }
>
> instance Content DefinitionListContent where
>     toContent = definitionListToNode

<!ELEMENT dl (dt|dd)+>
<!ATTLIST dl
  %attrs;
  >

> dl' :: (Functor t, Monad t, Block c) => Attrs -> XHtmlT t DefinitionListContent -> XHtmlT t c
> dl' = tellNode block "dl" []
>
> dl :: (Functor t, Monad t, Block c) => XHtmlT t DefinitionListContent -> XHtmlT t c
> dl = dl' []

<!ELEMENT dt %Inline;>
<!ATTLIST dt
  %attrs;
  >

> dt' :: (Functor t, Monad t) => Attrs -> XHtmlT t InlineContent -> XHtmlT t DefinitionListContent
> dt' = tellNode DefinitionList "dt" []
>
> dt :: (Functor t, Monad t) => XHtmlT t InlineContent -> XHtmlT t DefinitionListContent
> dt = dt' []

<!ELEMENT dd %Flow;>
<!ATTLIST dd
  %attrs;
  >

> dd' :: (Functor t, Monad t) => Attrs -> XHtmlT t InlineContent -> XHtmlT t DefinitionListContent
> dd' = tellNode DefinitionList "dd" []
>
> dd :: (Functor t, Monad t) => XHtmlT t InlineContent -> XHtmlT t DefinitionListContent
> dd = dd' []

> -- =================== Address ==========================================-->

<!-- information on author -->

<!ELEMENT address %Inline;>
<!ATTLIST address
  %attrs;
  >

> address' :: (Functor t, Monad t, Block c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> address' = tellNode block "address" []
>
> address :: (Functor t, Monad t, Block c) => XHtmlT t InlineContent -> XHtmlT t c
> address = address' []

> -- =================== Horizontal Rule ==================================-->

<!ELEMENT hr EMPTY>
<!ATTLIST hr
  %attrs;
  >

> hr' :: (Functor t, Monad t, Block c) => Attrs -> XHtmlT t c
> hr' = tellEmptyNode block "hr" []
>
> hr :: (Functor t, Monad t, Block c) => XHtmlT t c
> hr = hr' []

> -- =================== Preformatted Text ================================-->

<!-- content is %Inline; excluding "img|object|big|small|sub|sup" -->

<!ELEMENT pre %pre.content;>
<!ATTLIST pre
  %attrs;
  xml:space (preserve) #FIXED 'preserve'
  >

> pre' :: (Functor t, Monad t, Block c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> pre' = tellNode block "pre" []
>
> pre :: (Functor t, Monad t, Block c) => XHtmlT t InlineContent -> XHtmlT t c
> pre = pre' []

> -- =================== Block-like Quotes ================================-->

<!ELEMENT blockquote %Block;>
<!ATTLIST blockquote
  %attrs;
  cite        %URI;          #IMPLIED
  >

> blockquote' :: (Functor t, Monad t, Block c) => Attrs -> XHtmlT t BlockContent -> XHtmlT t c
> blockquote' = tellNode block "blockquote" []
>
> blockquote :: (Functor t, Monad t, Block c) => XHtmlT t BlockContent -> XHtmlT t c
> blockquote = blockquote' []

> -- =================== Inserted/Deleted Text ============================-->

<!--
  ins/del are allowed in block and inline content, but its
  inappropriate to include block content within an ins element
  occurring in inline content.
-->
<!ELEMENT ins %Flow;>
<!ATTLIST ins
  %attrs;
  cite        %URI;          #IMPLIED
  datetime    %Datetime;     #IMPLIED
  >

> ins' :: (Functor t, Monad t) => (Flow c, Content c) => Attrs -> XHtmlT t c -> XHtmlT t c
> ins' = tellNode flow "ins" []
>
> ins :: (Functor t, Monad t) => (Flow c, Content c) => XHtmlT t c -> XHtmlT t c
> ins = ins' []

<!ELEMENT del %Flow;>
<!ATTLIST del
  %attrs;
  cite        %URI;          #IMPLIED
  datetime    %Datetime;     #IMPLIED
  >

> del' :: (Functor t, Monad t) => (Flow c, Content c) => Attrs -> XHtmlT t c -> XHtmlT t c
> del' = tellNode flow "del" []
>
> del :: (Functor t, Monad t) => (Flow c, Content c) => XHtmlT t c -> XHtmlT t c
> del = del' []

> -- ================== The Anchor Element ================================-->

<!-- content is %Inline; except that anchors shouldn't be nested -->

<!ELEMENT a %a.content;>
<!ATTLIST a
  %attrs;
  %focus;
  charset     %Charset;      #IMPLIED
  type        %ContentType;  #IMPLIED
  name        NMTOKEN        #IMPLIED
  href        %URI;          #IMPLIED
  hreflang    %LanguageCode; #IMPLIED
  rel         %LinkTypes;    #IMPLIED
  rev         %LinkTypes;    #IMPLIED
  shape       %Shape;        "rect"
  coords      %Coords;       #IMPLIED
  >

> a' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> a' = tellNode inline "a" []
>
> a :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> a = a' []

> -- ===================== Inline Elements ================================-->

<!ELEMENT span %Inline;> <!-- generic language/style container -->
<!ATTLIST span
  %attrs;
  >

> span' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> span' = tellNode inline "span" []
>
> span_ :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> span_ = span' []

<!ELEMENT bdo %Inline;>  <!-- I18N BiDi over-ride -->
<!ATTLIST bdo
  %coreattrs;
  %events;
  lang        %LanguageCode; #IMPLIED
  xml:lang    %LanguageCode; #IMPLIED
  dir         (ltr|rtl)      #REQUIRED
  >

> bdo' :: (Functor t, Monad t, Inline c) 
>      => Text -- ^ Required language direction code.
>      -> Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> bdo' dir = tellNode inline "bdo" [Attr "dir" dir]
>
> bdo :: (Functor t, Monad t, Inline c) => Text -> XHtmlT t InlineContent -> XHtmlT t c
> bdo = flip bdo' []

<!ELEMENT br EMPTY>   <!-- forced line break -->
<!ATTLIST br
  %coreattrs;
  >

> br' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t c
> br' = tellEmptyNode inline "br" []
>
> br :: (Functor t, Monad t, Inline c) => XHtmlT t c
> br = br' []

<!ELEMENT em %Inline;>   <!-- emphasis -->
<!ATTLIST em %attrs;>

> em' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> em' = tellNode inline "em" []
>
> em :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> em = em' []

<!ELEMENT strong %Inline;>   <!-- strong emphasis -->
<!ATTLIST strong %attrs;>

> strong' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> strong' = tellNode inline "strong" []
>
> strong :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> strong = strong' []

<!ELEMENT dfn %Inline;>   <!-- definitional -->
<!ATTLIST dfn %attrs;>

> dfn' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> dfn' = tellNode inline "dfn" []
>
> dfn :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> dfn = dfn' []

<!ELEMENT code %Inline;>   <!-- program code -->
<!ATTLIST code %attrs;>

> code' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> code' = tellNode inline "code" []
>
> code :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> code = code' []

<!ELEMENT samp %Inline;>   <!-- sample -->
<!ATTLIST samp %attrs;>

> samp' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> samp' = tellNode inline "samp" []
>
> samp :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> samp = samp' []

<!ELEMENT kbd %Inline;>  <!-- something user would type -->
<!ATTLIST kbd %attrs;>

> kbd' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> kbd' = tellNode inline "kbd" []
>
> kbd :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> kbd = kbd' []

<!ELEMENT var %Inline;>   <!-- variable -->
<!ATTLIST var %attrs;>

> var' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> var' = tellNode inline "var" []
>
> var :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> var = var' []

<!ELEMENT cite %Inline;>   <!-- citation -->
<!ATTLIST cite %attrs;>

> cite' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> cite' = tellNode inline "cite" []
>
> cite :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> cite = cite' []

<!ELEMENT abbr %Inline;>   <!-- abbreviation -->
<!ATTLIST abbr %attrs;>

> abbr' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> abbr' = tellNode inline "abbr" []
>
> abbr :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> abbr = abbr' []

<!ELEMENT acronym %Inline;>   <!-- acronym -->
<!ATTLIST acronym %attrs;>

> acronym' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> acronym' = tellNode inline "acronym" []
>
> acronym :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> acronym = acronym' []

<!ELEMENT q %Inline;>   <!-- inlined quote -->
<!ATTLIST q
  %attrs;
  cite        %URI;          #IMPLIED
  >

> q' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> q' = tellNode inline "q" []
>
> q :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> q = q' []

<!ELEMENT sub %Inline;> <!-- subscript -->
<!ATTLIST sub %attrs;>

> sub' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> sub' = tellNode inline "sub" []
>
> sub :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> sub = sub' []

<!ELEMENT sup %Inline;> <!-- superscript -->
<!ATTLIST sup %attrs;>

> sup' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> sup' = tellNode inline "sup" []
>
> sup :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> sup = sup' []

<!ELEMENT tt %Inline;>   <!-- fixed pitch font -->
<!ATTLIST tt %attrs;>

> tt' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> tt' = tellNode inline "tt" []
>
> tt :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> tt = tt' []

<!ELEMENT i %Inline;>   <!-- italic font -->
<!ATTLIST i %attrs;>

> i' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> i' = tellNode inline "i" []
>
> i :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> i = i' []

<!ELEMENT b %Inline;>   <!-- bold font -->
<!ATTLIST b %attrs;>

> b' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> b' = tellNode inline "b" []
>
> b :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> b = b' []

<!ELEMENT big %Inline;>   <!-- bigger font -->
<!ATTLIST big %attrs;>

> big' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> big' = tellNode inline "big" []
>
> big :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> big = big' []

<!ELEMENT small %Inline;>   <!-- smaller font -->
<!ATTLIST small %attrs;>

> small' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> small' = tellNode inline "small" []
>
> small :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> small = small' []

> -- ==================== Object ======================================-->

> newtype ObjectContent = Object { objectToNode :: Node }
>
> instance Content ObjectContent where 
>     toContent = objectToNode
>
> instance Flow ObjectContent where flow = Object
> instance Inline ObjectContent where inline = Object
> instance Block ObjectContent where block = Object

<!--
  object is used to embed objects as part of HTML pages.
  param elements should precede other content. Parameters
  can also be expressed as attribute/value pairs on the
  object element itself when brevity is desired.
-->

<!ELEMENT object (#PCDATA | param | %block; | form | %inline; | %misc;)*>
<!ATTLIST object
  %attrs;
  declare     (declare)      #IMPLIED
  classid     %URI;          #IMPLIED
  codebase    %URI;          #IMPLIED
  data        %URI;          #IMPLIED
  type        %ContentType;  #IMPLIED
  codetype    %ContentType;  #IMPLIED
  archive     %UriList;      #IMPLIED
  standby     %Text;         #IMPLIED
  height      %Length;       #IMPLIED
  width       %Length;       #IMPLIED
  usemap      %URI;          #IMPLIED
  name        NMTOKEN        #IMPLIED
  tabindex    %Number;       #IMPLIED
  >

> object' :: (Functor t, Monad t, Flow c) => Attrs -> XHtmlT t ObjectContent -> XHtmlT t c
> object' = tellNode flow "object" []
>
> object :: (Functor t, Monad t, Flow c) => XHtmlT t ObjectContent -> XHtmlT t c
> object = object' []

<!--
  param is used to supply a named property value.
  In XML it would seem natural to follow RDF and support an
  abbreviated syntax where the param elements are replaced
  by attribute value pairs on the object start tag.
-->
<!ELEMENT param EMPTY>
<!ATTLIST param
  id          ID             #IMPLIED
  name        CDATA          #IMPLIED
  value       CDATA          #IMPLIED
  valuetype   (data|ref|object) "data"
  type        %ContentType;  #IMPLIED
  >

> param' :: (Functor t, Monad t) => Attrs -> XHtmlT t ObjectContent
> param' = tellEmptyNode Object "param" []
>
> param :: (Functor t, Monad t) => XHtmlT t ObjectContent
> param = param' []

> -- =================== Images ===========================================-->

<!--
   To avoid accessibility problems for people who aren't
   able to see the image, you should provide a text
   description using the alt and longdesc attributes.
   In addition, avoid the use of server-side image maps.
   Note that in this DTD there is no name attribute. That
   is only available in the transitional and frameset DTD.
-->

<!ELEMENT img EMPTY>
<!ATTLIST img
  %attrs;
  src         %URI;          #REQUIRED
  alt         %Text;         #REQUIRED
  longdesc    %URI;          #IMPLIED
  height      %Length;       #IMPLIED
  width       %Length;       #IMPLIED
  usemap      %URI;          #IMPLIED
  ismap       (ismap)        #IMPLIED
  >

> img' :: (Functor t, Monad t, Flow c) 
>      => Text -- ^ Required src attribute. 
>      -> Text -- ^ Required alt attribute.
>      -> Attrs -> XHtmlT t c
> img' src alt = tellEmptyNode flow "img" []
>
> img :: (Functor t, Monad t, Flow c) => Text -> Text -> XHtmlT t c
> img src alt = img' src alt [Attr "src" src, Attr "alt" alt]

<!-- usemap points to a map element which may be in this document
  or an external document, although the latter is not widely supported -->

> -- ================== Client-side image maps ============================-->

> newtype MapContent = Map { mapToNode :: Node }
>
> instance Content MapContent where 
>     toContent = mapToNode
>
> instance Flow MapContent where flow = Map
> instance Inline MapContent where inline = Map
> instance Block MapContent where block = Map

<!-- These can be placed in the same document or grouped in a
     separate document although this isn't yet widely supported -->

<!ELEMENT map ((%block; | form | %misc;)+ | area+)>
<!ATTLIST map
  %i18n;
  %events;
  id          ID             #REQUIRED
  class       CDATA          #IMPLIED
  style       %StyleSheet;   #IMPLIED
  title       %Text;         #IMPLIED
  name        NMTOKEN        #IMPLIED
  >

> map' :: (Functor t, Monad t, Flow c) 
>      => Text -- ^ Required id attribute. 
>      -> Attrs -> XHtmlT t MapContent -> XHtmlT t c
> map' id = tellNode flow "map" [Attr "id" id]
>
> map_ :: (Functor t, Monad t, Flow c) => Text -> XHtmlT t MapContent -> XHtmlT t c
> map_ = flip map' []

<!ELEMENT area EMPTY>
<!ATTLIST area
  %attrs;
  %focus;
  shape       %Shape;        "rect"
  coords      %Coords;       #IMPLIED
  href        %URI;          #IMPLIED
  nohref      (nohref)       #IMPLIED
  alt         %Text;         #REQUIRED
  >

> area' :: (Functor t, Monad t) => Text -- ^ Required alt attribute.
>       -> Attrs -> XHtmlT t MapContent
> area' alt = tellEmptyNode Map "area" [Attr "alt" alt]
>
> area :: (Functor t, Monad t) => Text -> XHtmlT t MapContent
> area = flip area' []

> -- ================ Forms ===============================================-->

<!ELEMENT form %form.content;>   <!-- forms shouldn't be nested -->

<!ATTLIST form
  %attrs;
  action      %URI;          #REQUIRED
  method      (get|post)     "get"
  enctype     %ContentType;  "application/x-www-form-urlencoded"
  onsubmit    %Script;       #IMPLIED
  onreset     %Script;       #IMPLIED
  accept      %ContentTypes; #IMPLIED
  accept-charset %Charsets;  #IMPLIED
  >

> form' :: (Functor t, Monad t, Block c)
>       => Text -- ^ Required action attribute. 
>       -> Attrs -> XHtmlT t FlowContent -> XHtmlT t c
> form' action = tellNode block "form" [Attr "action" action]
>
> form :: (Functor t, Monad t, Block c) => Text -> XHtmlT t FlowContent -> XHtmlT t c
> form = flip form' []

<!--
  Each label must not contain more than ONE field
  Label elements shouldn't be nested.
-->
<!ELEMENT label %Inline;>
<!ATTLIST label
  %attrs;
  for         IDREF          #IMPLIED
  accesskey   %Character;    #IMPLIED
  onfocus     %Script;       #IMPLIED
  onblur      %Script;       #IMPLIED
  >

> label' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t InlineContent -> XHtmlT t c
> label' = tellNode inline "label" []
>
> label :: (Functor t, Monad t, Inline c) => XHtmlT t InlineContent -> XHtmlT t c
> label = label' []

<!ENTITY % InputType
  "(text | password | checkbox |
    radio | submit | reset |
    file | hidden | image | button)"
   >

<!-- the name attribute is required for all but submit & reset -->

<!ELEMENT input EMPTY>     <!-- form control -->
<!ATTLIST input
  %attrs;
  %focus;
  type        %InputType;    "text"
  name        CDATA          #IMPLIED
  value       CDATA          #IMPLIED
  checked     (checked)      #IMPLIED
  disabled    (disabled)     #IMPLIED
  readonly    (readonly)     #IMPLIED
  size        CDATA          #IMPLIED
  maxlength   %Number;       #IMPLIED
  src         %URI;          #IMPLIED
  alt         CDATA          #IMPLIED
  usemap      %URI;          #IMPLIED
  onselect    %Script;       #IMPLIED
  onchange    %Script;       #IMPLIED
  accept      %ContentTypes; #IMPLIED
  >

> input' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t c
> input' = tellEmptyNode inline "input" []
>
> input :: (Functor t, Monad t, Inline c) => XHtmlT t c
> input = input' []

<!ELEMENT select (optgroup|option)+>  <!-- option selector -->
<!ATTLIST select
  %attrs;
  name        CDATA          #IMPLIED
  size        %Number;       #IMPLIED
  multiple    (multiple)     #IMPLIED
  disabled    (disabled)     #IMPLIED
  tabindex    %Number;       #IMPLIED
  onfocus     %Script;       #IMPLIED
  onblur      %Script;       #IMPLIED
  onchange    %Script;       #IMPLIED
  >

> newtype OptionContent = Option { optionToNode :: Node }
>
> instance Content OptionContent where 
>     toContent = optionToNode
>
> select' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t OptionContent -> XHtmlT t c 
> select' = tellNode inline "select" []
>
> select :: (Functor t, Monad t, Inline c) => XHtmlT t OptionContent -> XHtmlT t c
> select = select' []

<!ELEMENT optgroup (option)+>   <!-- option group -->
<!ATTLIST optgroup
  %attrs;
  disabled    (disabled)     #IMPLIED
  label       %Text;         #REQUIRED
  >

> optgroup' :: (Functor t, Monad t) => Text -- ^ Required label attribute. 
>           -> Attrs -> XHtmlT t OptionContent -> XHtmlT t OptionContent
> optgroup' label = tellNode Option "optgroup" [Attr "label" label]
>
> optgroup :: (Functor t, Monad t) => Text -> XHtmlT t OptionContent -> XHtmlT t OptionContent
> optgroup = flip optgroup' []

<!ELEMENT option (#PCDATA)>     <!-- selectable choice -->
<!ATTLIST option
  %attrs;
  selected    (selected)     #IMPLIED
  disabled    (disabled)     #IMPLIED
  label       %Text;         #IMPLIED
  value       CDATA          #IMPLIED
  >

> option' :: (Functor t, Monad t) => Attrs -> Text -> XHtmlT t OptionContent
> option' = tellTextNode Option "option" []
>
> option :: (Functor t, Monad t) => Text -> XHtmlT t OptionContent
> option = option' []

<!ELEMENT textarea (#PCDATA)>     <!-- multi-line text field -->
<!ATTLIST textarea
  %attrs;
  %focus;
  name        CDATA          #IMPLIED
  rows        %Number;       #REQUIRED
  cols        %Number;       #REQUIRED
  disabled    (disabled)     #IMPLIED
  readonly    (readonly)     #IMPLIED
  onselect    %Script;       #IMPLIED
  onchange    %Script;       #IMPLIED
  >

> textarea' :: (Functor t, Monad t, Inline c)
>           => Int -- ^ Required rows attribute.
>           -> Int -- ^ Required cols attribute.
>           -> Attrs -> Text -> XHtmlT t c
> textarea' rows cols = tellTextNode inline "textarea" 
>                           [ Attr "rows" (T.pack (show rows))
>                           , Attr "cols" (T.pack (show cols))
>                           ]
> 
> textarea :: (Functor t, Monad t, Inline c) => Int -> Int -> Text -> XHtmlT t c
> textarea  rows cols = textarea' rows cols []

<!--
  The fieldset element is used to group form fields.
  Only one legend element should occur in the content
  and if present should only be preceded by whitespace.
-->
<!ELEMENT fieldset (#PCDATA | legend | %block; | form | %inline; | %misc;)*>
<!ATTLIST fieldset
  %attrs;
  >

> newtype FieldSetContent = FieldSet { fieldSetToNode :: Node }
>
> instance Content FieldSetContent where 
>     toContent = fieldSetToNode
>
> instance Flow FieldSetContent where flow = FieldSet
> instance Inline FieldSetContent where inline = FieldSet
> instance Block FieldSetContent where block = FieldSet

> fieldset' :: (Functor t, Monad t, Block c) => Attrs -> XHtmlT t FieldSetContent -> XHtmlT t c
> fieldset' = tellNode block "fieldset" []
>
> fieldset :: (Functor t, Monad t, Block c) => XHtmlT t FieldSetContent -> XHtmlT t c
> fieldset = fieldset' []

<!ELEMENT legend %Inline;>     <!-- fieldset label -->
<!ATTLIST legend
  %attrs;
  accesskey   %Character;    #IMPLIED
  >

> legend' :: (Functor t, Monad t) => Attrs -> XHtmlT t InlineContent -> XHtmlT t FieldSetContent 
> legend' = tellNode FieldSet "legend" []
>
> legend :: (Functor t, Monad t) => XHtmlT t InlineContent -> XHtmlT t FieldSetContent
> legend = legend' []

<!--
 Content is %Flow; excluding a, form and form controls
--> 
<!ELEMENT button %button.content;>  <!-- push button -->
<!ATTLIST button
  %attrs;
  %focus;
  name        CDATA          #IMPLIED
  value       CDATA          #IMPLIED
  type        (button|submit|reset) "submit"
  disabled    (disabled)     #IMPLIED
  >

> button' :: (Functor t, Monad t, Inline c) => Attrs -> XHtmlT t FlowContent -> XHtmlT t c
> button' = tellNode inline "button" []
>
> button :: (Functor t, Monad t, Inline c) => XHtmlT t FlowContent -> XHtmlT t c
> button = button' []

> -- ======================= Tables =======================================-->

<!-- Derived from IETF HTML table standard, see [RFC1942] -->

<!--
 The border attribute sets the thickness of the frame around the
 table. The default units are screen pixels.

 The frame attribute specifies which parts of the frame around
 the table should be rendered. The values are not the same as
 CALS to avoid a name clash with the valign attribute.
-->
<!ENTITY % TFrame "(void|above|below|hsides|lhs|rhs|vsides|box|border)">

<!--
 The rules attribute defines which rules to draw between cells:

 If rules is absent then assume:
     "none" if border is absent or border="0" otherwise "all"
-->

<!ENTITY % TRules "(none | groups | rows | cols | all)">
  
<!-- horizontal alignment attributes for cell contents

  char        alignment char, e.g. char=':'
  charoff     offset for alignment char
-->
<!ENTITY % cellhalign
  "align      (left|center|right|justify|char) #IMPLIED
   char       %Character;    #IMPLIED
   charoff    %Length;       #IMPLIED"
  >

<!-- vertical alignment attributes for cell contents -->
<!ENTITY % cellvalign
  "valign     (top|middle|bottom|baseline) #IMPLIED"
  >

> newtype Table1Content = Table1 { table1ToNode :: Node }
>
> instance Content Table1Content where 
>     toContent = table1ToNode

> newtype Table2Content = Table2 { table2ToNode :: Node }
>
> instance Content Table2Content where 
>     toContent = table2ToNode
>
> newtype Table3Content = Table3 { table3ToNode :: Node }
>
> instance Content Table3Content where 
>     toContent = table3ToNode
>
> newtype TableColContent = TableCol { tableColToNode :: Node }
>
> instance Content TableColContent where 
>     toContent = tableColToNode

<!ELEMENT table
     (caption?, (col*|colgroup*), thead?, tfoot?, (tbody+|tr+))>

> table' :: (Functor t, Monad t, Block c) => Attrs -> XHtmlT t Table1Content -> XHtmlT t c
> table' = tellNode block "table" []
> 
> table :: (Functor t, Monad t, Block c) => XHtmlT t Table1Content -> XHtmlT t c
> table = table' []

<!ELEMENT caption  %Inline;>

> caption' :: (Functor t, Monad t) => Attrs -> XHtmlT t InlineContent -> XHtmlT t Table1Content
> caption' = tellNode Table1 "caption" []
>
> caption :: (Functor t, Monad t) => XHtmlT t InlineContent -> XHtmlT t Table1Content
> caption = caption' []

<!ELEMENT thead    (tr)+>

> thead' :: (Functor t, Monad t) => Attrs -> XHtmlT t Table2Content -> XHtmlT t Table1Content
> thead' = tellNode Table1 "thead" []
>
> thead :: (Functor t, Monad t) => XHtmlT t Table2Content -> XHtmlT t Table1Content
> thead = thead' []

<!ELEMENT tfoot    (tr)+>

> tfoot' :: (Functor t, Monad t) => Attrs -> XHtmlT t Table2Content -> XHtmlT t Table1Content
> tfoot' = tellNode Table1 "tfoot" []
>
> tfoot :: (Functor t, Monad t) => XHtmlT t Table2Content -> XHtmlT t Table1Content
> tfoot = tfoot' []

<!ELEMENT tbody    (tr)+>

> tbody' :: (Functor t, Monad t) => Attrs -> XHtmlT t Table2Content -> XHtmlT t Table1Content
> tbody' = tellNode Table1 "tbody" []
>
> tbody :: (Functor t, Monad t) => XHtmlT t Table2Content -> XHtmlT t Table1Content
> tbody = tbody' []

<!ELEMENT colgroup (col)*>

> colgroup' :: (Functor t, Monad t) => Attrs -> XHtmlT t TableColContent -> XHtmlT t Table1Content
> colgroup' = tellNode Table1 "colgroup" []
>
> colgroup :: (Functor t, Monad t) => XHtmlT t TableColContent -> XHtmlT t Table1Content
> colgroup = colgroup' []

<!ELEMENT col      EMPTY>

> col' :: (Functor t, Monad t) => Attrs -> XHtmlT t TableColContent
> col' = tellEmptyNode TableCol "col" []
>
> col :: (Functor t, Monad t) => XHtmlT t TableColContent
> col = col' []

<!ELEMENT tr       (th|td)+>

> tr' :: (Functor t, Monad t) => Attrs -> XHtmlT t Table3Content -> XHtmlT t Table2Content
> tr' = tellNode Table2 "tr" []
> 
> tr :: (Functor t, Monad t) => XHtmlT t Table3Content -> XHtmlT t Table2Content
> tr = tr' []

<!ELEMENT th       %Flow;>

> th' :: (Functor t, Monad t) => Attrs -> XHtmlT t FlowContent -> XHtmlT t Table3Content
> th' = tellNode Table3 "th" []
>
> th :: (Functor t, Monad t) => XHtmlT t FlowContent -> XHtmlT t Table3Content
> th = th' []

<!ELEMENT td       %Flow;>

> td' :: (Functor t, Monad t) => Attrs -> XHtmlT t FlowContent -> XHtmlT t Table3Content
> td' = tellNode Table3 "td" []
>
> td :: (Functor t, Monad t) => XHtmlT t FlowContent -> XHtmlT t Table3Content
> td = td' []

<!ATTLIST table
  %attrs;
  summary     %Text;         #IMPLIED
  width       %Length;       #IMPLIED
  border      %Pixels;       #IMPLIED
  frame       %TFrame;       #IMPLIED
  rules       %TRules;       #IMPLIED
  cellspacing %Length;       #IMPLIED
  cellpadding %Length;       #IMPLIED
  >

<!ATTLIST caption
  %attrs;
  >

<!--
colgroup groups a set of col elements. It allows you to group
several semantically related columns together.
-->
<!ATTLIST colgroup
  %attrs;
  span        %Number;       "1"
  width       %MultiLength;  #IMPLIED
  %cellhalign;
  %cellvalign;
  >

<!--
 col elements define the alignment properties for cells in
 one or more columns.

 The width attribute specifies the width of the columns, e.g.

     width=64        width in screen pixels
     width=0.5*      relative width of 0.5

 The span attribute causes the attributes of one
 col element to apply to more than one column.
-->
<!ATTLIST col
  %attrs;
  span        %Number;       "1"
  width       %MultiLength;  #IMPLIED
  %cellhalign;
  %cellvalign;
  >

<!--
    Use thead to duplicate headers when breaking table
    across page boundaries, or for static headers when
    tbody sections are rendered in scrolling panel.

    Use tfoot to duplicate footers when breaking table
    across page boundaries, or for static footers when
    tbody sections are rendered in scrolling panel.

    Use multiple tbody sections when rules are needed
    between groups of table rows.
-->
<!ATTLIST thead
  %attrs;
  %cellhalign;
  %cellvalign;
  >

<!ATTLIST tfoot
  %attrs;
  %cellhalign;
  %cellvalign;
  >

<!ATTLIST tbody
  %attrs;
  %cellhalign;
  %cellvalign;
  >

<!ATTLIST tr
  %attrs;
  %cellhalign;
  %cellvalign;
  >


<!-- Scope is simpler than headers attribute for common tables -->
<!ENTITY % Scope "(row|col|rowgroup|colgroup)">

<!-- th is for headers, td for data and for cells acting as both -->

<!ATTLIST th
  %attrs;
  abbr        %Text;         #IMPLIED
  axis        CDATA          #IMPLIED
  headers     IDREFS         #IMPLIED
  scope       %Scope;        #IMPLIED
  rowspan     %Number;       "1"
  colspan     %Number;       "1"
  %cellhalign;
  %cellvalign;
  >

<!ATTLIST td
  %attrs;
  abbr        %Text;         #IMPLIED
  axis        CDATA          #IMPLIED
  headers     IDREFS         #IMPLIED
  scope       %Scope;        #IMPLIED
  rowspan     %Number;       "1"
  colspan     %Number;       "1"
  %cellhalign;
  %cellvalign;
  >
