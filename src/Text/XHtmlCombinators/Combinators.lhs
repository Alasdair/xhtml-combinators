> {-# LANGUAGE OverloadedStrings #-}
> module Text.XHtmlCombinators.Combinators where
>
> import Control.Applicative hiding (empty)
> import Data.Text (Text)
> import qualified Data.Sequence as Seq
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

<!--================ Character mnemonic entities =========================-->

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

<!--================== Imported Names ====================================-->

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

<!--=================== Generic Attributes ===============================-->

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

<!--=================== Text Elements ====================================-->

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
> text :: CData c => Text -> XHtml c
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

<!--================== Block level elements ==============================-->

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

<!--================== Content models for exclusions =====================-->

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

<!--================ Document Structure ==================================-->

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
> doctype = tellS . Page . TextNode $ xhtml10strict
>
> xmlDec = tellS . Page . TextNode $ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
>
> html' :: Bool -- ^ True for XML declaration, false to omit.
>      -> Attrs -> XHtml TopLevelContent -> XHtml Page
> html' useXmlDec attrs x = do
>     if useXmlDec then xmlDec else empty
>     doctype 
>     tellNode Page "html" [Attr "xmlns" "http://www.w3.org/1999/xhtml"] attrs x
>
> html :: Bool -> XHtml TopLevelContent -> XHtml Page
> html useXmlDec = html' useXmlDec []

<!--================ Document Head =======================================-->

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

> head' :: Attrs -> XHtml HeadContent -> XHtml TopLevelContent
> head' = tellNode TopLevel "head" []
>
> head_ :: XHtml HeadContent -> XHtml TopLevelContent
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

> title' :: Attrs -> Text -> XHtml HeadContent
> title' = tellTextNode Head "title" []
>
> title :: Text -> XHtml HeadContent
> title = title' []

<!-- document base URI -->

<!ELEMENT base EMPTY>
<!ATTLIST base
  href        %URI;          #REQUIRED
  id          ID             #IMPLIED
  >

> base' :: Text -> Attrs -> XHtml HeadContent
> base' href = tellEmptyNode Head "base" [Attr "href" href]
>
> base :: Text -> XHtml HeadContent
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

> meta' :: Text -- ^ Required content attribute.
>       -> Attrs -> XHtml HeadContent
> meta' content = tellEmptyNode Head "meta" [Attr "content" content]
>
> meta :: Text -> XHtml HeadContent
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

> link' :: Attrs -> XHtml HeadContent
> link' = tellEmptyNode Head "link" []
>
> link :: XHtml HeadContent
> link = link' []
> -- ^ 'link' is a bit useless without any attributes, but it's 
> -- included anyway for consistency reasons.

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

> style' :: Text -- ^ Required type attribute.
>        -> Attrs -> Text -> XHtml HeadContent
> style' sType = tellTextNode Head "style" [Attr "type" sType]
>
> style :: Text -> Text -> XHtml HeadContent
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

> script' :: Text -- ^ Required type attribute.
>         -> Attrs -> Text -> XHtml HeadContent
> script' sType = tellTextNode Head "script" [Attr "type" sType]
>
> script :: Text -> Text -> XHtml HeadContent
> script = flip script' []

<!-- alternate content container for non script-based rendering -->

<!ELEMENT noscript %Block;>
<!ATTLIST noscript
  %attrs;
  >

> noscript' :: Block c => Attrs -> XHtml BlockContent -> XHtml c
> noscript' = tellNode block "noscript" []
>
> noscript :: Block c => XHtml BlockContent -> XHtml c
> noscript = noscript' []

<!--=================== Document Body ====================================-->

<!ELEMENT body %Block;>
<!ATTLIST body
  %attrs;
  onload          %Script;   #IMPLIED
  onunload        %Script;   #IMPLIED
  >

> body' :: Attrs -> XHtml BlockContent -> XHtml TopLevelContent
> body' = tellNode TopLevel "body" []
>
> body :: XHtml BlockContent -> XHtml TopLevelContent
> body = body' [] 

<!ELEMENT div %Flow;>  <!-- generic language/style container -->
<!ATTLIST div
  %attrs;
  >

> div' :: Block c => Attrs -> XHtml FlowContent -> XHtml c
> div' = tellNode block "div" []
> 
> div_ :: Block c => XHtml FlowContent -> XHtml c
> div_ = div' []

<!--=================== Paragraphs =======================================-->

<!ELEMENT p %Inline;>
<!ATTLIST p
  %attrs;
  >

> p' :: Block c => Attrs -> XHtml InlineContent -> XHtml c
> p' = tellNode block "p" []
>
> p :: Block c => XHtml InlineContent -> XHtml c
> p = p' []

<!--=================== Headings =========================================-->

<!--
  There are six levels of headings from h1 (the most important)
  to h6 (the least important).
-->

<!ELEMENT h1  %Inline;>
<!ATTLIST h1
   %attrs;
   >

> h1' :: Block c => Attrs -> XHtml InlineContent -> XHtml c
> h1' = tellNode block "h1" []
>
> h1 :: Block c => XHtml InlineContent -> XHtml c
> h1 = h1' []

<!ELEMENT h2 %Inline;>
<!ATTLIST h2
   %attrs;
   >

> h2' :: Block c => Attrs -> XHtml InlineContent -> XHtml c
> h2' = tellNode block "h2" []
>
> h2 :: Block c => XHtml InlineContent -> XHtml c
> h2 = h2' []

<!ELEMENT h3 %Inline;>
<!ATTLIST h3
   %attrs;
   >

> h3' :: Block c => Attrs -> XHtml InlineContent -> XHtml c
> h3' = tellNode block "h3" []
>
> h3 :: Block c => XHtml InlineContent -> XHtml c
> h3 = h3' []

<!ELEMENT h4 %Inline;>
<!ATTLIST h4
   %attrs;
   >

> h4' :: Block c => Attrs -> XHtml InlineContent -> XHtml c
> h4' = tellNode block "h4" []
>
> h4 :: Block c => XHtml InlineContent -> XHtml c
> h4 = h4' []

<!ELEMENT h5 %Inline;>
<!ATTLIST h5
   %attrs;
   >

> h5' :: Block c => Attrs -> XHtml InlineContent -> XHtml c
> h5' = tellNode block "h5" []
>
> h5 :: Block c => XHtml InlineContent -> XHtml c
> h5 = h5' []

<!ELEMENT h6 %Inline;>
<!ATTLIST h6
   %attrs;
   >

> h6' :: Block c => Attrs -> XHtml InlineContent -> XHtml c
> h6' = tellNode block "h6" []
>
> h6 :: Block c => XHtml InlineContent -> XHtml c
> h6 = h6' []

<!--=================== Lists ============================================-->

<!-- Unordered list -->

> newtype ListContent = List { listToNode :: Node }
>
> instance Content ListContent where
>     toContent = listToNode

<!ELEMENT ul (li)+>
<!ATTLIST ul
  %attrs;
  >

> ul' :: Block c => Attrs -> XHtml ListContent -> XHtml c
> ul' = tellNode block "ul" []
>
> ul :: Block c => XHtml ListContent -> XHtml c
> ul = ul' []

<!-- Ordered (numbered) list -->

<!ELEMENT ol (li)+>
<!ATTLIST ol
  %attrs;
  >

> ol' :: Block c => Attrs -> XHtml ListContent -> XHtml c
> ol' = tellNode block "ol" []
>
> ol :: Block c => XHtml ListContent -> XHtml c
> ol = ol' []

<!-- list item -->

<!ELEMENT li %Flow;>
<!ATTLIST li
  %attrs;
  >

> li' :: Attrs -> XHtml FlowContent -> XHtml ListContent
> li' = tellNode List "li" []
>
> li :: XHtml FlowContent -> XHtml ListContent
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

> dl' :: Block c => Attrs -> XHtml DefinitionListContent -> XHtml c
> dl' = tellNode block "dl" []
>
> dl :: Block c => XHtml DefinitionListContent -> XHtml c
> dl = dl' []

<!ELEMENT dt %Inline;>
<!ATTLIST dt
  %attrs;
  >

> dt' :: Attrs -> XHtml InlineContent -> XHtml DefinitionListContent
> dt' = tellNode DefinitionList "dt" []
>
> dt :: XHtml InlineContent -> XHtml DefinitionListContent
> dt = dt' []

<!ELEMENT dd %Flow;>
<!ATTLIST dd
  %attrs;
  >

> dd' :: Attrs -> XHtml InlineContent -> XHtml DefinitionListContent
> dd' = tellNode DefinitionList "dd" []
>
> dd :: XHtml InlineContent -> XHtml DefinitionListContent
> dd = dd' []

<!--=================== Address ==========================================-->

<!-- information on author -->

<!ELEMENT address %Inline;>
<!ATTLIST address
  %attrs;
  >

> address' :: Block c => Attrs -> XHtml InlineContent -> XHtml c
> address' = tellNode block "address" []
>
> address :: Block c => XHtml InlineContent -> XHtml c
> address = address' []

<!--=================== Horizontal Rule ==================================-->

<!ELEMENT hr EMPTY>
<!ATTLIST hr
  %attrs;
  >

> hr' :: Block c => Attrs -> XHtml c
> hr' = tellEmptyNode block "hr" []
>
> hr :: Block c => XHtml c
> hr = hr' []

<!--=================== Preformatted Text ================================-->

<!-- content is %Inline; excluding "img|object|big|small|sub|sup" -->

<!ELEMENT pre %pre.content;>
<!ATTLIST pre
  %attrs;
  xml:space (preserve) #FIXED 'preserve'
  >

> pre' :: Block c => Attrs -> XHtml InlineContent -> XHtml c
> pre' = tellNode block "pre" []
>
> pre :: Block c => XHtml InlineContent -> XHtml c
> pre = pre' []

<!--=================== Block-like Quotes ================================-->

<!ELEMENT blockquote %Block;>
<!ATTLIST blockquote
  %attrs;
  cite        %URI;          #IMPLIED
  >

> blockquote' :: Block c => Attrs -> XHtml BlockContent -> XHtml c
> blockquote' = tellNode block "blockquote" []
>
> blockquote :: Block c => XHtml BlockContent -> XHtml c
> blockquote = blockquote' []

<!--=================== Inserted/Deleted Text ============================-->

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

> ins' :: (Flow c, Content c) => Attrs -> XHtml c -> XHtml c
> ins' = tellNode flow "ins" []
>
> ins :: (Flow c, Content c) => XHtml c -> XHtml c
> ins = ins' []

<!ELEMENT del %Flow;>
<!ATTLIST del
  %attrs;
  cite        %URI;          #IMPLIED
  datetime    %Datetime;     #IMPLIED
  >

> del' :: (Flow c, Content c) => Attrs -> XHtml c -> XHtml c
> del' = tellNode flow "del" []
>
> del :: (Flow c, Content c) => XHtml c -> XHtml c
> del = del' []

<!--================== The Anchor Element ================================-->

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

> a' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> a' = tellNode inline "a" []
>
> a :: Inline c => XHtml InlineContent -> XHtml c
> a = a' []

<!--===================== Inline Elements ================================-->

<!ELEMENT span %Inline;> <!-- generic language/style container -->
<!ATTLIST span
  %attrs;
  >

> span' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> span' = tellNode inline "span" []
>
> span_ :: Inline c => XHtml InlineContent -> XHtml c
> span_ = span' []

<!ELEMENT bdo %Inline;>  <!-- I18N BiDi over-ride -->
<!ATTLIST bdo
  %coreattrs;
  %events;
  lang        %LanguageCode; #IMPLIED
  xml:lang    %LanguageCode; #IMPLIED
  dir         (ltr|rtl)      #REQUIRED
  >

> bdo' :: Inline c 
>      => Text -- ^ Required language direction code.
>      -> Attrs -> XHtml InlineContent -> XHtml c
> bdo' dir = tellNode inline "bdo" [Attr "dir" dir]
>
> bdo :: Inline c => Text -> XHtml InlineContent -> XHtml c
> bdo = flip bdo' []

<!ELEMENT br EMPTY>   <!-- forced line break -->
<!ATTLIST br
  %coreattrs;
  >

> br' :: Inline c => Attrs -> XHtml c
> br' = tellEmptyNode inline "br" []
>
> br :: Inline c => XHtml c
> br = br' []

<!ELEMENT em %Inline;>   <!-- emphasis -->
<!ATTLIST em %attrs;>

> em' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> em' = tellNode inline "em" []
>
> em :: Inline c => XHtml InlineContent -> XHtml c
> em = em' []

<!ELEMENT strong %Inline;>   <!-- strong emphasis -->
<!ATTLIST strong %attrs;>

> strong' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> strong' = tellNode inline "strong" []
>
> strong :: Inline c => XHtml InlineContent -> XHtml c
> strong = strong' []

<!ELEMENT dfn %Inline;>   <!-- definitional -->
<!ATTLIST dfn %attrs;>

> dfn' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> dfn' = tellNode inline "dfn" []
>
> dfn :: Inline c => XHtml InlineContent -> XHtml c
> dfn = dfn' []

<!ELEMENT code %Inline;>   <!-- program code -->
<!ATTLIST code %attrs;>

> code' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> code' = tellNode inline "code" []
>
> code :: Inline c => XHtml InlineContent -> XHtml c
> code = code' []

<!ELEMENT samp %Inline;>   <!-- sample -->
<!ATTLIST samp %attrs;>

> samp' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> samp' = tellNode inline "samp" []
>
> samp :: Inline c => XHtml InlineContent -> XHtml c
> samp = samp' []

<!ELEMENT kbd %Inline;>  <!-- something user would type -->
<!ATTLIST kbd %attrs;>

> kbd' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> kbd' = tellNode inline "kbd" []
>
> kbd :: Inline c => XHtml InlineContent -> XHtml c
> kbd = kbd' []

<!ELEMENT var %Inline;>   <!-- variable -->
<!ATTLIST var %attrs;>

> var' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> var' = tellNode inline "var" []
>
> var :: Inline c => XHtml InlineContent -> XHtml c
> var = var' []

<!ELEMENT cite %Inline;>   <!-- citation -->
<!ATTLIST cite %attrs;>

> cite' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> cite' = tellNode inline "cite" []
>
> cite :: Inline c => XHtml InlineContent -> XHtml c
> cite = cite' []

<!ELEMENT abbr %Inline;>   <!-- abbreviation -->
<!ATTLIST abbr %attrs;>

> abbr' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> abbr' = tellNode inline "abbr" []
>
> abbr :: Inline c => XHtml InlineContent -> XHtml c
> abbr = abbr' []

<!ELEMENT acronym %Inline;>   <!-- acronym -->
<!ATTLIST acronym %attrs;>

> acronym' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> acronym' = tellNode inline "acronym" []
>
> acronym :: Inline c => XHtml InlineContent -> XHtml c
> acronym = acronym' []

<!ELEMENT q %Inline;>   <!-- inlined quote -->
<!ATTLIST q
  %attrs;
  cite        %URI;          #IMPLIED
  >

> q' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> q' = tellNode inline "q" []
>
> q :: Inline c => XHtml InlineContent -> XHtml c
> q = q' []

<!ELEMENT sub %Inline;> <!-- subscript -->
<!ATTLIST sub %attrs;>

> sub' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> sub' = tellNode inline "sub" []
>
> sub :: Inline c => XHtml InlineContent -> XHtml c
> sub = sub' []

<!ELEMENT sup %Inline;> <!-- superscript -->
<!ATTLIST sup %attrs;>

> sup' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> sup' = tellNode inline "sup" []
>
> sup :: Inline c => XHtml InlineContent -> XHtml c
> sup = sup' []

<!ELEMENT tt %Inline;>   <!-- fixed pitch font -->
<!ATTLIST tt %attrs;>

> tt' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> tt' = tellNode inline "tt" []
>
> tt :: Inline c => XHtml InlineContent -> XHtml c
> tt = tt' []

<!ELEMENT i %Inline;>   <!-- italic font -->
<!ATTLIST i %attrs;>

> i' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> i' = tellNode inline "i" []
>
> i :: Inline c => XHtml InlineContent -> XHtml c
> i = i' []

<!ELEMENT b %Inline;>   <!-- bold font -->
<!ATTLIST b %attrs;>

> b' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> b' = tellNode inline "b" []
>
> b :: Inline c => XHtml InlineContent -> XHtml c
> b = b' []

<!ELEMENT big %Inline;>   <!-- bigger font -->
<!ATTLIST big %attrs;>

> big' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> big' = tellNode inline "big" []
>
> big :: Inline c => XHtml InlineContent -> XHtml c
> big = big' []

<!ELEMENT small %Inline;>   <!-- smaller font -->
<!ATTLIST small %attrs;>

> small' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> small' = tellNode inline "small" []
>
> small :: Inline c => XHtml InlineContent -> XHtml c
> small = small' []

<!--==================== Object ======================================-->

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

> object' :: Flow c => Attrs -> XHtml ObjectContent -> XHtml c
> object' = tellNode flow "object" []
>
> object :: Flow c => XHtml ObjectContent -> XHtml c
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

> param' :: Attrs -> XHtml ObjectContent
> param' = tellEmptyNode Object "param" []
>
> param :: XHtml ObjectContent
> param = param' []

<!--=================== Images ===========================================-->

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

> img' :: Flow c 
>      => Text -- ^ Required src attribute. 
>      -> Text -- ^ Required alt attribute.
>      -> Attrs -> XHtml c
> img' src alt = tellEmptyNode flow "img" []
>
> img :: Flow c => Text -> Text -> XHtml c
> img src alt = img' src alt [Attr "src" src, Attr "alt" alt]

<!-- usemap points to a map element which may be in this document
  or an external document, although the latter is not widely supported -->

<!--================== Client-side image maps ============================-->

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

> map' :: Flow c 
>      => Text -- ^ Required id attribute. 
>      -> Attrs -> XHtml MapContent -> XHtml c
> map' id = tellNode flow "map" [Attr "id" id]
>
> map_ :: Flow c => Text -> XHtml MapContent -> XHtml c
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

> area' :: Text -- ^ Required alt attribute.
>       -> Attrs -> XHtml MapContent
> area' alt = tellEmptyNode Map "area" [Attr "alt" alt]
>
> area :: Text -> XHtml MapContent
> area = flip area' []

<!--================ Forms ===============================================-->

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

> form' :: Block c 
>       => Text -- ^ Required action attribute. 
>       -> Attrs -> XHtml FlowContent -> XHtml c
> form' action = tellNode block "form" [Attr "action" action]
>
> form :: Block c => Text -> XHtml FlowContent -> XHtml c
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

> label' :: Inline c => Attrs -> XHtml InlineContent -> XHtml c
> label' = tellNode inline "label" []
>
> label :: Inline c => XHtml InlineContent -> XHtml c
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

> input' :: Inline c => Attrs -> XHtml c
> input' = tellEmptyNode inline "input" []
>
> input :: Inline c => XHtml c
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
> select' :: Inline c => Attrs -> XHtml OptionContent -> XHtml c 
> select' = tellNode inline "select" []
>
> select :: Inline c => XHtml OptionContent -> XHtml c
> select = select' []

<!ELEMENT optgroup (option)+>   <!-- option group -->
<!ATTLIST optgroup
  %attrs;
  disabled    (disabled)     #IMPLIED
  label       %Text;         #REQUIRED
  >

> optgroup' :: Text -- ^ Required label attribute. 
>           -> Attrs -> XHtml OptionContent -> XHtml OptionContent
> optgroup' label = tellNode Option "optgroup" [Attr "label" label]
>
> optgroup :: Text -> XHtml OptionContent -> XHtml OptionContent
> optgroup = flip optgroup' []

<!ELEMENT option (#PCDATA)>     <!-- selectable choice -->
<!ATTLIST option
  %attrs;
  selected    (selected)     #IMPLIED
  disabled    (disabled)     #IMPLIED
  label       %Text;         #IMPLIED
  value       CDATA          #IMPLIED
  >

> option' :: Attrs -> Text -> XHtml OptionContent
> option' = tellTextNode Option "option" []
>
> option :: Text -> XHtml OptionContent
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

> textarea' :: Inline c
>           => Int -- ^ Required rows attribute.
>           -> Int -- ^ Required cols attribute.
>           -> Attrs -> Text -> XHtml c
> textarea' rows cols = tellTextNode inline "textarea" 
>                           [ Attr "rows" (T.pack (show rows))
>                           , Attr "cols" (T.pack (show cols))
>                           ]
> 
> textarea :: Inline c => Int -> Int -> Text -> XHtml c
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

> fieldset' :: Block c => Attrs -> XHtml FieldSetContent -> XHtml c
> fieldset' = tellNode block "fieldset" []
>
> fieldset :: Block c => XHtml FieldSetContent -> XHtml c
> fieldset = fieldset' []

<!ELEMENT legend %Inline;>     <!-- fieldset label -->
<!ATTLIST legend
  %attrs;
  accesskey   %Character;    #IMPLIED
  >

> legend' :: Attrs -> XHtml InlineContent -> XHtml FieldSetContent 
> legend' = tellNode FieldSet "legend" []
>
> legend :: XHtml InlineContent -> XHtml FieldSetContent
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

> button' :: Inline c => Attrs -> XHtml FlowContent -> XHtml c
> button' = tellNode inline "button" []
>
> button :: Inline c => XHtml FlowContent -> XHtml c
> button = button' []

<!--======================= Tables =======================================-->

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

> table' :: Block c => Attrs -> XHtml Table1Content -> XHtml c
> table' = tellNode block "table" []
> 
> table :: Block c => XHtml Table1Content -> XHtml c
> table = table' []

<!ELEMENT caption  %Inline;>

> caption' :: Attrs -> XHtml InlineContent -> XHtml Table1Content
> caption' = tellNode Table1 "caption" []
>
> caption :: XHtml InlineContent -> XHtml Table1Content
> caption = caption' []

<!ELEMENT thead    (tr)+>

> thead' :: Attrs -> XHtml Table2Content -> XHtml Table1Content
> thead' = tellNode Table1 "thead" []
>
> thead :: XHtml Table2Content -> XHtml Table1Content
> thead = thead' []

<!ELEMENT tfoot    (tr)+>

> tfoot' :: Attrs -> XHtml Table2Content -> XHtml Table1Content
> tfoot' = tellNode Table1 "tfoot" []
>
> tfoot :: XHtml Table2Content -> XHtml Table1Content
> tfoot = tfoot' []

<!ELEMENT tbody    (tr)+>

> tbody' :: Attrs -> XHtml Table2Content -> XHtml Table1Content
> tbody' = tellNode Table1 "tbody" []
>
> tbody :: XHtml Table2Content -> XHtml Table1Content
> tbody = tbody' []

<!ELEMENT colgroup (col)*>

> colgroup' :: Attrs -> XHtml TableColContent -> XHtml Table1Content
> colgroup' = tellNode Table1 "colgroup" []
>
> colgroup :: XHtml TableColContent -> XHtml Table1Content
> colgroup = colgroup' []

<!ELEMENT col      EMPTY>

> col' :: Attrs -> XHtml TableColContent
> col' = tellEmptyNode TableCol "col" []
>
> col :: XHtml TableColContent
> col = col' []

<!ELEMENT tr       (th|td)+>

> tr' :: Attrs -> XHtml Table3Content -> XHtml Table2Content
> tr' = tellNode Table2 "tr" []
> 
> tr :: XHtml Table3Content -> XHtml Table2Content
> tr = tr' []

<!ELEMENT th       %Flow;>

> th' :: Attrs -> XHtml FlowContent -> XHtml Table3Content
> th' = tellNode Table3 "th" []
>
> th :: XHtml FlowContent -> XHtml Table3Content
> th = th' []

<!ELEMENT td       %Flow;>

> td' :: Attrs -> XHtml FlowContent -> XHtml Table3Content
> td' = tellNode Table3 "td" []
>
> td :: XHtml FlowContent -> XHtml Table3Content
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
