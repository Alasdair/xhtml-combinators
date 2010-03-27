{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.XHtmlCombinators.Safe
import Text.XHtmlCombinators.Render
import qualified Text.XHtmlCombinators.Attributes.Safe as A
import Text.XHtmlCombinators.Extras.Lorem

tut1 :: XHtml Page
tut1 = html True $ do
    head_ $ title "Example 1"
    body $ p (text "Hello, World")

{-
tut2 :: XHtml Page
tut2 = html True $ do
         head_ $ title "Example 1"
         body $ text "Hello, World"
-}

{-
pre' :: Block c => Attrs -> XHtml InlineContent -> XHtml c
-}

tut2 :: XHtml Page
tut2 = html True $ do
    head_ $ title "Example 2"
    body $ do
        h2 (text "Example 2")
        p $ do 
            text "A slightly more complex example"
            br
            text "foo"
            br
            text "bar"
            br
            text "baz"
        p' [A.style "color: red;"] $ do
            text "A red paragraph!"
        p (text "Now for a list of my favourite things beginning with the letter C")
        ul $ mapM_ (li . text) favs

favs :: [Text]
favs = ["Cookies", "Cider", "Cthulu"]

tut3 :: XHtmlT IO Page
tut3 = html True $ do
    head_ $ title "Example 3"
    body $ do
        loremIO
        loremIO' [A.style "color: blue;"]
        loremIO