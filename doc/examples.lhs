Necessary extensions and imports:

> {-# LANGUAGE OverloadedStrings #-}
> import Text.XHtmlCombinators
> import Text.XHtmlCombinators.Render
> import qualified Text.XHtmlCombinators.Attributes as A

> import Data.Text (Text)
> import qualified Data.Text as T
> import qualified Data.Text.IO as T

The most simple example:

> example1 = html True $ do
>     head_ $ title "Example 1"
>     body $ p (text "Hello, World!")

Output:

*Main> T.putStrLn $ renderPretty example1
<?xml version="1.0" encoding="UTF-8" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>Example 1</title>
  </head>
  <body>
    <p>Hello, World!</p>
  </body>
</html>

A more complex example:

> example2 = html True $ do
>     head_ $ title "Example 2"
>     body $ do
>         h2 (text "Example 2")
>         p $ do 
>             text "A slightly more complex example"
>             br
>             text "foo"
>             br
>             text "bar"
>             br
>             text "baz"
>         p' [A.style "color: red;"] $ do
>             text "A red paragraph!"
>         p (text "Now for a list of my favourite things beginning with the letter C")
>         ul $ mapM_ (li . text) favs
>
> favs :: [Text]
> favs = ["Cookies", "Cider", "Cthulu"]

*Main> T.putStrLn $ renderPretty example2
<?xml version="1.0" encoding="UTF-8" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>Example 2</title>
  </head>
  <body>
    <h2>Example 2</h2>
    <p>
      A slightly more complex example
      <br />
      foo
      <br />
      bar
      <br />
      baz
    </p>
    <p style="color: red;">A red paragraph!</p>
    <p>Now for a list of my favourite things beginning with the letter C</p>
    <ul>
      <li>Cookies</li>
      <li>Cider</li>
      <li>Cthulu</li>
    </ul>
  </body>
</html>