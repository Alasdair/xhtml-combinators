# xhtml-combinators

xhtml-combinators is a (hopefully) fast and easy to use set of xhtml combinators with the following goals in mind:

* Ensure that generated xhtml is (mostly) valid, leveraging the power of haskell's type system to check validity at compile time. Obviously there are limits to what we can make the type-checker do without using too much type-level wizardry, or hurting usability.

* Run fast. The use of Data.Text already gives significant  improvements over a naive String based approach, and I haven't even begun to profile or optimise anything yet. Further improvements should easily be possible.

* Easy to use.

The following basic example shows how it all fits together. For more see doc/examples.lhs

    example1 = html True $ do
        head_ $ title "Example 1"
        body $ p (text "Hello, World!")

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
