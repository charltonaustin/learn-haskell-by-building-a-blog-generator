module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn  (render_  myhtml)

newtype Html = Html String
newtype Structure = Structure String
type Title = String


append_ :: Structure -> Structure -> Structure
append_ (Structure x) (Structure y) = Structure (x <> y)

render_ :: Html -> String
render_ (Html s) = s

myhtml :: Html
myhtml =
  makeHtml
    "Hello title"
    (append_ (h1_ "Hello, world!") (p_ "Let's learn about Haskell!"))

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

p_ :: String -> Structure
p_  = Structure . el "p"

h1_ :: String -> Structure
h1_  = Structure . el "h1"


makeHtml :: Title -> Structure -> Html
makeHtml title (Structure content) = Html (el "html" (el "head" (el "title" title) <> el "body" content))