module Html
   ( Html
   , Title
   , Structure
   , html_
   , p_
   , h1_
   , append_
   , render
   ) where

newtype Html = Html String
newtype Structure = Structure String
type Title = String


append_ :: Structure -> Structure -> Structure
append_ (Structure x) (Structure y) = Structure (x <> y)

render :: Html -> String
render (Html s) = s

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

p_ :: String -> Structure
p_  = Structure . el "p"

h1_ :: String -> Structure
h1_  = Structure . el "h1"


html_ :: Title -> Structure -> Html
html_ title (Structure content) = Html (el "html" (el "head" (el "title" title) <> el "body" content))