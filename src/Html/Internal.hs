module Html.Internal where

import Data.List (intercalate)
import Numeric.Natural

-- * Types

newtype Html = Html String

newtype Structure = Structure String

type Title = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title (Structure content) = Html (el "html" (el "head" (el "title" (escape title)) <> el "body" content))

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h_ :: Natural -> String -> Structure
h_ n = Structure . el ("h" <> show n) . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . unwords . map (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . unwords . map (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

createStructure :: String -> Structure
createStructure = Structure

instance Semigroup Structure where
  (<>) c1 c2 =
    Structure (getStructureString c1 <> getStructureString c2)

-- * Render

render :: Html -> String
render (Html s) = s

-- * Utilities

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concatMap escapeChar