module Html.Internal where

import Data.Maybe (Maybe (Just), fromMaybe)
import Numeric.Natural

-- * Types

newtype Html = Html String

newtype Content = Content String

newtype Structure = Structure String

newtype Head = Head String

type Title = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title (Structure content) = Html (el "html" (el "head" (el "title" (escape title)) <> el "body" content))

title_ :: Title -> Head
title_ = Head . el "title" . escape

p_ :: Content -> Structure
p_ = Structure . el "p" . getContentString

h_ :: Natural -> Content -> Structure
h_ n = Structure . el ("h" <> show n) . getContentString

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . unwords . map (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . unwords . map (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

txt_ :: String -> Content
txt_ = Content . escape

link_ :: FilePath -> Content -> Content
link_ path (Content content) = Content (elAtt "a" (Just (att_ "href" (escape path))) (escape content))

img_ :: FilePath -> Content
img_ path = Content ("<img src=\"" <> escape path <> "\">")

b_ :: Content -> Content
b_ (Content content) = Content (el "b" (escape content))

i_ :: Content -> Content
i_ (Content content) = Content (el "i" (escape content))

empty_ :: Structure
empty_ = Structure ""

concatStructure :: [Structure] -> Structure
concatStructure list =
  case list of
    [] -> empty_
    x : xs -> x <> concatStructure xs

instance Semigroup Structure where
  (<>) c1 c2 =
    Structure (getStructureString c1 <> getStructureString c2)

instance Monoid Structure where
  mempty = Structure ""

instance Semigroup Content where
  (<>) c1 c2 =
    Content (getContentString c1 <> getContentString c2)

instance Monoid Content where
  mempty = Content ""

instance Semigroup Head where
  (<>) c1 c2 =
    Head (getHeadString c1 <> getHeadString c2)

instance Monoid Head where
  mempty = Head ""

-- * Render

render :: Html -> String
render (Html s) = s

-- * Utilities

el :: String -> String -> String
el tag = elAtt tag Nothing

att_ :: String -> String -> String
att_ name value = escape name <> "=" <> "\"" <> escape value <> "\""

elAtt :: String -> Maybe String -> String -> String
elAtt tag maybeAttribute content =
  "<" <> tag <> fromMaybe "" maybeAttribute <> ">" <> content <> "</" <> tag <> ">"

getContentString :: Content -> String
getContentString content =
  case content of
    Content str -> str

getHeadString :: Head -> String
getHeadString s =
  case s of
    Head str -> str

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