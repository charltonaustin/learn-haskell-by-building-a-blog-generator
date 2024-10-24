module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn myhtml

myhtml :: String
myhtml =
  makeHtml
    "Hello title"
    (h1_ "Hello, world!" <> p_ "Let's learn about Haskell!")

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: String -> String
html_ = el "html"

body_ :: String -> String
body_ = el "body"

title_ :: String -> String
title_ = el "title"

head_ :: String -> String
head_  = el "head"

p_ :: String -> String
p_  = el "p"

h1_ :: String -> String
h1_  = el "h1"


makeHtml :: String -> String -> String
makeHtml title content = html_ (head_ (title_ title) <> body_ content)