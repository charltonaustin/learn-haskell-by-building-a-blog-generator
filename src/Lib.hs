module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn (makeHtml "My page title" "My page content")

html_ :: String -> String
html_ content = "<html>" <> content <> "</html>"

body_ :: String -> String
body_ content = "<body>" <> content <> "</body>"

title_ :: String -> String
title_ content = "<title>" <> content <> "</title>"

head_ :: String -> String
head_ content = "<head>" <> content <> "</head>"

makeHtml :: String -> String -> String
makeHtml title content = html_ (head_ (title_ title) <> body_ content)