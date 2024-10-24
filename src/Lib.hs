module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn (wrapHtml "Hello, world!")

wrapHtml :: String -> String
wrapHtml content = "<html><body>" <> content <> "</body></html>"
