import Html
import Markup (parse, renderMarkup)

main :: IO ()
main = do
    putStrLn (render myhtml)
    putStrLn (renderMarkup (parse file))

myhtml :: Html
myhtml =
  html_
    "Hello title"
    (h1_ "Hello, world!" <> p_ "Let's learn about Haskell!")

file :: String
file =
  unlines
    [ "first paragraph",
      "",
      "second paragraph"
    ]