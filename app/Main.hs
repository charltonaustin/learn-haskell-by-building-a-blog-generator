import Html
import Html.Internal (createStructure)
import Markup (parse, renderMarkup)

main :: IO ()
main = do
  putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "Hello title"
    (createStructure (renderMarkup (parse file)))

file :: String
file =
  unlines
    [ "* Title"
    , "first paragraph"
    ,  ""
    ,  "second paragraph"
    ,  "- ul one"
    ,  "- ul two"
    ,  "# ol 1"
    ,  "# ol 2"
    ,  "> code block"
    ]