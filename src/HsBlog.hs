-- HsBlog.hs
module HsBlog
  ( convertSingle,
    convertDirectory,
    process,
  )
where

import Convert (convert, convertStructure)
import qualified Html
import qualified Markup
import System.IO

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = error "Not implemented"

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

previews :: (FilePath, Markup.Document) -> Html.Structure
previews (path, doc) =
  case doc of
    Markup.Heading 1 heading : article ->
      Html.h_ 3 (Html.link_ path (Html.txt_ heading))
        <> foldMap convertStructure (take 3 article)
        <> Html.p_ (Html.link_ path (Html.txt_ "..."))
    _ ->
      Html.h_ 3 (Html.link_ path (Html.txt_ path))

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex xs =
  Html.html_
    "Blog"
    (Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog")) <> Html.h_ 2 (Html.txt_ "Posts") <> foldMap previews xs)