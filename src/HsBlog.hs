-- HsBlog.hs
module HsBlog
  ( convertSingle,
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

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse



