-- HsBlog.hs
module HsBlog
  ( convertSingle
  , convertDirectory
  , process
  )
  where

import qualified  Markup
import qualified  Html
import Convert (convert)

import System.IO

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = error "Not implemented"

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse
