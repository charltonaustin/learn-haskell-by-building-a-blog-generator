module Directory
  ( convertDirectory
  , buildIndex
  )
  where

import qualified  Markup
import qualified  Html
import Convert (convert, convertStructure)

import Data.List (partition)
import Data.Traversable (for)
import Control.Monad (void, when)

import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, displayException, SomeException(..))
import System.Exit (exitFailure)
import System.FilePath
  ( takeExtension
  , takeBaseName
  , (<.>)
  , (</>)
  , takeFileName
  )
import System.Directory
  ( createDirectory
  , removeDirectoryRecursive
  , listDirectory
  , doesDirectoryExist
  , copyFile
  )

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

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = error "Not implemented"