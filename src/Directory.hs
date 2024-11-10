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
import Options.Applicative (value)

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
convertDirectory = error "Not Implemented"

-- convertDirectory inputDir outputDir = do
--  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
--  createOutputDirectoryOrExit outputDir
-- let
--    outputHtmls = txtsToRenderedHtml filesToProcess
--  copyFiles outputDir filesToCopy
--  writeFiles outputDir outputHtmls
--  putStrLn "Done."

data DirContents
  = DirContents
    { dcFilesToProcess :: [(FilePath, String)]
      -- ^ File paths and their content
    , dcFilesToCopy :: [FilePath]
      -- ^ Other file paths, to be copied directly
    }

-- | Returns the directory content
getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent = error "Not Implemented"
-- getDirFilesAndContent inputDir = do
--   files <- map (inputDir </>) <$> listDirectory inputDir
--   let
--     (txtFiles, otherFiles) =
--       partition ((== ".txt") . takeExtension) files
--   txtFilesAndContent <-
--     applyIoOnList readFile txtFiles >>= filterAndReportFailures
--   pure $ DirContents
--     { dcFilesToProcess = txtFilesAndContent
--     , dcFilesToCopy = otherFiles
--     }


applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList action inputs = do
  for inputs $ \input -> do
    maybeResult <-
      catch
        (Right <$> action input)
        ( \(SomeException e) -> do
          pure $ Left (displayException e)
        )
    pure (input, maybeResult)


filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap $ \(file, contentOrErr) ->
    case contentOrErr of
      Left err -> do
        hPutStrLn stderr err
        pure []
      Right content ->
        pure [(file, content)]
