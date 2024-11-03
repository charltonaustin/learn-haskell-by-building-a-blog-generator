-- Main.hs
module Main where

import Convert (convert)
import qualified Html
import qualified Markup
import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= \args ->
    case args of
      -- No program arguments: reading from stdin and writing to stdout
      [] ->
        getContents >>= \content ->
          putStrLn (process "Empty title" content)
      -- With input and output file paths as program arguments
      [input, output] ->
        readFile input >>= \content ->
          doesFileExist output >>= \exists ->
            let writeResult = writeFile output (process input content)
             in if exists
                  then whenIO confirm writeResult
                  else writeResult
      -- Any other kind of program arguments
      _ ->
        putStrLn "Usage: learn-haskell-by-building-a-blog-generator-exe <input-file> <output-file>"

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/n)"
    *> getLine
    >>= \answer ->
      case answer of
        "y" -> pure True
        "n" -> pure False
        _ ->
          putStrLn "Invalid response. use y or n"
            *> confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action =
  cond >>= \result ->
    if result
      then action
      else pure ()
