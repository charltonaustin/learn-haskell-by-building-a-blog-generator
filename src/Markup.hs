module Markup (
    Document
    , Structure(..)
    , parse
    , renderMarkup
) where

import Numeric.Natural
import Data.Maybe

type Document = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving Show

renderMarkup :: Document -> String
renderMarkup s =
    case s of
        [] -> ""
        d:rest ->
            case d of
                Heading n s -> s <> renderMarkup rest
                Paragraph s -> s <> renderMarkup rest
                UnorderedList xs -> concat xs <>  renderMarkup rest
                OrderedList xs -> concat xs <> renderMarkup rest
                CodeBlock xs -> concat xs <> renderMarkup rest

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    [] -> maybeToList context
    -- Paragraph case
    currentLine : rest ->
      let
        line = trim currentLine
      in
        if line == ""
          then
            maybe id (:) context (parseLines Nothing rest)
          else
            case context of
              Just (Paragraph paragraph) ->
                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
              _ ->
                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words