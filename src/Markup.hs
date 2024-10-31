module Markup (
    Document
    , Structure(..)
    , parse
    , renderMarkup
) where

import Numeric.Natural

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
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
  let
    paragraph = Paragraph (unlines (reverse currentParagraph))
  in
    case txts of
      [] -> [paragraph]
      currentLine : rest ->
        if trim currentLine == ""
          then
            paragraph : parseLines [] rest
          else
            parseLines (currentLine : currentParagraph) rest

trim :: String -> String
trim = unwords . words