module Markup (
    Document
    , Markup.Structure(..)
    , parse
    , renderMarkup
) where

import Numeric.Natural
import Data.Maybe
import Html (h1_, p_, ul_, ol_, code_, render, Structure)
import Html.Internal (getStructureString, createStructure)

type Document = [Markup.Structure]

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
                Heading _ s -> getStructureString (h1_ s) <> renderMarkup rest
                Paragraph s -> getStructureString (p_ s) <> renderMarkup rest
                UnorderedList xs -> getStructureString (ul_ (map createStructure xs)) <>  renderMarkup rest
                OrderedList xs -> getStructureString (ol_ (map createStructure xs)) <> renderMarkup rest
                CodeBlock xs -> (concatMap getStructureString (map code_ xs)) <> renderMarkup rest

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Markup.Structure -> [String] -> Document
parseLines context txts =
  case txts of
    -- done case
    [] -> maybeToList context

    -- Heading 1 case
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)

    -- Unordered list case
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          parseLines (Just (UnorderedList (list <> [trim line]))) rest

        _ ->
          maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)

    -- Ordered list case
    ('#' : ' ' : line) : rest ->
      case context of
        Just (OrderedList list) ->
          parseLines (Just (OrderedList (list <> [trim line]))) rest

        _ ->
          maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)

    -- Code block case
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock code) ->
          parseLines (Just (CodeBlock (code <> [line]))) rest

        _ ->
          maybe id (:) context (parseLines (Just (CodeBlock [line])) rest)

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