module Markup
  ( Document,
    Markup.Structure (..),
    parse,
    renderMarkup,
  )
where

import Data.Maybe
import Html (Structure, code_, h_, ol_, p_, render, ul_)
import Html.Internal (createStructure, getStructureString)
import Numeric.Natural

type Document = [Markup.Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Show)

renderMarkup :: Document -> String
renderMarkup s =
  case s of
    [] -> ""
    d : rest ->
      case d of
        Heading n s -> getStructureString (h_ n s) <> renderMarkup rest
        Paragraph s -> getStructureString (p_ s) <> renderMarkup rest
        UnorderedList xs -> getStructureString (ul_ (map createStructure xs)) <> renderMarkup rest
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
    ('*' : line) : rest ->
      maybe
        id
        (:)
        context
        ( Heading (toEnum (length (['*'] <> takeWhile (== '*') line))) (trim (dropWhile (== '*') line))
            : parseLines Nothing rest
        )
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
      let line = trim currentLine
       in if line == ""
            then
              maybe id (:) context (parseLines Nothing rest)
            else case context of
              Just (Paragraph paragraph) ->
                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
              _ ->
                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words