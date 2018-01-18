{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import qualified Data.List as List
import Text.Read (readMaybe)


data Failable e a
  = Failure e
  | Success a
    deriving (Show, Eq)


-- EXERCISE 1 --


parse :: String -> [LogMessage]
parse text =
  List.map parseMessage $ lines text


parseMessage :: String -> LogMessage
parseMessage text =
  let
    textList = words text
    newLog = getLogMsg . getInfoMsg . getTimestamp . getMsgType
  in
    newLog textList


getMsgType :: [String] -> Failable [String] (MessageType, [String])
getMsgType textList =
  case head $ head textList of
    'I' ->
      Success (Info, drop 1 textList)

    'W' ->
      Success (Warning, drop 1 textList)

    'E' ->
      case readMaybeInt $ head $ drop 1 textList of
        Nothing ->
          Failure textList

        Just int ->
          Success (Error int, drop 2 textList)

    _ ->
      Failure textList


getTimestamp :: Failable [String] (MessageType, [String]) -> Failable [String] (MessageType, TimeStamp, [String])
getTimestamp failable =
  case failable of
    Failure textList ->
      Failure textList

    Success (msgType, textList) ->
      case readMaybeInt $ head textList of
        Nothing ->
          Failure textList

        Just timestamp ->
          Success (msgType, timestamp, drop 1 textList)


getInfoMsg :: Failable [String] (MessageType, TimeStamp, [String]) -> Failable [String] (MessageType, TimeStamp, String)
getInfoMsg failable =
  case failable of
    Failure textList ->
      Failure textList

    Success (msgType, timestamp, textList) ->
      Success (msgType, timestamp, unwords textList)


getLogMsg :: Failable [String] (MessageType, TimeStamp, String) -> LogMessage
getLogMsg failable =
  case failable of
    Failure textList ->
      Unknown $ unwords textList

    Success (msg, ts, info) ->
      LogMessage msg ts info


readMaybeInt :: String -> Maybe Int
readMaybeInt int =
  readMaybe int

