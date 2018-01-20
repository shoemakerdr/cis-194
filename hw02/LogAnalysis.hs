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
getTimestamp (Failure textList) = Failure textList
getTimestamp (Success (msgType, textList)) =
  case readMaybeInt $ head textList of
    Nothing ->
      Failure textList

    Just timestamp ->
      Success (msgType, timestamp, drop 1 textList)


getInfoMsg :: Failable [String] (MessageType, TimeStamp, [String]) -> Failable [String] (MessageType, TimeStamp, String)
getInfoMsg (Failure textList) = Failure textList
getInfoMsg (Success (msgType, timestamp, textList)) =
  Success (msgType, timestamp, unwords textList)


getLogMsg :: Failable [String] (MessageType, TimeStamp, String) -> LogMessage
getLogMsg (Failure textList) =
  Unknown $ unwords textList
getLogMsg (Success (msg, ts, info)) =
  LogMessage msg ts info


readMaybeInt :: String -> Maybe Int
readMaybeInt int = readMaybe int



-- EXERCISE 2 --


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMsg@(LogMessage _ _ _) tree =
  case tree of
    Leaf ->
      Node Leaf logMsg Leaf

    Node left msg right ->
      if logMsg `lessThan` msg then
        case left of
          Leaf ->
            Node (Node Leaf logMsg Leaf) msg right

          Node _ _ _ ->
            Node (insert logMsg left) msg right
      else
        case right of
          Leaf ->
            Node left msg (Node Leaf logMsg Leaf)

          Node _ _ _ ->
            Node left msg (insert logMsg right)


lessThan :: LogMessage -> LogMessage -> Bool
lessThan (LogMessage _ timestamp1 _) (LogMessage _ timestamp2 _) =
  timestamp1 < timestamp2
lessThan _ _ = False



-- EXERCISE 3 --


build :: [LogMessage] -> MessageTree
build logMsgs =
  foldl (flip insert) Leaf logMsgs



-- EXERCISE 4 --


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMsg right) =
  case (left, right) of
    (Leaf, Leaf) ->
      [logMsg]

    (Node _ _ _, Node _ _ _ ) ->
      inOrder left ++ [logMsg] ++ inOrder right

    (Node _ _ _, Leaf) ->
      inOrder left ++ [logMsg]

    (Leaf, Node _ _ _) ->
      [logMsg] ++ inOrder right



-- EXERCISE 5 --


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMsgs =
  map extractInfo $ filter errorGreaterThan50 logMsgs


errorGreaterThan50 :: LogMessage -> Bool
errorGreaterThan50 (LogMessage (Error level) _ _) =
  level > 50
errorGreaterThan50 _ = False


extractInfo :: LogMessage -> String
extractInfo (LogMessage _ _ info) = info
extractInfo (Unknown info) = info

