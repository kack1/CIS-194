{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage

parseMessage s = case head ws of
                "E" -> LogMessage (Error (read $ head $ tail ws :: Int)) (read $ head $ tail $ tail ws :: Int) (unwords $ drop 2 ws)
                "W" -> LogMessage Warning (read $ head $ tail ws :: Int) (unwords $ drop 2 ws)
                "I" -> LogMessage Info (read $ head $ tail ws :: Int) (unwords $ drop 2 ws)
                _   -> Unknown s
                where ws = words s
parse :: String -> [LogMessage]
parse x = map parseMessage (lines x)
compareLogMessageTime :: LogMessage -> LogMessage -> Int
compareLogMessageTime (LogMessage _ time1 _) (LogMessage _ time2 _)
    | time1 > time2     = 1
    | time1 < time2     = -1
    | time1 == time2    = 0
compareLogMessageTime _  _ = -99

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert lm@(LogMessage {}) (Node l m r)
    | compareLogMessageTime lm m == 1 = Node l m (insert lm r)
    | otherwise = Node (insert lm l) m r
insert n@(LogMessage{}) l@Leaf = Node l n l

build ::  [LogMessage] -> MessageTree
build []        =  Leaf
build l    =  foldr insert Leaf l 


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf =  []
inOrder (Node l m r) = inOrder  l ++  m : inOrder r

isImportant :: LogMessage -> Bool
isImportant (LogMessage (Error t) _ _ ) = t >= 50
isImportant _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong (t@(LogMessage _ _ m):ts) 
    | isImportant t   = m :  whatWentWrong ts
    | otherwise       = whatWentWrong ts
whatWentWrong _ = []
