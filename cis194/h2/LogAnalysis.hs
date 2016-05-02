 --{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
    import Log

    parseMessage :: String -> LogMessage
    parseMessage = parseParts . words
        where
            parseParts :: [String] -> LogMessage
            parseParts ("I":timestamp:msg) = create Info timestamp msg
            parseParts ("W":timestamp:msg) = create Warning timestamp msg
            parseParts ("E":errorLevel:timestamp:msg) = create (Error (read errorLevel)) timestamp msg
            parseParts msg = Unknown (unwords msg)

            create :: MessageType -> String -> [String] -> LogMessage
            create messageType timestamp msg = LogMessage messageType (read timestamp) (unwords msg)

    parse :: String -> [LogMessage]
    parse = map parseMessage . lines

    insert :: LogMessage -> MessageTree -> MessageTree
    insert (Unknown _) tree = tree
    insert logMessage Leaf = Node Leaf logMessage Leaf
    insert newLogMessage@(LogMessage _ newTimestamp _) (Node lBranch logMessage@(LogMessage _ timestamp _) rBranch)
        | newTimestamp <= timestamp = Node (insert newLogMessage lBranch) logMessage rBranch
        | otherwise = Node lBranch logMessage (insert newLogMessage rBranch)

    --buildx :: [LogMessage] -> MessageTree -> MessageTree
    --buildx [] tree = tree
    --buildx (m:ms) tree = buildx ms (insert m tree)

    build :: [LogMessage] -> MessageTree
    build = foldl (flip insert) Leaf

    inOrder :: MessageTree -> [LogMessage]
    inOrder Leaf = []
    inOrder (Node lBranch msg rBranch) = (inOrder (lBranch)) ++ [msg] ++ (inOrder rBranch)

    extractMessage :: LogMessage -> String
    extractMessage (Unknown msg) = msg
    extractMessage (LogMessage _ _ msg) = msg

    whatWentWrong :: [LogMessage] -> [String]
    whatWentWrong = map extractMessage . inOrder . build . (filter (\x -> isSevereError x))
        where 
            isSevereError :: LogMessage -> Bool
            isSevereError (LogMessage (Error errorLevel) _ _) = errorLevel >= 50
            isSevereError _ = False

    testLogs = [
        (LogMessage (Error 1) 500 "Wohoo"),
        (LogMessage Info 100 "A"),
        (LogMessage (Error 100) 999 "Z"),
        (LogMessage (Error 50) 500 "X")
        ]
        