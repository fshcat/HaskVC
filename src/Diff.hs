module Diff (rawDiff, assembleDiff, parseDiff) where
import System.Process
import Text.Parsec

-- Stores output of diff command in string
rawDiff :: FilePath -> FilePath -> IO String
rawDiff old new = do 
    (_,out,_) <- readProcessWithExitCode "diff" [old,new] ""
    return out

-- Reassembles diff string from parsed data structure
assembleDiff :: Diff -> IO String
assembleDiff diff = return $ concatStrings mapped
    where
    mapped = fmap concatBlock diff

    -- Concatenates all elements in a list of strings
    -- (could probably rewrite using foldl)
    concatStrings (x:[]) = x
    concatStrings (x:xs) = x ++ (concatStrings xs)

    -- Checks what kind of change each block consists of, diverts
    -- to according function
    concatBlock ((ln1,ind,ln2),changes)
        | ind == 'd' = ln1 ++ [ind] ++ ln2 ++ "\n" ++ (handleDel changes) ++ "\n"
        | ind == 'c' = ln1 ++ [ind] ++ ln2 ++ "\n" ++ (handleChanges changes "< ") ++ "\n"
        | ind == 'a' = ln1 ++ [ind] ++ ln2 ++ "\n" ++ (handleAdd changes) ++ "\n"
        | otherwise  = ""
    
    handleDel (x:[]) = "< " ++ x
    handleDel (x:xs) = "< " ++ x ++ "\n" ++ (handleDel xs)  

    handleAdd (x:[]) = "> " ++ x
    handleAdd (x:xs) = "> " ++ x ++ "\n" ++ (handleAdd xs)

    -- Changes prefix used once divider is reached
    handleChanges (x:[]) prefix = prefix ++ x
    handleChanges (x:xs) prefix = if x=="---" 
                                  then (handleChanges xs "> ") 
                                  else (prefix ++ x ++ "\n" ++ (handleChanges xs prefix))

-- Parse diff string into data structure.
-- Unsure if this should be IO String -> IO (Maybe Diff) instead
parseDiff :: String -> IO (Maybe Diff)
parseDiff rawDiff = do 
    -- rawDiff <- rawDiffIO 
    let eitherDiff = runParser pDiff () "" rawDiff
        result = processEither eitherDiff
    return result
    where
        -- outputs Nothing if runParser returns an error code
        processEither :: Either a b -> Maybe b
        processEither (Left e) = Nothing
        processEither (Right r) = Just r

type Identifier = (String,Char,String)
type Changes = [String]
type ChangeBlock = (Identifier,Changes)
type Diff = [ChangeBlock]

-- Parses the indicators at the top of change blocks that describe the lines
-- affected and the kind of change made, e.g. 6,7c8
parseIdentifier :: Parsec String () Identifier
parseIdentifier = do
    firstLine <- many1 ((try digit) <|> (try $ char ','))
    indicator <- letter
    secondLine <- many1 ((try digit) <|> (try $ char ','))
    newline
    return (firstLine,indicator,secondLine)

-- Parses one line of a change block, which consists of a symbol followed by
-- a space and the change made, e.g. > Add word
parseChange :: Parsec String () String
parseChange = do
    oneOf ['<','>','=']
    space
    change <- many . noneOf $ ['\n']
    newline
    return change
-- Parses the divider that appears in 'c' blocks between the previous and changed line
parseDivider :: Parsec String () String
parseDivider = do
    divider <- many1 . oneOf $ ['-']
    newline
    return divider

-- Parses all the changes in a change block
parseChanges :: Parsec String () Changes
parseChanges = many1 (parseChange <|> parseDivider)

-- Parses the changes and the identifier in a change block
parseChangeBlock :: Parsec String () ChangeBlock
parseChangeBlock = do
    id <- parseIdentifier
    chs <- parseChanges
    return (id,chs)

-- Full diff output parser
pDiff :: Parsec String () Diff
pDiff = many parseChangeBlock
