module DirectoryTree (directoryToTree, addTreeEntry, deleteTreeEntry) where
import System.Directory
import System.Process
import Data.Tree
import Data.Time
import Data.Time.Clock
import Data.List.Split
import Data.List
import Data.Monoid

-- Debugging functions
drawDirectoryTree :: FilePath -> IO ()
drawDirectoryTree path = do
    tree <- directoryToTree path
    putStrLn . drawTree . dirTreeToTree $ tree

dirTreeToTree :: DirTree -> Tree String
dirTreeToTree (File s i) = Node ((show i)++ " " ++ s) []
dirTreeToTree (Directory s []) = Node s []
dirTreeToTree (Directory s tl) = Node s (map dirTreeToTree tl)

-- Directory tree data structure
data DirTree
   = Directory
   { name :: String
   , subdirs :: [DirTree] 
   }
   | File
   { name :: String
   , fid :: Int
   } deriving (Show)

-- We only ever want to compare files 
-- and directories by name
instance Eq DirTree where
    (Directory a _) == (Directory b _) = a == b
    (File a _) == (File b _) = a == b
    _ == _ = False

type DiffInfo = ([String], [String], [String])

data Commit = Commit { timestamp :: UTCTime, tree :: DirTree } deriving (Show)

-- Collects DiffInfo ([Additions], [Modifications], [Deletions])
-- between the current project directory and the previous commit
collectDiffs :: Commit -> DirTree -> IO DiffInfo
collectDiffs (Commit lastCommit commitTree) currentTree = runComparison commitTree currentTree
    where
        isFile :: DirTree -> Bool
        isFile (File _ _) = True
        isFile _ = False

        runComparison :: DirTree -> DirTree -> IO DiffInfo
        
        runComparison (File name1 _) (File name2 _) = do
            fileModDate <- getModificationTime name2
            if name1 == name2 && lastCommit >= fileModDate then
                 return ([], [], [])
            else if name1 == name2 && lastCommit < fileModDate then
                 return ([], [name2], [])
            else return ([name2], [], [name1])

        runComparison (Directory _ contents1) (Directory _ contents2) = do
            let oldFiles        = map name $ filter isFile contents1
            let currentFiles    = map name $ filter isFile contents2

            let fileDeletions   = [([], [], oldFiles \\ currentFiles)]
            let fileCreations   = [(currentFiles \\ oldFiles, [], [])]
            
            let contentDeletions = contents1 \\ contents2
            let contentCreations = contents2 \\ contents1
            let uncommonContent = contentDeletions ++ contentCreations

            let commonContents1 = contents1 \\ uncommonContent
            let commonContents2 = contents2 \\ uncommonContent

            innerComparisons <- sequence $ map (uncurry runComparison) (zip commonContents1 commonContents2)
            return . mconcat $ (fileCreations ++ innerComparisons ++ fileDeletions)
        
        runComparison _ _ = return ([], [], [])

-- Turns directory into Tree
directoryToTree :: FilePath -> IO DirTree
directoryToTree path = entryToTree path
    where
    -- Process whether given path is a file or directory
    entryToTree :: FilePath -> IO DirTree
    entryToTree path = do
        dir <- doesDirectoryExist path
        if dir then subDirectoryToTree path else fileToTree path

    subDirectoryToTree :: FilePath -> IO DirTree
    subDirectoryToTree path = do
        -- Grabs the contents of the directory as a list, then maps
        -- directoryToTree to it to turn the entries into trees
        entryNames <- listSubEntries path
        let entryPaths = map ((path++"/")++) entryNames

        let subTreesMonadic = map entryToTree entryPaths
        subTrees <- sequence subTreesMonadic

        return $ Directory path subTrees

    fileToTree :: FilePath -> IO DirTree
    fileToTree name = return $ File name 0

    -- Equivalent to doesDirectoryExist but returns [] for files
    listSubEntries :: FilePath -> IO [String]
    listSubEntries path = do
        isDirectory <- doesDirectoryExist path
        if isDirectory then listDirectory path else return []

    
-- Turns path string into list of entries
-- i.e. pathToList "/home/user" == ["home","user]
pathToList :: FilePath -> [String]
pathToList path = splitBySlash path "" []
    where
    splitBySlash :: FilePath -> String -> [FilePath] -> [FilePath]
    splitBySlash "" word list = list ++ [word]
    splitBySlash (char:rem) word list 
        | char == '/' && list == [] = splitBySlash rem "" list
        | char == '/' = splitBySlash rem "" (list ++ [word])
        | otherwise = splitBySlash rem (word ++ [char]) list

-- Adds entry to tree. First filepath is the absolute
-- path of the directory described by the given tree.
-- This is used to determine if added entry is a file
-- or a directory
addTreeEntry :: Bool -> FilePath -> DirTree -> DirTree
addTreeEntry ftype p t = addEntry (pathToList p) t
    where
    addEntry :: [FilePath] -> DirTree -> DirTree
    addEntry (entry:rem) (Directory name contents) =
        let processExtraction :: Maybe ([DirTree], DirTree) -> DirTree
            processExtraction Nothing = Directory name $ (listToTree (entry:rem)):contents
            processExtraction (Just (filteredTrees, extraction)) = 
                Directory name $ (addEntry rem extraction):filteredTrees 
        in processExtraction $ pullDir entry contents

    -- Turns a path list into a DirTree
    listToTree :: [FilePath] -> DirTree
    listToTree (entry:[])  = if ftype then Directory entry [] else File entry 0
    listToTree (entry:rem) = 
        Directory (entry) [listToTree rem]

    -- Extracts a directory from a list of DirTree if it has
    -- the target name. Equals nothing if no match is found,
    -- ignores files.
    pullDir :: FilePath -> [DirTree] -> Maybe ([DirTree], DirTree)
    pullDir target list = helper target [] list 
        where
        helper _ _ [] = Nothing
        helper tar passed ((File i n):left) = helper tar ((File i n):passed) left
        helper tar passed ((Directory name l):left)
            | name == tar = Just (passed ++ left, (Directory name l))
            | otherwise   = helper tar ((Directory name l):passed) left

-- Deletes entry from tree. Returns nothing if entry isn't part of tree
deleteTreeEntry :: FilePath -> DirTree -> Maybe DirTree
deleteTreeEntry p t = delEntry (pathToList p) t
    where
    delEntry :: [FilePath] -> DirTree -> Maybe DirTree
    delEntry (entry:[])  (Directory name contents) = do
        (filteredTrees, extraction) <- pullEntry entry contents
        return $ Directory name filteredTrees

    delEntry (entry:rem) (Directory name contents) = do
        (filteredTrees, extraction) <- pullEntry entry contents
        delTree <- delEntry rem extraction
        return $ Directory name $ (delTree):filteredTrees

    -- Extracts an entry from a list of DirTree similarly
    -- to pullDir, but is capable of extracitng a file
    pullEntry :: FilePath -> [DirTree] -> Maybe ([DirTree], DirTree)
    pullEntry target list = helper target [] list 
        where
        helper _ _ [] = Nothing
        helper tar passed (entry:left)
            | name entry == tar         = Just (passed ++ left, entry)
            | otherwise                 = helper tar (entry:passed) left
