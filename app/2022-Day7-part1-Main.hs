{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Main where
import Data.List (nub)
import Control.Exception (throw, Exception)
import qualified Data.Tree as T

main :: IO ()
main = do 
    input <- readFile "./2022-Day7.txt"
    let fs = parseInput $ lines input
    let printable = T.drawTree $ fmap show fs
    putStrLn $ "tree: " ++ printable 
    let result = foldl (findAtMostPredicate 100000) [] $ T.flatten fs 
    let total = sum $ map (\(_, _, size)->size) result
    putStrLn $ show total 

data TraverseException = LastOfPathNotFoundInTree | HeadOfFilesystemDoesntMatch | NextInPathNotFound deriving (Show, Eq)

instance Exception TraverseException

data FileDescriptor = File | Dir deriving (Eq, Show)

type Metadata =  (FileDescriptor, String, Int)

type FileSystem = T.Tree Metadata

type Path = [String]

parseInput :: [String] -> FileSystem
parseInput (h:rest) = 
    let rootNode = (T.Node(Dir, "/", 0) [])
    in
        parseCommand rest rootNode ["/"]

parseCommand :: [String] -> FileSystem -> Path -> FileSystem
parseCommand [] fs path = fs
parseCommand (command:rest) fs path =
    case (command) of
        ('$':' ':'c':'d':' ':dir) -> 
            let newPath = applyCd path dir 
                alreadyAdded = pathAlreadyAdded fs newPath
            in
                case (alreadyAdded) of
                True -> parseCommand rest fs newPath 
                False -> 
                    let
                        newFs = addInPath fs newPath (Dir, dir, 0)
                    in
                    parseCommand rest newFs newPath
        ('$':' ':'l':'s':_) -> parseCommand rest fs path
        (fileLine) -> 
            let newDescriptor = parseLsLine fileLine
                newFs = addInPath fs path newDescriptor
            in
                parseCommand rest newFs path

parseLsLine :: String -> Metadata
parseLsLine l =
    let
        splitted = break (==' ') l
        clean = (fst splitted, drop 1 $ snd splitted)
    in
        case (fst clean) of
            "dir" -> (Dir, snd clean, 0)
            otherwise -> (File, snd clean, read $ fst clean)

applyCd :: Path -> String -> Path
applyCd path ".." = take ((length path) -1) path
applyCd path dir = path ++ [dir] 

applyLs :: FileSystem -> Path -> [Metadata] -> FileSystem
applyLs fs path metas 
    | pathAlreadyAdded fs path = fs
    | otherwise = foldl (\acc meta -> addInPath acc path meta ) fs metas

findAtMostPredicate :: Int -> [Metadata] -> Metadata -> [Metadata]
findAtMostPredicate target acc (File, name, size) = acc
findAtMostPredicate target acc (Dir, name, size)
    | size <= target = acc ++ [(Dir, name, size)]
    | otherwise = acc

pathAlreadyAdded :: FileSystem -> Path -> Bool
pathAlreadyAdded fs (p:[]) = dirAlreadyAdded fs [p] p
pathAlreadyAdded fs path =
    let pathUntil = take ((length path)-1) path
        dirName = last path
    in
        dirAlreadyAdded fs pathUntil dirName 

dirAlreadyAdded :: FileSystem -> Path -> String -> Bool
dirAlreadyAdded (T.Node (rootFd, rootName, rootSize) nodes) (p:[]) name
  | rootFd == Dir && rootName == name = True
  | otherwise =
    (length $ filter (\(T.Node(fd, n, _) _) -> fd==Dir && n==name ) nodes) > 0
dirAlreadyAdded (T.Node (rootFd, rootName, rootSize) nodes) (p:next:path) name
  | rootFd == Dir &&  rootName == name = True
  | otherwise = 
    let elemIndex = findIndexOfPathInNodes nodes next 
    in dirAlreadyAdded (nodes !! elemIndex) ([next]++path) name

addInPath :: FileSystem -> Path -> Metadata -> FileSystem
-- if we get last element of path we append file to current root in filesystem
-- otherwise we rise exception
addInPath (T.Node (rootFd, rootName, rootSize) nodes) (p:[]) (metaFd, metaName, metaSize)
    | rootName == p = (T.Node (rootFd, rootName, rootSize + metaSize) $ nodes ++ [T.Node (metaFd, metaName, metaSize) []])
    | otherwise = throw LastOfPathNotFoundInTree 

-- in this case we need to find next element in path, recursive call if found
-- until we get to the last elem in path, updating the size of nodes 
-- as we traverse
addInPath (T.Node (rootFd, rootName, rootSize) nodes) (p:next:path) (metaFd, metaName, metaSize) 
  | rootName == p = 
    let elemIndex = findIndexOfPathInNodes nodes next 
        updatedNode = addInPath (nodes !! elemIndex) ([next]++path) (metaFd, metaName, metaSize) 
        updatedNodes = replace elemIndex updatedNode nodes
    in 
        (T.Node (rootFd, rootName, rootSize + metaSize) updatedNodes) 
  | otherwise = throw HeadOfFilesystemDoesntMatch 

findIndexOfPathInNodes :: [T.Tree Metadata] -> String -> Int
findIndexOfPathInNodes [] _ = throw NextInPathNotFound
findIndexOfPathInNodes ((T.Node (fd, name, size) _):xs) target
 | name == target = 0
 | otherwise = 1 + findIndexOfPathInNodes xs target 

testTree :: FileSystem
testTree = T.Node (Dir, "/", 48381165) [T.Node (Dir, "a", 94853) [T.Node (Dir, "e", 584) [T.Node (File, "i", 584) [] ], T.Node (File, "f", 29116) [], T.Node (File, "g", 2557) [], T.Node (File, "h.lst", 62596)[]], T.Node(File, "b.txt", 14848514)[], T.Node(File, "c.dat", 8504156)[], T.Node(Dir, "d", 24933642)[T.Node(File, "j", 4060174)[], T.Node(File, "d.log", 8033020)[], T.Node(File, "d.ext", 5626152)[], T.Node(File, "k", 7214296)[]  ] ] 

testCommands = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"

testParse = (parseInput $ lines testCommands) == testTree

testFindAtMost = (foldl (findAtMostPredicate 100000) [] $ T.flatten testTree) == [(Dir,"a",94853),(Dir,"e",584)]

replace index elem = map (\(index', elem') -> if index' == index then elem else elem') . zip [0..]
