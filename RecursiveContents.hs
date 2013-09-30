module RecursiveContents (getRecursiveContents) where

import Control.Monad (forM, filterM, liftM)
import System.Directory (Permissions(..), getModificationTime, getPermissions, doesDirectoryExist, getDirectoryContents, getPermissions)
import System.FilePath ((</>), takeExtension)
import System.Time (ClockTime(..))
import Control.Exception (bracket, handle)
import System.IO (IOMode (..), hClose, hFileSize, openFile)
import Control.Exception

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory then getRecursiveContents path else return [path]
  return (concat paths)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do f <- getRecursiveContents path
                       return $ filter p f

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle noEx $ bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)

betterFind :: Predicate -> FilePath -> IO [FilePath]                                         
betterFind p path = do 
    c <- getRecursiveContents path
    filterM (check path p) c

check :: FilePath -> Predicate -> FilePath -> IO Bool
check path p name = do 
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            let bool = (p name perms size modified)
            -- putStrLn ("file: " ++ show (path)  ++ " name: " ++ show (name) ++ " bool: " ++ show (bool))
            return bool

noEx :: SomeException -> IO (Maybe a)
noEx _ =  return Nothing

type Predicate = FilePath
                 -> Permissions
                 -> Maybe Integer
                 -> ClockTime
                 -> Bool

myTest path _ (Just size) _ = takeExtension path == ".pdf" && size > 131072
myTest _ _ _ _ = False

type InfoP a = FilePath ->
               Permissions ->
               Maybe Integer ->
               ClockTime ->
               a
               

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g w x y z = f w x y z && g w x y z

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP = liftP2 (&&)
orP = liftP2 (||)

constP :: a -> InfoP a
constP k _ _ _ _ = k

liftP' q f k w x y z = f w x y z `q` constP k w x y z

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

myTest2 = (liftPath takeExtension `equalP` ".pdf") 

(==?) = equalP
(&&?) = andP
(>?) = greaterP

myTest3 = (liftPath takeExtension ==? ".pdf") &&? (sizeP >? 131072)

infix 4 ==?
infixr 3 &&?
infix 4 >?

myTest4 = liftPath takeExtension ==? ".pdf" &&? sizeP >? 131072

myTest5 = liftPath takeExtension ==? ".pdf"


data Info = Info {
    infoPath :: FilePath
  , infoPerms :: Maybe Permissions
  , infoSize :: Maybe Integer
  , infoModTime :: Maybe ClockTime
  } deriving (Eq, Ord, Show)


traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents) $ \info -> do
    if isDirectory info && infoPath info /= path
       then traverse order (infoPath info)
       else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents  path = do
  names <- getDirectoryContents path
  return $ filter (`notElem` [".", ".."]) names

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle noEx (Just `liftM` act)

getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)


traverseVerbose order path = do
     names <- getDirectoryContents path
     let usefulNames = filter (`notElem` [".", ".."]) names
     contents <- mapM getEntryName ("" : usefulNames)
     recursiveContents <- mapM recurse (order contents)
     return (concat recursiveContents)
  where getEntryName name = getInfo (path </> name)
        isDirectory info = case infoPerms info of
                             Nothing -> False
                             Just perms -> searchable perms
        recurse info = do
         if isDirectory info && infoPath info /= path
            then traverseVerbose order (infoPath info)
            else return [info]

data Iterate seed = Done { unwrap :: seed }
                  | Skip { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

foldTree :: Iterator a -> a -> FilePath -> IO a

foldTree iter initSeed path = do
     endSeed <- fold initSeed path
     return (unwrap endSeed)
  where
     fold seed subpath = getUsefulContents subpath >>= walk seed
  
