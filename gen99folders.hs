import Data.List
import System.Directory
import System.IO
import System.Environment
import Control.Monad

-- this program creates folders
--    /folderName1
--    /folderName2
--    ...
--    /folderNameN  
-- for all i in N 
-- where 'folderName' is the first command line arg
--       'N' is the second command line arg

-- must check if file exists!
createFile :: String -> IO ()
createFile fileName = writeFile path fileData
    where path = "./"++fileName++"/"++fileName++".hs"
          fileData = "import Data.List\n\n-- Function goes here:\n    raight hurr ;)\n\nmain = do\n"

createFolder :: String -> IO ()
createFolder folderName = do
  createDirectoryIfMissing False ("./"++folderName)

genNfoldersNamed :: [String] -> IO ()
genNfoldersNamed [folderName,n] = genFldrs 1
    where m = read n
          genFldrs x 
              | x > m = putStrLn "Finished"
              | otherwise = do
                   let fileFldrName = folderName++(show x)
                   createFolder fileFldrName
                   createFile fileFldrName
                   genFldrs (x+1)
genNfoldersNamed _ = putStrLn "genNfoldersNamed takes exactly two arguments"

main = do
    argList <- getArgs
    genNfoldersNamed argList
