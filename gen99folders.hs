import Data.List
import System.Directory
import System.Environment
import Control.Monad

createFolder :: String -> Int -> IO ()
createFolder fldrName n = createDirectoryIfMissing False ("./"++fldrName++(show n))

genNfoldersNamed :: [String] -> IO ()
genNfoldersNamed [folderName,n] = genFldrs 1
    where m = read n
          genFldrs x 
              | x > m = putStrLn "Finished"
              | otherwise = do
                   createFolder folderName x
                   genFldrs (x+1)
genNfoldersNamed _ = putStrLn "genNfoldersNamed takes exactly two arguments"

main = do
    argList <- getArgs
    genNfoldersNamed argList
