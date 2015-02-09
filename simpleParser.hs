module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

-- figure out how to parse numbers with "#-" prefixes without
-- bein overridden by parseAtom

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             deriving (Show)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of 
    Left err -> "No match: " ++ show err
    Right val -> "Found value "++(show val)

symbol :: Parser Char
symbol = oneOf "!$%&|*+ -/: <=? >@^_~#"

escapedChar :: Parser Char
escapedChar = oneOf ['\n','\r','\t','\\','\"']

-- fix reading of binary (add "#b")
radixPrefix :: Parser String
radixPrefix = string "#o" <|> string "#d" <|> string "#x"

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr = parseAtom 
        <|> parseString
        <|> parseNumber
--      <|> parseChar

parseString :: Parser LispVal
parseString = do 
    char '"'
    x <- many (symbol <|> escapedChar)
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of 
           "#t" -> Bool True
           "#f" -> Bool False
           otherwise -> Atom atom
    
parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
parseNumber = do
    prefix <- radixPrefix <|> many1 digit
    rest <- many digit
    return $ Number $ case prefix of
        "#o" -> fst $ (readOct rest) !! 0
        "#x" -> fst $ (readHex rest) !! 0
        "#d" -> read rest
        otherwise -> read prefix

-- add parseChar

main :: IO ()
main = do
    args <- getArgs
    putStrLn . readExpr $ args !! 0