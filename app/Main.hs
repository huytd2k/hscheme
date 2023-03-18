module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad (liftM)


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseEscape :: Parser Char
parseEscape = do
    char '\\'
    resp <- many letter
    return $ case resp of
        "n" -> '\n'
        "r" -> '\r'
        "t" -> '\t'
        _   -> error "Unknown escape sequence"

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (parseEscape <|> noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit
-- Ex1: Rewrite parseNumber using do notation
-- parseNumber = do 
--     digits <- many1 digit
--     return $ Number . read $ digits
-- Ex1: Rewrite parseNumber using explicit binding
-- parseNumber = many1 digit >>= return . Number . read

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "Not match: " ++ show err
    Right val -> "Found value " ++ (fmap show val)

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)