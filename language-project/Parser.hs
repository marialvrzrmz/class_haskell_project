module Parser where

import Syntax
import Text.ParserCombinators.ReadP
    ( ReadP,
      between,
      chainl1,
      eof,
      many1,
      munch1,
      readP_to_S,
      satisfy,
      skipSpaces,
      string ) --allows combining small parsers into larger ones. 
import Control.Applicative ((<|>))

-- Utility parsers
whitespace :: ReadP () --whitespace handling
whitespace = skipSpaces

symbol :: String -> ReadP String --symbols
symbol s = string s <* whitespace

identifier :: ReadP String --Parses variable names. Variables consist of letters and underscores.
identifier = many1 (satisfy (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ "_")) <* whitespace

int :: ReadP Int --
int = read <$> munch1 (`elem` ['0'..'9']) <* whitespace

-- Core parsers
parseProgram :: ReadP Program
parseProgram = do
    symbol "begin"
    stmts <- parseStatements
    symbol "end"
    return $ BeginEnd stmts

parseStatements :: ReadP Statements
parseStatements = do
    stmt <- parseStmt
    rest <- (symbol ";" *> parseStatements) <|> return (End stmt)
    case rest of
        End _ -> return $ End stmt
        Seq _ _ -> return $ Seq stmt rest

parseStmt :: ReadP Stmt
parseStmt = parseAssign <|> parseWhile <|> parseFor <|> parsePrint

parseAssign :: ReadP Stmt
parseAssign = do
    var <- identifier
    symbol ":"
    typ <- parseType
    symbol "="
    Assign var typ <$> parseExpr


parseWhile :: ReadP Stmt
parseWhile = do
    symbol "while"
    cond <- between (symbol "(") (symbol ")") parseExpr
    While cond <$> parseStatements

parseFor :: ReadP Stmt
parseFor = do
    symbol "for"
    symbol "("
    initStmt <- parseAssign
    symbol ";"
    cond <- parseExpr
    symbol ";"
    stepStmt <- parseAssign
    symbol ")"
    For initStmt cond stepStmt <$> parseStatements

parsePrint :: ReadP Stmt
parsePrint = do
    symbol "print"
    Print <$> parseExpr

parseExpr :: ReadP Expr
parseExpr = parseBinExpr <|> parseValue <|> parseIfElse <|> parseRef

parseBinExpr :: ReadP Expr
parseBinExpr = chainl1 parseSimpleExpr (parseOp >>= \op -> return (`BinExpr` op))

parseSimpleExpr :: ReadP Expr
parseSimpleExpr = parseValue <|> parseRef <|> between (symbol "(") (symbol ")") parseExpr

parseValue :: ReadP Expr
parseValue = (Value . ValI <$> int)
         <|> (Value . ValB <$> ((symbol "true" >> return True) <|> (symbol "false" >> return False)))

parseIfElse :: ReadP Expr
parseIfElse = do
    symbol "if"
    cond <- between (symbol "(") (symbol ")") parseExpr
    symbol "then"
    trueExpr <- parseExpr
    symbol "else"
    IfElse cond trueExpr <$> parseExpr

parseRef :: ReadP Expr
parseRef = Ref <$> identifier

parseOp :: ReadP Op
parseOp = (symbol "+" >> return Add)
      <|> (symbol "-" >> return Sub)
      <|> (symbol "*" >> return Mul)
      <|> (symbol "/" >> return Div)
      <|> (symbol ">=" >> return GEq)

parseType :: ReadP Type
parseType = (symbol "int" >> return TypeI)
        <|> (symbol "bool" >> return TypeB)

-- Parsing entry point
parse :: String -> Maybe Program
parse input = case readP_to_S (whitespace >> parseProgram <* eof) input of
    [(result, "")] -> Just result
    _ -> Nothing
