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

-- Skip whitespace characters
whitespace :: ReadP () 
whitespace = skipSpaces

-- Parse a specific symbol with trailing whitespace
symbol :: String -> ReadP String 
symbol s = string s <* whitespace

-- Parse identifiers (variable names) using letters and underscores
identifier :: ReadP String 
identifier = many1 (satisfy (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ "_")) <* whitespace

-- Parse integer values
int :: ReadP Int 
int = read <$> munch1 (`elem` ['0'..'9']) <* whitespace

-- Top-level program parser: matches 'begin' ... 'end' structure
parseProgram :: ReadP Program
parseProgram = do
    symbol "begin"
    stmts <- parseStatements
    symbol "end"
    return $ BeginEnd stmts

-- Parse a sequence of statements, allowing multiple statements separated by semicolons
parseStatements :: ReadP Statements
parseStatements = do
    stmt <- parseStmt
    rest <- (symbol ";" *> parseStatements) <|> return (End stmt)
    case rest of
        End _ -> return $ End stmt
        Seq _ _ -> return $ Seq stmt rest

-- Choose between different types of statements (assign, while, for, print)
parseStmt :: ReadP Stmt
parseStmt = parseAssign <|> parseWhile <|> parseFor <|> parsePrint

-- Parse variable assignment with type annotation
parseAssign :: ReadP Stmt
parseAssign = do
    var <- identifier
    symbol ":"
    typ <- parseType
    symbol "="
    Assign var typ <$> parseExpr

-- Parse while loop: condition followed by statements
parseWhile :: ReadP Stmt
parseWhile = do
    symbol "while"
    cond <- between (symbol "(") (symbol ")") parseExpr
    While cond <$> parseStatements

-- Parse for loop with initialization, condition, and step statements
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

-- Parse print statement
parsePrint :: ReadP Stmt
parsePrint = do
    symbol "print"
    Print <$> parseExpr

-- Parse expressions: binary expressions, values, if-else, or references
parseExpr :: ReadP Expr
parseExpr = parseBinExpr <|> parseValue <|> parseIfElse <|> parseRef

-- Parse binary expressions with operator precedence
parseBinExpr :: ReadP Expr
parseBinExpr = chainl1 parseSimpleExpr (parseOp >>= \op -> return (`BinExpr` op))

-- Parse simple expressions: values, references, or parenthesized expressions
parseSimpleExpr :: ReadP Expr
parseSimpleExpr = parseValue <|> parseRef <|> between (symbol "(") (symbol ")") parseExpr

-- Parse literal values (integers and booleans)
parseValue :: ReadP Expr
parseValue = (Value . ValI <$> int)
         <|> (Value . ValB <$> ((symbol "true" >> return True) <|> (symbol "false" >> return False)))

-- Parse if-else expression
parseIfElse :: ReadP Expr
parseIfElse = do
    symbol "if"
    cond <- between (symbol "(") (symbol ")") parseExpr
    symbol "then"
    trueExpr <- parseExpr
    symbol "else"
    IfElse cond trueExpr <$> parseExpr

-- Parse variable references
parseRef :: ReadP Expr
parseRef = Ref <$> identifier

-- Parse arithmetic and comparison operators
parseOp :: ReadP Op
parseOp = (symbol "+" >> return Add)
      <|> (symbol "-" >> return Sub)
      <|> (symbol "*" >> return Mul)
      <|> (symbol "/" >> return Div)
      <|> (symbol ">=" >> return GEq)

-- Parse type annotations (int or bool)
parseType :: ReadP Type
parseType = (symbol "int" >> return TypeI)
        <|> (symbol "bool" >> return TypeB)

-- Main parsing function: tries to parse the entire input, returns Maybe Program
parse :: String -> Maybe Program
parse input = case readP_to_S (whitespace >> parseProgram <* eof) input of
    [(result, "")] -> Just result
    _ -> Nothing