-- as a challenge create a parse program

module Syntax where
import Language.Haskell.TH (viaStrategy)

-- grammar of the language
{-
<program> -> begin <stmts> end
<stmts> -> <stmt> | <stmt>;<stmts>
<stmt> -> <var> = <expr> | while <expr> <stmts> | for <stmt> <expr> <stmt> <stmts> | print <expr> -- | if <expr> then <stmts> else <stmts>
<var> -> string
<op> -> + | - | * | / | >=
<expr> -> <val> | <expr> <op> <expr> | if <expr> then <expr> else <expr>
    | func <var> <expr> | <expr> <expr> | <var>
<val> -> integers | booleans | error
<env> -> [(<var>, <val>)]
<result> -> <env> | error
-}

-- abstract data types

--data Program = BeginEnd Statements
newtype Program = BeginEnd Statements
    --deriving Show

data Statements = End Stmt | Seq Stmt Statements
    --deriving Show

data Stmt = Assign Var Type Expr
        | While Expr Statements
        | For Stmt Expr Stmt Statements
        | Print Expr
    --deriving Show
-- | IfElS Expr Statements Statements
type Var = String

data Type = TypeI | TypeB
    --deriving Show

data Op = Add | Sub | Mul | Div | GEq
    --deriving Show

data Expr = Value Val | BinExpr Expr Op Expr | IfElse Expr Expr Expr | Func Var Type Expr -- expr is the body of the fun
        | App Expr Expr | Ref Var
    --deriving Show
data Val = ValI Int | ValB Bool | ValE String 
    --deriving Show

type Env = [(Var, Val)]

data Result = Valid Env | Error String
    deriving Show

precedence :: Op -> Int
precedence Add = 0
precedence Sub = 0
precedence Mul = 1
precedence Div = 1
precedence GEq = -1

requiresParenth :: Op -> Expr -> Bool
requiresParenth op (BinExpr _ o2 _) = precedence op < precedence o2
requiresParenth _ _ = False 

instance Show Expr where
    show (Value v) = show v
    show (IfElse c e1 e2) = "if (" ++ show c ++ ") then " ++ show e1 ++ " else " ++ show e2
    show (Ref x) = x
    show (App e1 e2) = show e1 ++ "(" ++ show e2 ++ ")"
    show (Func x t e) = "\\" ++ x ++ ":" ++ show t ++ " -> " ++ show e
    show (BinExpr e1 op er) = if requiresParenth op er then 
                                    show e1 ++ " " ++ show op ++ "(" ++ show er ++ ")"
                                    else show e1 ++ " " ++ show op ++ " " ++ show er

instance Show Program where
    show(BeginEnd ss) = "begin\n" ++ show ss ++ "\nend"
instance Show Val where
    show (ValI vi) = show vi
    show (ValB vb) = show vb 
    show (ValE em) = "ERR: " ++ em

instance Show Statements where
    show (End s) = "\t" ++ show s ++ ";"
    show (Seq s ss) = "\t" ++ show s ++ ";\n" ++ show ss

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show GEq = ">="

instance Show Type where
    show TypeI = "int"
    show TypeB = "bool"

instance Show Stmt where
    show (Assign x t e) = x ++ " : " ++ show t ++ " = " ++ show e
    show (While e ss) = "while (" ++ show e ++ ")\n" ++ show ss 
    show (For si e su ss) = "for (" ++ show si ++ "; " ++ show e ++ "; " ++ show su ++ ")\n"
                                    ++ show ss 
    show (Print e) = "print " ++ show e 
