import System.Process (CreateProcess(env))
-- single line comment

{-
    multi line comment
-}
-- grammar of tha language
{-
<program> -> begin <stmts> end
<stmts> -> <stmt> | <stmt>;<stmts> | <stmt> -> While <expr> do <stmt> end | <stmt> -> for <var> = <expr> to <expr> do <stmts> end
<stmt> -> <var> = <expr>
<var> -> string
<op> -> + | - | * | /
<expr> -> <val> | <expr> <op> <expr> | if <expr> then <expr> else <expr>
    | func <var> <expr> | <expr> <expr>
<val> -> integers | booleans
-}

-- abstract data types

data Program = BeginEnd Statements
    

data Statements = End Stmt | Seq Stmt Statements
    deriving Show

data Stmt = Assign Var Type Expr | While Expr Statements | For Var Expr Expr Statements
    deriving Show

type Var = String

data Type = TypeI | TypeB
    deriving Show

data Op = Add | Sub | Mul | Div | GEq | And | Or | Not
    deriving Show

data Expr = Value Val | BinExpr Expr Op Expr | IfElse Expr Expr Expr | Func Var Type Expr --expr is the body of the function
        | App Expr Expr | Ref Var | UnExpr Op Expr        -- for unary operations like not
    deriving Show

data Val = ValI Int | ValB Bool | ValE String
    deriving Show

type Env = [(Var, Val)]

data Result = Valid Env | Error String
    deriving Show

evaluateP :: Program -> Result
evaluateP (BeginEnd ss) = evaluateSS ss []

evaluateSS :: Statements -> Env -> Result
evaluateSS (End s) env = evaluateS s env
evaluateSS (Seq s ss) env = case evaluateS s env of
                                Valid env' -> evaluateSS ss env'
                                Error s -> Error s
    --evaluateSS ss (evaluateS s env) 

evaluateS :: Stmt -> Env -> Result
evaluateS (Assign x t e) env = case (evaluate e env, t) of
                                (ValI vi, TypeI) -> Valid $ (x, ValI vi) : env
                                (ValB vb, TypeB) -> Valid $ (x, ValB vb) : env
                                (ValI _, TypeB) -> Error $ "Type mismatch for" ++ x
                                (ValB _, TypeI) -> Error $ "Type mismatch" ++ x
                                (ValE s, _ ) -> Error s
evaluateS (While cond body) env = case evaluate cond env of
                                    ValB True -> case evaluateSS body env of -- If the condition is True, execute the body and then evaluate the while loop again
                                                    Valid env' -> evaluateS (While cond body) env'
                                                    Error err -> Error err
                                    ValB False -> Valid env -- If the condition is False, exit the loop and return the environment
                                    _ -> Error "While condition must be a boolean"
evaluateS (For var startExpr endExpr body) env = case (evaluate startExpr env, evaluate endExpr env) of
                                                    (ValI start, ValI end) -> evalForLoop var start end body env
                                                    _ -> Error "For loop bounds must be integers"
-- Helper function to handle For loop iterations
evalForLoop :: Var -> Int -> Int -> Statements -> Env -> Result
evalForLoop var start end body env
    | start > end = Valid env
    | otherwise = case evaluateSS body ((var, ValI start) : env) of
                    Valid env' -> evalForLoop var (start + 1) end body env'
                    Error err -> Error err
{-
    Var: The loop variable (e.g., i in for i = 1 to 5).
    Int (start): The starting value of the loop variable.
    Int (end): The ending value of the loop variable.
    Statements: The body of the loop, which contains one or more statements that are executed for each iteration.
    Env: The current environment (variable bindings).
    Result: Either a successful evaluation with the updated environment or an error message.
    | start > end = Valid env => This is the termination condition. When the loop variable (start) exceeds the end value, the loop stops, and the current environment is returned. This is how the loop knows when to stop iterating.
    Assign loop variable: The loop variable (var) is set to start in the environment ((var, ValI start) : env). This simulates the assignment of the current iteration's value to the loop variable.

    Evaluate body: The body of the loop (body) is evaluated using the updated environment. This runs the statements inside the loop for the current value of the loop variable.

    Recursive call:

    If the body of the loop evaluates successfully (Valid env'), the loop proceeds to the next iteration. The function recursively calls itself with the loop variable incremented by 1 (start + 1), repeating the process until the loop variable exceeds the end value.
    If an error occurs while evaluating the body (Error err), the loop terminates and returns the error.
-}

evaluateOp:: Int -> Op -> Int -> Val
evaluateOp i1 Add i2 = ValI (i1 + i2)
evaluateOp i1 Sub i2 = ValI (i1 - i2)
evaluateOp i1 Mul i2 = ValI (i1 * i2)
evaluateOp i1 Div i2 = ValI (i1 `div` i2)
evaluateOp i1 GEq i2 = ValB (i1 >= i2)

evaluate :: Expr -> Env -> Val
evaluate (Value v) _ = v

-- Evaluate binary operations like and, or, and arithmetic
evaluate (BinExpr e1 op e2) env = case (evaluate e1 env, evaluate e2 env) of
    (ValB b1, ValB b2) -> case op of
        And -> ValB (b1 && b2)
        Or  -> ValB (b1 || b2)
        _   -> ValE "Invalid boolean operation"
    (ValI i1, ValI i2) -> case op of
        Add -> evaluateOp i1 Add i2
        Sub -> evaluateOp i1 Sub i2
        Mul -> evaluateOp i1 Mul i2
        Div -> evaluateOp i1 Div i2
        GEq -> ValB (i1 >= i2)
        _   -> ValE "Operands should be integer"
    _ -> ValE "Type mismatch in binary expression"

-- Evaluate unary operations like not
evaluate (UnExpr Not e) env = case evaluate e env of  
    ValB b -> ValB (not b) --negates the boolean values
    _ -> ValE "Not operation requires a boolean expression"

-- Other cases (e.g., IfElse, Ref, Func, App, etc.) remain unchanged
evaluate (IfElse c e1 e2) env = case evaluate c env of
    ValB True  -> evaluate e1 env
    ValB False -> evaluate e2 env
    ValI 0     -> evaluate e2 env
    ValI _     -> evaluate e1 env
    _ -> ValE "Condition should be a boolean expression"
    
evaluate (Ref x) env = case lookup x env of
    Nothing -> ValE "Variable not in scope"
    Just v  -> v

evaluate (App (Func x t e) e2) env = case (evaluate e2 env, t) of 
    (ValI vi, TypeI) -> evaluate e ((x, ValI vi) : env)
    (ValB vb, TypeB) -> evaluate e ((x, ValB vb) : env)
    (ValI _, TypeB) -> ValE $ "Type mismatch for" ++ x
    (ValB _, TypeI) -> ValE $ "Type mismatch for" ++ x
    (ValE s, _) -> ValE s

evaluate _ _ = ValE "undefined"

precedence :: Op -> Int
precedence Add = 0
precedence Sub = 0
precedence Mul = 1
precedence Div = 1

main :: IO ()
main = do
    let env = [("x", ValI 5), ("y", ValB True)]
    
    -- Test 'and' (True && False -> False)
    let andTest = BinExpr (Value (ValB True)) And (Value (ValB False))
    print (evaluate andTest env) -- Output: ValB False

    -- Test 'or' (True || False -> True)
    let orTest = BinExpr (Value (ValB True)) Or (Value (ValB False))
    print (evaluate orTest env) -- Output: ValB True

    -- Test 'not' (not True -> False)
    let notTest = UnExpr Not (Value (ValB True))
    print (evaluate notTest env) -- Output: ValB False

    -- Test combined expressions
    let combinedTest = BinExpr (UnExpr Not (Value (ValB False))) And (Value (ValB True))
    print (evaluate combinedTest env) -- Output: ValB True
    -- Initialize the environment with variable 'x' set to 0
    let env = [("x", ValI 0)]
    
    -- Example of testing the for loop
    let forTest = For "i" (Value (ValI 1)) (Value (ValI 5)) 
                   (End (Assign "x" TypeI (BinExpr (Ref "x") Add (Ref "i"))))
    
    -- Example of testing the while loop
    let whileTest = While (BinExpr (Ref "x") GEq (Value (ValI 0)))
                          (Seq (Assign "x" TypeI (BinExpr (Ref "x") Sub (Value (ValI 1)))) 
                               (End (Assign "y" TypeI (Value (ValI 42)))))
    
    -- Running both loops
    let resultFor = evaluateS forTest env
    let resultWhile = evaluateS whileTest env
    
    -- Print the results
    putStrLn "For loop result:"
    print resultFor
    putStrLn "\nWhile loop result:"
    print resultWhile
