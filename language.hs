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

data Op = Add | Sub | Mul | Div | GEq
    deriving Show

data Expr = Value Val | BinExpr Expr Op Expr | IfElse Expr Expr Expr | Func Var Type Expr --expr is the body of the function
        | App Expr Expr | Ref Var
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
evaluate (IfElse c e1 e2) env = case evaluate c env of
                                ValB True -> evaluate e1 env
                                ValB False -> evaluate e2 env
                                ValI 0 -> evaluate e2 env
                                ValI _ -> evaluate e1 env
                                --ValI i -> error "condition should be a boolean expression"
evaluate (BinExpr e1 op e2) env = case (evaluate e1 env, evaluate e2 env) of
                                (ValI i1, ValI i2) -> evaluateOp i1 op i2
                                _ -> ValE "operands should be integer"
evaluate (Ref x) env = case lookup x env of
                        Nothing -> ValE "Variable not in scope"
                        Just v -> v
evaluate (App (Func x t e) e2) env = case (evaluate e2 env, t)of 
                                    (ValI vi, TypeI) -> evaluate e ((x, ValI vi) : env)
                                    (ValB vb, TypeB) -> evaluate e ((x, ValB vb) : env)
                                    (ValI _, TypeB) -> ValE $ "Type mismatch for" ++ x
                                    (ValB _, TypeI) -> ValE $ "Type mismatch for" ++ x
                                    (ValE s, _) -> ValE s

evaluate _ env = ValE "undefined" --Func or App e1 e2 where e1 is not a function

precedence :: Op -> Int
precedence Add = 0
precedence Sub = 0
precedence Mul = 1
precedence Div = 1


-- Example of a while loop: while x > 0 do x = x - 1 end
stmtWhile:: Stmt
stmtWhile = While (BinExpr (Ref "x") GEq (Value (ValI 0)))
                  (Seq (Assign "x" TypeI (BinExpr (Ref "x") Sub (Value (ValI 1)))) 
                       (End (Assign "y" TypeI (Value (ValI 42)))))

-- Example of a for loop: for i = 1 to 5 do x = x + i end
stmtFor :: Stmt
stmtFor = For "i" (Value (ValI 1)) (Value (ValI 5)) 
               (End (Assign "x" TypeI (BinExpr (Ref "x") Add (Ref "i"))))

-- Example program using both loops
exampleProgram :: Program
exampleProgram = BeginEnd (Seq stmtFor (End stmtWhile))
