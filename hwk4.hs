import Prelude hiding ( lookup )

data Exp = Num Int -- int
  | Plus Exp Exp -- e1 + e2
  | Minus Exp Exp -- e1 - e2
  | Times Exp Exp -- e1 * e2
  | Div Exp Exp -- e1 `div ` e2
  | Var String -- variable
  | Let String Exp Exp -- let x = e1 in e2
  | App Exp Exp -- e1 e2
  | Fn String Exp -- \x -> e
  deriving ( Show )

type Ctx = [( String , Val )] -- evaluation context

data Val = CVal Int -- int value
  | FnVal String Exp Ctx -- function value
  | Error String -- error value
  deriving ( Show )
  
toString :: Exp -> String
toString (Num x) = "Num " ++ show x
toString (Plus x y) = "(" ++ toString x ++ " + " ++ toString y ++ ")"
toString (Minus x y) = "(" ++ toString x ++ " - " ++ toString y ++ ")"
toString (Times x y) = "(" ++ toString x ++ " * " ++ toString y ++ ")"
toString (Div x y) = "(" ++ toString x ++ " div " ++ toString y ++ ")"
toString (Var x) = "Var " ++ show x
toString (Let x e1 e2) = "let val " ++ x ++ " = " ++ toString e1 ++ " in " ++ toString e2 ++ " end"
toString (App e1 e2) = "(" ++ toString e1 ++ " " ++ toString e2 ++ ")"
toString (Fn x e) = "\\" ++ x ++ " -> " ++ toString e

toStringValue :: Val -> String
toStringValue (CVal x) = "CVal " ++ show x
toStringValue (FnVal x e ctx) = "FnVal " ++ show x ++ " (" ++ toString e ++ ") " ++ show ctx
toStringValue (Error m) = m

lookup' :: Ctx -> String -> Val
lookup' [] y = Error ("Variable " ++ y ++ " is not found")
lookup' ((x, v) : ctx) y = if x == y then v else lookup' ctx y

eval :: Exp -> Ctx -> Val
eval (Num x) _ = CVal x
eval (Plus e1 e2) ctx =
    case (eval e1 ctx, eval e2 ctx) of
        (CVal a1, CVal a2) -> CVal (a1 + a2)
        (Error m, _) -> Error m
        (_, Error m) -> Error m
        (v1, v2) -> Error ("Plus error: " ++ toStringValue v2 ++ " is not a number")
eval (Minus e1 e2) ctx =
    case (eval e1 ctx, eval e2 ctx) of
        (CVal a1, CVal a2) -> CVal (a1 - a2)
        (Error m, _) -> Error m
        (_, Error m) -> Error m
        (v1, v2) -> Error ("Minus error: " ++ toStringValue v2 ++ " is not a number")
eval (Times e1 e2) ctx =
    case (eval e1 ctx, eval e2 ctx) of
        (CVal a1, CVal a2) -> CVal (a1 * a2)
        (Error m, _) -> Error m
        (_, Error m) -> Error m
        (v1, v2) -> Error ("Times error: " ++ toStringValue v1 ++ " or " ++ toStringValue v2 ++ " is not a number")
eval (Div e1 e2) ctx =
    case (eval e1 ctx, eval e2 ctx) of
        (_, CVal 0) -> Error ("Division by zero error: " ++ show (Div e1 e2))
        (CVal a1, CVal a2) -> CVal (a1 `div` a2)
        (Error m, _) -> Error m
        (_, Error m) -> Error m
        (v1, v2) -> Error ("Div error: " ++ toStringValue v1 ++ " or " ++ toStringValue v2 ++ " is not a number")
eval (Var x) ctx = lookup' ctx x
eval (Let x e1 e2) ctx =
    let a = eval e1 ctx
    in
        case a of
            Error m -> Error m
            v -> eval e2 ((x, v) : ctx)
eval (App e1 e2) ctx =
    case eval e1 ctx of
        FnVal x e ctx' -> eval e ((x, eval e2 ctx) : ctx')
        v -> Error ("Application error: " ++ toStringValue v ++ " is not a function")
eval (Fn x e) ctx = FnVal x e ctx

-- Test cases
main = do
  let t1 = Let "y" (Num 10)
             (Let "f" (Fn "x" (Plus (Var "x") (Var "y")))
                  (Let "y" (Num 20)
                       (App (Var "f") (Num 5))))
  let t2 = Let "y" (Num 10)
             (Let "f" (Fn "x" (Plus (Var "x") (Var "z")))
                  (Let "y" (Num 20)
                       (App (Var "f") (Num 5))))
  let t3 = Div (Num 10) (Num 0)
  let t4 = Plus (Num 10) (Minus (Num 20) (Fn "x" (Num 3)))
  let t5 = App (Num 10) (Num 20)
  let t6 = Let "f" (Num 10)
             (App (Var "f") (Num 20))
  let t7 = Let "x" (Plus (Num 10) (Fn "x" (Var "x")))
             (Plus (Num 0) (Num 20))
  let t = [t1, t2, t3, t4, t5, t6, t7]
  sequence_ $ map (\x -> print $ eval x []) t
