module Type where

data Quant = ALL | EX deriving (Show, Eq)
data BinaryOperator = Mul | Div | Mod | Add | Sub
                    | Le | Lt | Ge | Gt | Eq | Neq
                    | And | Or | Imp deriving (Eq, Ord, Show)
data UnaryOperator = Minus | Not deriving (Eq, Ord, Show)
data Function = ToInt | ToReal deriving(Eq, Ord, Show)

type Var = String

data Hoare = Hoare [DeclareVar] Expression Statement Expression -- Hoare triple

data DeclareVar = IntVar Var | RealVar Var | ArrayVar Var deriving Show

data Statement = Abort
               | Skip
               | Assign Var Expression -- var := expr
               | Update Var Expression Expression -- var[expr] := expr
               | If Expression Statement Statement -- if expr then statement else statement fi
               | While Expression Expression Statement -- while expr inv expr do statement od
               | Sequence Statement Statement  -- statement ; statement

data Expression = Number String
                | Boolean Bool
                | Variable Var
                | Function1 Function Expression
                | Select Var Expression
                | Store Var Expression Expression
                | Quantifier Quant Var Expression
                | BinaryOp BinaryOperator Expression Expression
                | UnaryOp UnaryOperator Expression
                | Paren Expression

instance Show Hoare where
    show (Hoare dv pre prog post) = show dv ++ " { " ++ show pre ++ " } " ++ show prog ++ "{ " ++ show post ++ " }"

instance Show Expression where
    show (Number a) = a
    show (Boolean b) = show b
    show (Variable v) = v
    show (Function1 f e) = show f ++ "(" ++ show e ++ ")"
    show (Select v index) = v ++ "[" ++ show index ++ "]"
    show (Store v index e) = v ++ "[" ++ show index ++ " := " ++ show e ++ "]"
    show (Quantifier q v e) = "(" ++ show q ++ "." ++ v ++ " " ++ show e ++ ")"
    show (BinaryOp op a b) = concat ["(", show a, " ", show op, " ", show b, ")"]
    show (UnaryOp op a) = (show op ++ " " ++ show a)
    show (Paren a) = show a

instance Show Statement where
    show (Abort) = "abort"
    show (Skip) = "skip"
    show (Assign v e) = "(" ++ v ++ " := " ++ show e ++ ")"
    show (Update v index e) = v ++ "[" ++ show index ++ " := " ++ show e ++ "]"
    show (If cond t f) = concat ["if ", show cond, " then " ,show t, " else ", show f, " fi"]
    show (While cond inv stat) = concat ["while ", show cond, " inv ", show inv, " do " , show stat, " od"]
    show (Sequence s1 s2) = "(" ++ show s1 ++ " ; " ++ show s2 ++ ")"

mkNot :: Expression -> Expression
mkNot e = UnaryOp Not e

mkBOp :: BinaryOperator -> Expression -> Expression -> Expression
mkBOp op = BinaryOp op

mkImp = mkBOp Imp
mkAnd = mkBOp And