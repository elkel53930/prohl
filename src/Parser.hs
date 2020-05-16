module Parser where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.Combinator

import Type

funs1 = [ ("int", ToInt) -- functions that has 1 parameter.
        , ("real", ToReal)]

bops2 = [ ("*", Mul)
        , ("div", Div)
        , ("mod", Mod)
        ]
bops3 = [ ("+", Add)
        , ("-", Sub)
        ]
bops4 = [ ("<=", Le)
        , ("<", Lt)
        , (">=", Ge)
        , (">", Gt)
        ]
bops5 = [ ("==", Eq)
        , ("!=", Neq)]
bops6 = [ ("&", And)
        , ("|", Or)
        ]
bops7 = [ ("=>", Imp) -- Right-to-Left
        ] 

uops1 = [ ("-", Minus)
        , ("!", Not)
        ]

procedure :: Parser Procedure
procedure = do
    symbol "proc"
    name <- iden
    symbol "("
    vars <- declares
    symbol ")"
    h <- hoare
    return $ Procedure name vars h

hoare :: Parser Hoare
hoare = do
    symbol "{"
    pre <- expression
    symbol "}"
    prog <- statements
    symbol "{"
    post <- expression
    symbol "}"
    return $ Hoare pre prog post

declares :: Parser [DeclareVar]
declares = do
    ds <- many $ try declare
    return $ ds
    where
        declare = do
            i <- iden
            array <- optionMaybe $ choice [symbol "[]", symbol"."]
            case array of
                Nothing -> return $ IntVar i
                Just "[]" -> return $ ArrayVar i
                Just "." -> return $ RealVar i

statement :: Parser Statement
statement = try skip
        <|> try abort
        <|> try if_
        <|> try while
        <|> try assign
        <|> try update
        <|> try procCall

skip :: Parser Statement
skip = do
    symbol "skip"
    return Skip

abort :: Parser Statement
abort = do
    symbol "abort"
    return Abort

assign :: Parser Statement
assign = do
    v <- iden
    symbol ":="
    e <- expression
    return $ Assign v e

update :: Parser Statement
update = do
    v <- iden
    symbol "["
    index <- expression
    symbol "]"
    symbol ":="
    e <- expression
    return $ Update v index e

if_ :: Parser Statement
if_ = do
    symbol "if"
    cond <- expression
    symbol "then"
    t <- statements
    symbol "else"
    f <- statements
    symbol "fi"
    return $ If cond t f

while :: Parser Statement
while = do
    symbol "while"
    cond <- expression
    symbol "inv"
    inv <- expression
    symbol "do"
    stat <- statements
    symbol "od" 
    return $ While cond inv stat

statements :: Parser Statement
statements = statement `chainl1` seq
    where
        seq = do
            symbol ";"
            return $ Sequence

number :: Parser Expression
number = do
    x <- many1 digit
    d <- optionMaybe (do
        char '.'
        ds <- many digit
        return $ '.' : ds)
    skipSpace
    case d of
        Nothing -> return $ Number x
        Just demical -> return $ Number (x++demical)

boolean :: Parser Expression
boolean = do
    r <- choice [symbol "true", symbol "false"]
    return $ if r == "true"
        then Boolean True
        else Boolean False 

variable :: Parser Expression
variable = do
    v <- iden
    return $ Variable v

function1 :: (String, Function) -> Parser Expression
function1 (s,fun) = do
    symbol s
    symbol("(")
    p <- expression
    symbol(")")
    return $ Function1 fun p

procCall :: Parser Statement
procCall = do
    name <- iden
    symbol "("
    es <- many expression
    symbol ")"
    return $ ProcCall name es

select :: Parser Expression
select = do
    v <- iden
    symbol "["
    index <- expression
    symbol "]"
    return $ Select v index

store :: Parser Expression
store = do
    v <- iden
    symbol "["
    index <- expression
    symbol ":="
    e <- expression
    symbol "]"
    return $ Store v index e

symbol :: String -> Parser String
symbol s = do
    res <- string s
    skipSpace
    return res

iden :: Parser String
iden = do
    hd <- letter <|> char '_'
    tl <- many (letter <|> char '_' <|> digit)
    skipSpace
    return $ hd : tl

paren :: Parser Expression
paren = do
    symbol "("
    res <- expression
    symbol ")"
    return $ Paren res

unary :: (String, UnaryOperator) -> Parser (Expression -> Expression)
unary (s, op) = do
    symbol s
    return $ UnaryOp op

binary :: (String, BinaryOperator) -> Parser (Expression -> Expression -> Expression)
binary (s, op) = do
    symbol s
    return $ BinaryOp op

withUnary :: Parser Expression
withUnary = do
    uop <- choice $ map unary uops1
    n <- term
    return $ uop n

quantifier :: Parser Expression
quantifier = do
    q <- choice [symbol "ALL", symbol "EX"]
    v <- iden
    symbol "."
    e <- expression
    let f = if q == "ALL." then ALL else EX
    return $ Quantifier f v e

skipSpace :: Parser ()
skipSpace = do
    many $ sp <|> comment
    return ()
    where sp = do{space; return ()}

comment :: Parser ()
comment = do
    string "/*"
    commentEnd
    where
        commentEnd = try end <|> cmnt
        end = do
            string "*/"
            return ()
        cmnt = do
            anyChar
            commentEnd


term :: Parser Expression
term = try paren
   <|> number
   <|> try quantifier
   <|> try boolean
   <|> try store
   <|> try select
   <|> try ( choice $ map function1 funs1 )
   <|> try variable
   <|> withUnary

expression :: Parser Expression
expression = term `chainl1` (choice $ map (try . binary) bops2)
            `chainl1` (choice $ map (try . binary) bops3)
            `chainl1` (choice $ map (try . binary) bops4)
            `chainl1` (choice $ map (try . binary) bops5)
            `chainl1` (choice $ map (try . binary) bops6)
            `chainr1` (choice $ map (try . binary) bops7)

parseProcedures sourceName source = parse ((many1 procedure) <* eof) sourceName source
