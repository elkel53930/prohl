module Wp where

import Type

wp :: [Expression] -> Statement -> Expression -> ([Expression], Expression)
wp props s post = case s of
    Skip -> (props, post)
    Abort -> (props, Boolean False)
    Assign v e -> (props, subst post v e)
    Update v i e -> (props, subst post v (Store v i e))
    If cond  t f -> 
        let (props', pret) = wp props t post
            (props'', pref) = wp props' f post
        in (props'', mkAnd (mkImp cond pret) (mkImp (mkNot cond) pref))
    Sequence s1 s2 -> 
        let (props', p) = wp props s2 post
        in wp props' s1 p
    While cond inv s -> (p_inv : p_post : props', inv)
        where
            (props', p) = wp props s inv -- sの事前条件
            p_inv = mkImp (mkAnd cond inv) p -- invがループ不変条件であること
            p_post = mkImp (mkAnd (mkNot cond) inv) post -- ループを抜けた時点で事後条件postが成り立つ


spud :: Expression -> Var
spud (Variable v) = v
spud _ = undefined

-- Lexical substitution
-- Substitute v in expr with e.
subst :: Expression -> Var -> Expression -> Expression
subst expr v e = case expr of
    Number _ -> expr
    Boolean _ -> expr
    Variable v' -> if v'==v then e else (Variable v')
    Function1 f e' -> Function1 f (subst e' v e)
    Select v' e' -> Select (spud $ subst (Variable v') v e) (subst e' v e)
    Store v' e1 e2 -> Store (spud $ subst (Variable v') v e) (subst e1 v e) (subst e2 v e)
    BinaryOp op e1 e2 -> BinaryOp op (subst e1 v e) (subst e2 v e)
    UnaryOp op e' -> UnaryOp op (subst e' v e)
    Paren e' -> Paren (subst e' v e)
    Quantifier q v' e' ->
        if v'==v then
            expr
        else
            Quantifier q (spud $ subst (Variable v') v e) (subst e' v e)

mangling :: Procedure -> Procedure
mangling (Procedure name dv (Hoare pre pro post)) = Procedure name (map mand dv) (Hoare (mane pre) (mans pro) (mane post))
    where
    manv v = '$' : name ++ v
    mans stat = case stat of
        Sequence s1 s2 -> Sequence (mans s1) (mans s2)
        While e inv s -> While (mane e) (mane inv) (mans s)
        If cond st sf -> If (mane cond) (mans st) (mans sf)
        ProcCall p es -> ProcCall p (map mane es)
        Update v e1 e2 -> Update (manv v) (mane e1) (mane e2)
        Assign v e -> Assign (manv v) (mane e)
        otherwise -> stat
    mand dec = case dec of
        IntVar v -> IntVar (manv v)
        RealVar v -> RealVar (manv v)
        ArrayVar v -> ArrayVar (manv v)
    mane expr = case expr of
        Variable v -> Variable (manv v)
        Function1 f e -> Function1 f (mane e)
        Select v e -> Select (manv v) (mane e)
        Store v e1 e2 -> Store (manv v) (mane e1) (mane e2)
        Quantifier q v e -> Quantifier q (manv v) (mane e)
        BinaryOp op e1 e2 -> BinaryOp op (mane e1) (mane e2)
        UnaryOp op e -> UnaryOp op (mane e)
        Paren e -> Paren (mane e)
        otherwise -> expr

