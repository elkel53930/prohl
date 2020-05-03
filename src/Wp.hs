module Wp where

import Type

wp :: Statement -> Expression -> Expression
wp Skip post = post
wp (Assign v e) post = subst post v e
wp (Update v i e) post = subst post v (Store v i e)
wp (If cond  t f) post = 
    let pret = wp t post
        pref = wp f post
    in BinaryOp And (BinaryOp Imp cond pret) (BinaryOp Imp (UnaryOp Not cond) pref)
wp (Sequence s1 s2) post = 
    let p = wp s2 post in wp s1 p
wp (While cond inv s) post = undefined

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

