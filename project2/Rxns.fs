module Rxns
open AST 

type Expr = Empty | EL of Species list 

type Rxns = Rxn of Expr * Expr * float 