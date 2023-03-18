module Expr where

import Context
import Declarator

data AST =
    NoAST
  | DeclAST Declarator
  | StrAST String
  | CtxAST Context
  deriving Show


getCtx (CtxAST ctx)    = ctx
getCtx ast             = error $ "[getCtx] Can't happen: " ++ show ast

getDecl (DeclAST decl) = decl
getDecl ast            = error $ "[getDecl] Can't happen: " ++ show ast

getStr (StrAST str)    = str
getStr ast             = error $ "[getStr] Can't happen: " ++ show ast
