module Declarator where

import Context

data Declarator_kind =
    DeclaratorIdentifier
  | DeclaratorFunction Context
  | DeclaratorOther
  deriving Show

type Declarator = (String, Declarator_kind)

identifier :: Declarator -> String
identifier = fst

kind :: Declarator -> Declarator_kind
kind = snd


identifier_declarator :: String -> Declarator
identifier_declarator i = (i, DeclaratorIdentifier)

function_declarator d ctx =
  case kind d of
    DeclaratorIdentifier -> (identifier d, DeclaratorFunction ctx)
    _                    -> d

other_declarator d =
  case kind d of
    DeclaratorIdentifier -> (identifier d, DeclaratorOther)
    _                    -> d

reinstall_function_context d =
  case kind d of
    DeclaratorFunction ctx ->
      do restore_context ctx
         declare_varname (identifier d)
         return ()

    _ -> return ()


