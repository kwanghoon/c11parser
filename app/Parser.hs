module Parser where

-- Todo: implied production rules !!

import Attrs
import CommonParserUtil
import Token
import Expr
import Context
import Declarator
import C11Filter

-- | Utility
rule prodRule action               = (prodRule, action, Nothing  )
ruleWithPrec prodRule action prec  = (prodRule, action, Just prec)
ruleWithNoAction prodRule          = (prodRule, noAction, Nothing)
ruleNoActionWithPrec prodRule prec = (prodRule, noAction, Just prec)

noAction = \rhs -> return NoAST


-- | for writing grammar rules

nonTerminal h xs = h  ++ nt xs
  where
    nt []     = ""
    nt (x:xs) = "_" ++ x ++ nt xs

toProdRule [] = error "toProdRule: Cannot be empty"
toProdRule (x:xs) = x ++ " ->" ++ toProdRule' xs
  where
    toProdRule' []     = " "
    toProdRule' [x]    = " " ++ x
    toProdRule' (x:xs) = " " ++ x ++ toProdRule' xs
      
-- %inline ioption(X):
-- | /* nothing */
-- | X
--     {}

-- option(X):
-- | o = ioption(X)
--     { o }

option :: String -> [(ProdRuleStr, ParseAction Token AST IO LPS, ProdRulePrec)]   -- Todo: %inline? Done!
option x =
  [ (toProdRule [ nonTerminal "option" [x] ]   , noAction,                   Nothing)
  , (toProdRule [ nonTerminal "option" [x], x ], \rhs -> return (get rhs 1), Nothing)
  ]

-- (* By convention, [X*] is syntactic sugar for [list(X)]. *)

-- list(X):
-- | /* nothing */
-- | X list(X)
--     {}

list x =
  map toProdRule $
    map (nonTerminal "list" [x] :)
      [ []
      , [ x, nonTerminal "list" [x] ]
      ]

-- (* A list of A's and B's that contains exactly one A: *)

-- list_eq1(A, B):
-- | A B*
-- | B list_eq1(A, B)
--     {}

list_eq1 a b =
  map toProdRule $
    map (nonTerminal "list_eq1" [a,b] : )
      [ [ a, nonTerminal "list" [b] ]
      , [ b, nonTerminal "list_eq1" [a,b] ]
      ]

-- (* A list of A's and B's that contains at least one A: *)

-- list_ge1(A, B):
-- | A B*
-- | A list_ge1(A, B)
-- | B list_ge1(A, B)
--     {}

list_ge1 a b =   -- Todo: 함수에서 언급하는 Nonterminal에 대한 생산 규칙도 모두 나열!
  map toProdRule $
    map (nonTerminal "list_ge1" [a,b] :)
      [ [ a, nonTerminal "list" [b] ]
      , [ a, nonTerminal "list_ge1" [a,b] ]
      , [ b, nonTerminal "list_ge1" [a,b] ]
      ]

-- (* A list of A's, B's and C's that contains exactly one A and exactly one B: *)

-- list_eq1_eq1(A, B, C):
-- | A list_eq1(B, C)
-- | B list_eq1(A, C)
-- | C list_eq1_eq1(A, B, C)
--     {}

list_eq1_eq1 a b c =
  map toProdRule $
    map (nonTerminal "list_eq1_eq1" [a,b,c] :)
    [ [ a, nonTerminal "list_eq1" [b,c] ]
    , [ b, nonTerminal "list_eq1" [a,c] ]
    , [ c, nonTerminal "list_eq1_eq1" [a,b,c] ]
    ]

-- (* A list of A's, B's and C's that contains exactly one A and at least one B: *)

-- list_eq1_ge1(A, B, C):
-- | A list_ge1(B, C)
-- | B list_eq1(A, C)
-- | B list_eq1_ge1(A, B, C)
-- | C list_eq1_ge1(A, B, C)
--     {}

list_eq1_ge1 a b c =
  map toProdRule $
    map (nonTerminal "list_eq1_ge1" [a,b,c] :)
    [ [ a, nonTerminal "list_ge1" [b,c] ]
    , [ b, nonTerminal "list_eq1" [a,c] ]
    , [ b, nonTerminal "list_eq1_ge1" [a,b,c] ]
    , [ c, nonTerminal "list_eq1_ge1" [a,b,c] ]
    ]

-- [Copied]
-- scoped(X):
-- | ctx = save_context x = X
--     { restore_context ctx; x }

scoped x =
  [
    toProdRule [ nonTerminal "scoped" [x], "save_context", x ]
  ]


-- [Copied]
-- init_declarator_list(declarator):
-- | init_declarator(declarator)
-- | init_declarator_list(declarator) "," init_declarator(declarator)
--     {}

init_declarator_list declarator =
  map toProdRule $
    [ [ nonTerminal "init_declarator_list" [declarator]
      , nonTerminal "init_declarator" [declarator] ]
    ,
      [ nonTerminal "init_declarator_list" [declarator]
      , nonTerminal "init_declarator_list" [declarator]
      , ","
      , nonTerminal "init_declarator" [declarator] ]
    ]


-- [Copied]
-- init_declarator(declarator):
-- | declarator
-- | declarator "=" c_initializer
--     {}

init_declarator declarator =
  map toProdRule $
    map (nonTerminal "init_declarator" [declarator] :)
    [ [declarator] ]

    ++

    map (nonTerminal "init_declarator" [declarator] :)
    [ [ "declarator", "=", "c_initializer" ] ]
    

-- | a parser specification
parserSpec :: ParserSpec Token AST IO LPS
parserSpec = ParserSpec
  {
    startSymbol = "translation_unit_file'",

    -- (* There is a reduce/reduce conflict in the grammar. It corresponds to the
    --    conflict in the second declaration in the following snippet:

    --      typedef int T;
    --      int f(int(T));

    --    It is specified by 6.7.6.3 11: 'T' should be taken as the type of the
    --    parameter of the anonymous function taken as a parameter by f (thus,
    --    f has type (T -> int) -> int).

    --    The reduce/reduce conflict is solved by letting menhir reduce the production
    --    appearing first in this file. This is the reason why we have the
    --    [typedef_name_spec] proxy: it is here just to make sure the conflicting
    --    production appears before the other (which concerns [general_identifier]). *)
    
    
    -- (* These precedence declarations solve the dangling else conflict. *)
    tokenPrecAssoc =
      [ (Attrs.Nonassoc, [ "below_ELSE" ]) 
      , (Attrs.Nonassoc, [ "ELSE" ])
      ],
    
    parserSpecList = c11ParserSpecList,
    baseDir = "./",
    actionTblFile = "action_table.txt",  
    gotoTblFile = "goto_table.txt",
    grammarFile = "prod_rules.txt",
    parserSpecFile = "mygrammar.grm",
    genparserexe = "yapb-exe",

    synCompSpec = Just (SynCompSpec {isAbleToSearch=canSearch})
  }


c11ParserSpecList =
  [

    ruleWithNoAction "translation_unit_file' -> translation_unit_file",
    
    -- (* Upon finding an identifier, the lexer emits two tokens. The first token,
    --    [NAME], indicates that a name has been found; the second token, either [TYPE]
    --    or [VARIABLE], tells what kind of name this is. The classification is
    --    performed only when the second token is demanded by the parser. *)

    -- typedef_name:
    -- | i = NAME TYPE
    --     { i }

    rule "typedef_name -> NAME TYPE" (\rhs -> return (StrAST (getText rhs 1))),

    -- var_name:
    -- | i = NAME VARIABLE
    --     { i }

    rule "var_name -> NAME VARIABLE" (\rhs -> return (StrAST (getText rhs 1))),

    -- (* [typedef_name_spec] must be declared before [general_identifier], so that the
    --    reduce/reduce conflict is solved the right way. *)

    -- typedef_name_spec:
    -- | typedef_name
    --     {}

    ruleWithNoAction "typedef_name_spec -> typedef_name",

    -- general_identifier:
    -- | i = typedef_name
    -- | i = var_name
    --     { i }

    rule "general_identifier -> typedef_name" (\rhs -> return (get rhs 1)),

    rule "general_identifier -> var_name" (\rhs -> return (get rhs 1)),

    -- save_context:
    -- | (* empty *)
    --     { save_context () }

    rule "save_context -> " (\rhs -> do ctx <- save_context (); return (CtxAST ctx))
  ] ++

  -- scoped(X):
  -- | ctx = save_context x = X
  --     { restore_context ctx; x }

  -- ==> Defined above

  map (\prodRule ->
         rule
           prodRule
           (\rhs ->
              do let ctx = getCtx (get rhs 1)
                 restore_context ctx
                 return (get rhs 2))) (
    scoped "parameter_type_list" ++
    scoped "compound_statement"  ++
    scoped "selection_statement" ++
    scoped "iteration_statement" ++
    scoped "statement"           ++
    scoped "parameter_type_list"
  ) ++

 [
    -- (* [declarator_varname] and [declarator_typedefname] are like [declarator]. In
    --    addition, they have the side effect of introducing the declared identifier as
    --    a new variable or typedef name in the current context. *)

    -- declarator_varname:
    -- | d = declarator
    --     { declare_varname (identifier d); d }

    rule "declarator_varname -> declarator"
      (\rhs -> do let d = getDecl (get rhs 1)
                  declare_varname (identifier d)
                  return (DeclAST d)),

    -- declarator_typedefname:
    -- | d = declarator
    --     { declare_typedefname (identifier d); d }

    rule "declarator_typedefname -> declarator"
      (\rhs -> do let d = getDecl (get rhs 1)
                  declare_typedefname (identifier d)
                  return (DeclAST d)),

    -- (* End of the helpers, and beginning of the grammar proper: *)

    -- primary_expression:
    -- | var_name
    -- | CONSTANT
    -- | STRING_LITERAL
    -- | "(" expression ")"
    -- | generic_selection
    --     {}

    ruleWithNoAction "primary_expression -> var_name",

    ruleWithNoAction "primary_expression -> CONSTANT",

    ruleWithNoAction "primary_expression -> STRING_LITERAL",

    ruleWithNoAction "primary_expression -> ( expression )",

    ruleWithNoAction "primary_expression -> generic_selection",

    -- generic_selection:
    -- | "_Generic" "(" assignment_expression "," generic_assoc_list ")"
    --     {}

    ruleWithNoAction "generic_selection -> _Generic ( assignment_expression , generic_assoc_list )",

    -- generic_assoc_list:
    -- | generic_association
    -- | generic_assoc_list "," generic_association
    --     {}

    ruleWithNoAction "generic_assoc_list -> generic_association",

    ruleWithNoAction "generic_assoc_list -> generic_assoc_list , generic_association",

    -- generic_association:
    -- | type_name ":" assignment_expression
    -- | "default" ":" assignment_expression
    --     {}

    ruleWithNoAction "generic_association -> type_name : assignment_expression",

    ruleWithNoAction "generic_association -> default : assignment_expression",

    -- postfix_expression:
    -- | primary_expression
    -- | postfix_expression "[" expression "]"
    -- | postfix_expression "(" argument_expression_list? ")"
    -- | postfix_expression "." general_identifier
    -- | postfix_expression "->" general_identifier
    -- | postfix_expression "++"
    -- | postfix_expression "--"
    -- | "(" type_name ")" "{" initializer_list ","? "}"
    --     {}

    ruleWithNoAction "postfix_expression -> primary_expression",

    ruleWithNoAction "postfix_expression -> postfix_expression [ expression ]",

    ruleWithNoAction $ "postfix_expression -> postfix_expression ( " ++ nonTerminal "option" [ "argument_expression_list" ] ++ " )",

    ruleWithNoAction "postfix_expression -> postfix_expression . general_identifier",

    ruleWithNoAction "postfix_expression -> postfix_expression -> general_identifier",

    ruleWithNoAction "postfix_expression -> postfix_expression ++",

    ruleWithNoAction "postfix_expression -> postfix_expression --",

    ruleWithNoAction $ "postfix_expression -> ( type_name ) { initializer_list " ++ nonTerminal "option" [ "comma" ] ++ " }",


    -- argument_expression_list:
    -- | assignment_expression
    -- | argument_expression_list "," assignment_expression
    --     {}

    ruleWithNoAction "argument_expression_list -> assignment_expression",

    ruleWithNoAction "argument_expression_list -> argument_expression_list , assignment_expression",

    -- unary_expression:
    -- | postfix_expression
    -- | "++" unary_expression
    -- | "--" unary_expression
    -- | unary_operator cast_expression
    -- | "sizeof" unary_expression
    -- | "sizeof" "(" type_name ")"
    -- | "_Alignof" "(" type_name ")"
    --     {}

    ruleWithNoAction "unary_expression -> postfix_expression",

    ruleWithNoAction "unary_expression -> ++ unary_expression",

    ruleWithNoAction "unary_expression -> -- unary_expression",

    ruleWithNoAction "unary_expression -> unary_operator cast_expression",

    ruleWithNoAction "unary_expression -> sizeof unary_expression",

    ruleWithNoAction "unary_expression -> sizeof ( type_name )",

    ruleWithNoAction "unary_expression -> _Alignof ( type_name )",

    -- unary_operator:
    -- | "&"
    -- | "*"
    -- | "+"
    -- | "-"
    -- | "~"
    -- | "!"
    --     {}

    ruleWithNoAction "unary_operator -> &",

    ruleWithNoAction "unary_operator -> *",

    ruleWithNoAction "unary_operator -> +",

    ruleWithNoAction "unary_operator -> -",

    ruleWithNoAction "unary_operator -> ~",

    ruleWithNoAction "unary_operator -> !",

    -- cast_expression:
    -- | unary_expression
    -- | "(" type_name ")" cast_expression
    --     {}

    ruleWithNoAction "cast_expression -> unary_expression",

    ruleWithNoAction "cast_expression -> ( type_name ) cast_expression",


    -- multiplicative_operator:
    --   "*" | "/" | "%" {}

    ruleWithNoAction "multiplicative_operator -> *",

    ruleWithNoAction "multiplicative_operator -> /",

    ruleWithNoAction "multiplicative_operator -> %",


    -- multiplicative_expression:
    -- | cast_expression
    -- | multiplicative_expression multiplicative_operator cast_expression
    --     {}

    ruleWithNoAction "multiplicative_expression -> cast_expression",

    ruleWithNoAction "multiplicative_expression -> multiplicative_expression multiplicative_operator cast_expression",


    -- additive_operator:
    --   "+" | "-" {}

    ruleWithNoAction "additive_operator -> +",

    ruleWithNoAction "additive_operator -> -",


    -- additive_expression:
    -- | multiplicative_expression
    -- | additive_expression additive_operator multiplicative_expression
    --     {}

    ruleWithNoAction "additive_expression -> multiplicative_expression",

    ruleWithNoAction "additive_expression -> additive_expression additive_operator multiplicative_expression",


    -- shift_operator:
    --   "<<" | ">>" {}

    ruleWithNoAction "shift_operator -> <<",

    ruleWithNoAction "shift_operator -> >>",


    -- shift_expression:
    -- | additive_expression
    -- | shift_expression shift_operator additive_expression
    --     {}

    ruleWithNoAction "shift_expression -> additive_expression",

    ruleWithNoAction "shift_expression -> shift_expression shift_operator additive_expression",


    -- relational_operator:
    --   "<" | ">" | "<=" | ">=" {}

    ruleWithNoAction "relational_operator -> <", 

    ruleWithNoAction "relational_operator -> >", 

    ruleWithNoAction "relational_operator -> <=", 

    ruleWithNoAction "relational_operator -> >=", 


    -- relational_expression:
    -- | shift_expression
    -- | relational_expression relational_operator shift_expression
    --     {}

    ruleWithNoAction "relational_expression -> shift_expression",

    ruleWithNoAction "relational_expression -> relational_expression relational_operator shift_expression",


    -- equality_operator:
    --   "==" | "!=" {}

    ruleWithNoAction "equality_operator -> ==",

    ruleWithNoAction "equality_operator -> !=",

    -- equality_expression:
    -- | relational_expression
    -- | equality_expression equality_operator relational_expression
    --     {}

    ruleWithNoAction "equality_expression -> relational_expression",

    ruleWithNoAction "equality_expression -> equality_expression equality_operator relational_expression",

    -- and_expression:
    -- | equality_expression
    -- | and_expression "&" equality_expression
    --     {}

    ruleWithNoAction "and_expression -> equality_expression",

    ruleWithNoAction "and_expression -> and_expression & equality_expression",


    -- exclusive_or_expression:
    -- | and_expression
    -- | exclusive_or_expression "^" and_expression
    --     {}

    ruleWithNoAction "exclusive_or_expression -> and_expression",

    ruleWithNoAction "exclusive_or_expression -> exclusive_or_expression ^ and_expression",


    -- inclusive_or_expression:
    -- | exclusive_or_expression
    -- | inclusive_or_expression "|" exclusive_or_expression
    --     {}

    ruleWithNoAction "inclusive_or_expression -> exclusive_or_expression",

    ruleWithNoAction "inclusive_or_expression -> inclusive_or_expression | exclusive_or_expression",


    -- logical_and_expression:
    -- | inclusive_or_expression
    -- | logical_and_expression "&&" inclusive_or_expression
    --     {}

    ruleWithNoAction "logical_and_expression -> inclusive_or_expression",

    ruleWithNoAction "logical_and_expression -> logical_and_expression && inclusive_or_expression",


    -- logical_or_expression:
    -- | logical_and_expression
    -- | logical_or_expression "||" logical_and_expression
    --     {}

    ruleWithNoAction "logical_or_expression -> logical_and_expression",

    ruleWithNoAction "logical_or_expression -> logical_or_expression || logical_and_expression",


    -- conditional_expression:
    -- | logical_or_expression
    -- | logical_or_expression "?" expression ":" conditional_expression
    --     {}

    ruleWithNoAction "conditional_expression -> logical_or_expression",

    ruleWithNoAction "conditional_expression -> logical_or_expression ? expression : conditional_expression",


    -- assignment_expression:
    -- | conditional_expression
    -- | unary_expression assignment_operator assignment_expression
    --     {}

    ruleWithNoAction "assignment_expression -> conditional_expression",

    ruleWithNoAction "assignment_expression -> unary_expression assignment_operator assignment_expression",


    -- assignment_operator:
    -- | "="
    -- | "*="
    -- | "/="
    -- | "%="
    -- | "+="
    -- | "-="
    -- | "<<="
    -- | ">>="
    -- | "&="
    -- | "^="
    -- | "|="
    --     {}

    ruleWithNoAction "assignment_operator -> =",  

    ruleWithNoAction "assignment_operator -> *=",  

    ruleWithNoAction "assignment_operator -> /=",  

    ruleWithNoAction "assignment_operator -> %=",  

    ruleWithNoAction "assignment_operator -> +=",  

    ruleWithNoAction "assignment_operator -> -=",  

    ruleWithNoAction "assignment_operator -> <<=",  

    ruleWithNoAction "assignment_operator -> >>=",  

    ruleWithNoAction "assignment_operator -> &=",  

    ruleWithNoAction "assignment_operator -> ^=",  

    ruleWithNoAction "assignment_operator -> |=",  

    -- expression:
    -- | assignment_expression
    -- | expression "," assignment_expression
    --     {}

    ruleWithNoAction "expression -> assignment_expression",

    ruleWithNoAction "expression -> expression , assignment_expression",


    -- constant_expression:
    -- | conditional_expression
    --     {}

    ruleWithNoAction "constant_expression -> conditional_expression",

    -- (* We separate type declarations, which contain an occurrence of ["typedef"], and
    --    normal declarations, which do not. This makes it possible to distinguish /in
    --    the grammar/ whether a declaration introduces typedef names or variables in
    --    the context. *)

    -- declaration:
    -- | declaration_specifiers         init_declarator_list(declarator_varname)?     ";"
    -- | declaration_specifiers_typedef init_declarator_list(declarator_typedefname)? ";"
    -- | static_assert_declaration
    --     {}

    ruleWithNoAction $ toProdRule
      [ "declaration",
        "declaration_specifiers",
        nonTerminal "option"
         [ nonTerminal "init_declarator_list" [ "declarator_varname" ] ], ";" ],

    ruleWithNoAction $ toProdRule
      [ "declaration",
        "declaration_specifiers_typedef",
        nonTerminal "option"
           [ nonTerminal "init_declarator_list" [ "declarator_typedefname" ] ], ";" ],

    ruleWithNoAction "declaration -> static_assert_declaration",


    -- (* [declaration_specifier] corresponds to one declaration specifier in the C18
    --    standard, deprived of "typedef" and of type specifiers. *)

    -- declaration_specifier:
    -- | storage_class_specifier (* deprived of "typedef" *)
    -- | type_qualifier
    -- | function_specifier
    -- | alignment_specifier
    --     {}

    ruleWithNoAction "declaration_specifier -> storage_class_specifier",

    ruleWithNoAction "declaration_specifier -> type_qualifier",

    ruleWithNoAction "declaration_specifier -> function_specifier",

    ruleWithNoAction "declaration_specifier -> alignment_specifier",


    -- (* [declaration_specifiers] requires that at least one type specifier be
    --    present, and, if a unique type specifier is present, then no other type
    --    specifier be present. In other words, one should have either at least one
    --    nonunique type specifier, or exactly one unique type specifier.

    --    This is a weaker condition than 6.7.2 2. Encoding this condition in the
    --    grammar is necessary to disambiguate the example in 6.7.7 6:

    --      typedef signed int t;
    --      struct tag {
    --      unsigned t:4;
    --      const t:5;
    --      };

    --    The first field is a named t, while the second is unnamed of type t.

    --    [declaration_specifiers] forbids the ["typedef"] keyword. *)

    -- declaration_specifiers:
    -- | list_eq1(type_specifier_unique,    declaration_specifier)
    -- | list_ge1(type_specifier_nonunique, declaration_specifier)
    --     {}

    ruleWithNoAction $ toProdRule
      [ "declaration_specifiers",
        nonTerminal "list_eq1" ["type_specifier_unique", "declaration_specifier" ] ], 

    ruleWithNoAction $ toProdRule
      [ "declaration_specifiers",
        nonTerminal "list_ge1" ["type_specifier_nonunique", "declaration_specifier" ] ],

    -- (* [declaration_specifiers_typedef] is analogous to [declaration_specifiers],
    --    but requires the ["typedef"] keyword to be present (exactly once). *)

    -- declaration_specifiers_typedef:
    -- | list_eq1_eq1("typedef", type_specifier_unique,    declaration_specifier)
    -- | list_eq1_ge1("typedef", type_specifier_nonunique, declaration_specifier)
    --     {}

    ruleWithNoAction $ toProdRule
      [ "declaration_specifiers_typedef",
        nonTerminal "list_eq1_eq1" ["typedef", "type_specifier_unique", "declaration_specifier"] ],

    ruleWithNoAction $ toProdRule
      [ "declaration_specifiers_typedef",
        nonTerminal "list_eq1_ge1" ["typedef", "type_specifier_nonunique", "declaration_specifier" ] ]

  ] ++

  -- (* The parameter [declarator] in [init_declarator_list] and [init_declarator]
  --    is instantiated with [declarator_varname] or [declarator_typedefname]. *)

  -- init_declarator_list(declarator):
  -- | init_declarator(declarator)
  -- | init_declarator_list(declarator) "," init_declarator(declarator)
  --     {}

  -- ==> Defined above

  map ruleWithNoAction (
    init_declarator_list "declarator_varname" ++
    init_declarator_list "declarator_typedefname"
  ) ++

  -- init_declarator(declarator):
  -- | declarator
  -- | declarator "=" c_initializer
  --     {}

  -- ==> Defined above

  map ruleWithNoAction (
    init_declarator "declarator_varname" ++
    init_declarator "declarator_typedefname"
  ) ++

  [
    -- (* [storage_class_specifier] corresponds to storage-class-specifier in the
    --    C18 standard, deprived of ["typedef"] (which receives special treatment). *)

    -- storage_class_specifier:
    -- | "extern"
    -- | "static"
    -- | "_Thread_local"
    -- | "auto"
    -- | "register"
    --     {}

    ruleWithNoAction "storage_class_specifier -> extern",

    ruleWithNoAction "storage_class_specifier -> static",

    ruleWithNoAction "storage_class_specifier -> _Thread_local",

    ruleWithNoAction "storage_class_specifier -> auto",

    ruleWithNoAction "storage_class_specifier -> register",


    -- (* A type specifier which can appear together with other type specifiers. *)

    -- type_specifier_nonunique:
    -- | "char"
    -- | "short"
    -- | "int"
    -- | "long"
    -- | "float"
    -- | "double"
    -- | "signed"
    -- | "unsigned"
    -- | "_Complex"
    --     {}

    ruleWithNoAction "type_specifier_nonunique -> char",

    ruleWithNoAction "type_specifier_nonunique -> short",

    ruleWithNoAction "type_specifier_nonunique -> int",

    ruleWithNoAction "type_specifier_nonunique -> long",

    ruleWithNoAction "type_specifier_nonunique -> float",

    ruleWithNoAction "type_specifier_nonunique -> double",

    ruleWithNoAction "type_specifier_nonunique -> signed",

    ruleWithNoAction "type_specifier_nonunique -> unsigned",

    ruleWithNoAction "type_specifier_nonunique -> _Complex",


    -- (* A type specifier which cannot appear together with other type specifiers. *)

    -- type_specifier_unique:
    -- | "void"
    -- | "_Bool"
    -- | atomic_type_specifier
    -- | struct_or_union_specifier
    -- | enum_specifier
    -- | typedef_name_spec
    --     {}

    ruleWithNoAction "type_specifier_unique -> void",

    ruleWithNoAction "type_specifier_unique -> _Bool",

    ruleWithNoAction "type_specifier_unique -> atomic_type_specifier",

    ruleWithNoAction "type_specifier_unique -> struct_or_union_specifier",

    ruleWithNoAction "type_specifier_unique -> enum_specifier",

    ruleWithNoAction "type_specifier_unique -> typedef_name_spec",


    -- struct_or_union_specifier:
    -- | struct_or_union general_identifier? "{" struct_declaration_list "}"
    -- | struct_or_union general_identifier
    --     {}

    ruleWithNoAction $ toProdRule
      [ "struct_or_union_specifier",
        "struct_or_union",
        nonTerminal "option" ["general_identifier"], "{", "struct_declaration_list", "}" ],

    ruleWithNoAction "struct_or_union_specifier -> struct_or_union general_identifier",


    -- struct_or_union:
    -- | "struct"
    -- | "union"
    --     {}

    ruleWithNoAction "struct_or_union -> struct",

    ruleWithNoAction "struct_or_union -> union",


    -- struct_declaration_list:
    -- | struct_declaration
    -- | struct_declaration_list struct_declaration
    --     {}

    ruleWithNoAction "struct_declaration_list -> struct_declaration",

    ruleWithNoAction "struct_declaration_list -> struct_declaration_list struct_declaration",


    -- struct_declaration:
    -- | specifier_qualifier_list struct_declarator_list? ";"
    -- | static_assert_declaration
    --     {}

    ruleWithNoAction $ toProdRule
      [ "struct_declaration",
        "specifier_qualifier_list",
        nonTerminal "option" ["struct_declarator_list"], ";" ],

    ruleWithNoAction "struct_declaration -> static_assert_declaration",


    -- (* [specifier_qualifier_list] is as in the standard, except it also encodes the
    --    same constraint as [declaration_specifiers] (see above). *)

    -- specifier_qualifier_list:
    -- | list_eq1(type_specifier_unique,    type_qualifier | alignment_specifier {})
    -- | list_ge1(type_specifier_nonunique, type_qualifier | alignment_specifier {})
    --     {}

    ruleWithNoAction $ toProdRule
      [ "specifier_qualifier_list",
        nonTerminal "list_eq1" ["type_specifier_unique", "type_qualifier"] ],

    ruleWithNoAction $ toProdRule
      [ "specifier_qualifier_list",
        nonTerminal "list_eq1" ["type_specifier_unique", "alignment_specifier"] ],

    ruleWithNoAction $ toProdRule
      [ "specifier_qualifier_list",
        nonTerminal "list_ge1" ["type_specifier_nonunique", "type_qualifier"] ],

    ruleWithNoAction $ toProdRule
      [ "specifier_qualifier_list",
        nonTerminal "list_ge1" ["type_specifier_nonunique", "alignment_specifier"] ],


    -- struct_declarator_list:
    -- | struct_declarator
    -- | struct_declarator_list "," struct_declarator
    --     {}

    ruleWithNoAction "struct_declarator_list -> struct_declarator",

    ruleWithNoAction "struct_declarator_list -> struct_declarator_list , struct_declarator",


    -- struct_declarator:
    -- | declarator
    -- | declarator? ":" constant_expression
    --     {}

    ruleWithNoAction "struct_declarator -> declarator",

    ruleWithNoAction $ toProdRule
      [ "struct_declarator",
        nonTerminal "option" ["declarator"], ":", "constant_expression" ],


    -- enum_specifier:
    -- | "enum" general_identifier? "{" enumerator_list ","? "}"
    -- | "enum" general_identifier
    --     {}

    ruleWithNoAction $ toProdRule
      [ "enum_specifier",
        "enum", nonTerminal "option" ["general_identifier"], "{", "enumerator_list",
        nonTerminal "option" ["comma"], "}" ],   --Todo: comma instead of ,

    ruleWithNoAction "enum_specifier -> enum general_identifier",


    -- enumerator_list:
    -- | enumerator
    -- | enumerator_list "," enumerator
    --     {}

    ruleWithNoAction "enumerator_list -> enumerator",

    ruleWithNoAction "enumerator_list -> enumerator_list , enumerator",


    -- enumerator:
    -- | i = enumeration_constant
    -- | i = enumeration_constant "=" constant_expression
    --     { declare_varname i }

    rule "enumerator -> enumeration_constant"
      (\rhs -> do let i = getStr (get rhs 1)
                  declare_varname i
                  return NoAST),

    rule "enumerator -> enumeration_constant = constant_expression"
      (\rhs -> do let i = getStr (get rhs 1)
                  declare_varname i
                  return NoAST),


    -- enumeration_constant:
    -- | i = general_identifier
    --     { i }

    rule "enumeration_constant -> general_identifier"
      (\rhs -> return (get rhs 1)),


    -- atomic_type_specifier:
    -- | "_Atomic" "(" type_name ")"
    -- | "_Atomic" ATOMIC_LPAREN type_name ")"
    --     {}

    ruleWithNoAction "atomic_type_specifier -> _Atomic ( type_name )",

    ruleWithNoAction "atomic_type_specifier -> _Atomic ATOMIC_LPAREN type_name )",


    -- type_qualifier:
    -- | "const"
    -- | "restrict"
    -- | "volatile"
    -- | "_Atomic"
    --     {}

    ruleWithNoAction "type_qualifier -> const",

    ruleWithNoAction "type_qualifier -> restrict",

    ruleWithNoAction "type_qualifier -> volatile",

    ruleWithNoAction "type_qualifier -> _Atomic",


    -- function_specifier:
    --   "inline" | "_Noreturn"
    --     {}

    ruleWithNoAction "function_specifier -> inline",

    ruleWithNoAction "function_specifier -> _Noreturn",


    -- alignment_specifier:
    -- | "_Alignas" "(" type_name ")"
    -- | "_Alignas" "(" constant_expression ")"
    --     {}

    ruleWithNoAction "alignment_specifier -> _Alignas ( type_name )",

    ruleWithNoAction "alignment_specifier -> _Alignas ( constant_expression )",


    -- declarator:
    -- | ioption(pointer) d = direct_declarator
    --     { other_declarator d }

    rule "declarator -> direct_declarator"
      (\rhs -> do let d = getDecl (get rhs 1)
                  let od = other_declarator d
                  return (DeclAST od)),
    
    rule "declarator -> pointer direct_declarator"
      (\rhs -> do let d = getDecl (get rhs 2)
                  let od = other_declarator d
                  return (DeclAST od))
  ]

  ++

  let od_action rhs =
        do let d = getDecl (get rhs 1)
           let od = other_declarator d
           return (DeclAST od)
  in
  [
    -- (* The occurrences of [save_context] inside [direct_declarator] and
    --    [direct_abstract_declarator] seem to serve no purpose. In fact, they are
    --    required in order to avoid certain conflicts. In other words, we must save
    --    the context at this point because the LR automaton is exploring multiple
    --    avenues in parallel and some of them do require saving the context. *)

    -- direct_declarator:
    -- | i = general_identifier
    --     { identifier_declarator i }
    -- | "(" save_context d = declarator ")"
    --     { d }
    -- | d = direct_declarator "[" type_qualifier_list? assignment_expression? "]"
    -- | d = direct_declarator "[" "static" type_qualifier_list? assignment_expression "]"
    -- | d = direct_declarator "[" type_qualifier_list "static" assignment_expression "]"
    -- | d = direct_declarator "[" type_qualifier_list? "*" "]"
    --     { other_declarator d }
    -- | d = direct_declarator "(" ctx = scoped(parameter_type_list) ")"
    --     { function_declarator d ctx }
    -- | d = direct_declarator "(" save_context identifier_list? ")"
    --     { other_declarator d }

    rule "direct_declarator -> general_identifier"
      (\rhs -> do let i = getStr (get rhs 1)
                  let d = identifier_declarator i
                  return (DeclAST d)),

    rule "direct_declarator -> ( save_context declarator )"
      (\rhs -> return (get rhs 3)),

    rule (toProdRule
      [ "direct_declarator",
        "direct_declarator", "[", nonTerminal "option" ["type_qualifier_list"],
         nonTerminal "option" ["assignment_expression"], "]" ])
      od_action,

    rule (toProdRule
      [ "direct_declarator",
        "direct_declarator", "[", "static",
        nonTerminal "option" ["type_qualifier_list"], " assignment_expression", "]" ])
      od_action,

    rule "direct_declarator -> direct_declarator [ type_qualifier_list static assignment_expression ]" od_action,

    rule (toProdRule
      [ "direct_declarator", "direct_declarator", "[",
        nonTerminal "option" ["type_qualifier_list"], "*", "]" ])
      od_action,

    rule (toProdRule
      [ "direct_declarator", "direct_declarator", "(",
        nonTerminal "scoped" ["parameter_type_list"], ")" ])
      (\rhs -> do let d   = getDecl (get rhs 1)
                  let ctx = getCtx  (get rhs 3)
                  let fd  = function_declarator d ctx
                  return (DeclAST fd)),

    rule (toProdRule
      [ "direct_declarator",
        "direct_declarator", "(", "save_context",
        nonTerminal "option" ["identifier_list"], ")" ])
      (\rhs -> do let d = getDecl (get rhs 1)
                  let od = other_declarator d
                  return (DeclAST od)),


    -- pointer:
    -- | "*" type_qualifier_list? pointer?
    --     {}

    ruleWithNoAction $ toProdRule
      [ "pointer",
        "*", nonTerminal "option" ["type_qualifier_list"], nonTerminal "option" ["pointer"] ],


    -- type_qualifier_list:
    -- | type_qualifier_list? type_qualifier
    --     {}

    ruleWithNoAction $ toProdRule
      [ "type_qualifier_list",
        nonTerminal "option" ["type_qualifier_list"], " type_qualifier" ],


    -- parameter_type_list:
    -- | parameter_list option("," "..." {}) ctx = save_context
    --     { ctx }

    rule (toProdRule
      [ "parameter_type_list",
        "parameter_list", nonTerminal "option" ["comma", "dotdotdot"], "save_context" ])
      (\rhs -> return (get rhs 3)), 


    -- parameter_list:
    -- | parameter_declaration
    -- | parameter_list "," parameter_declaration
    --     {}

    ruleWithNoAction "parameter_list -> parameter_declaration",
    ruleWithNoAction "parameter_list -> parameter_list , parameter_declaration",


    -- parameter_declaration:
    -- | declaration_specifiers declarator_varname
    -- | declaration_specifiers abstract_declarator?
    --     {}

    ruleWithNoAction "parameter_declaration -> declaration_specifiers declarator_varname",

    ruleWithNoAction $ toProdRule
      [ "parameter_declaration",
        "declaration_specifiers", nonTerminal "option" ["abstract_declarator"] ],


    -- identifier_list:
    -- | var_name
    -- | identifier_list "," var_name
    --     {}

    ruleWithNoAction "identifier_list -> var_name",
    ruleWithNoAction "identifier_list -> identifier_list , var_name",


    -- type_name:
    -- | specifier_qualifier_list abstract_declarator?
    --     {}

    ruleWithNoAction $ toProdRule
      [ "type_name",
        "specifier_qualifier_list ", nonTerminal "option" ["abstract_declarator"] ],


    -- abstract_declarator:
    -- | pointer
    -- | ioption(pointer) direct_abstract_declarator
    --     {}

    ruleWithNoAction "abstract_declarator -> pointer",

    ruleWithNoAction "abstract_declarator -> direct_abstract_declarator",

    ruleWithNoAction "abstract_declarator -> pointer direct_abstract_declarator",


    -- direct_abstract_declarator:
    -- | "(" save_context abstract_declarator ")"
    -- | direct_abstract_declarator? "[" ioption(type_qualifier_list) assignment_expression? "]"
    -- | direct_abstract_declarator? "[" "static" type_qualifier_list? assignment_expression "]"
    -- | direct_abstract_declarator? "[" type_qualifier_list "static" assignment_expression "]"
    -- | direct_abstract_declarator? "[" "*" "]"
    -- | ioption(direct_abstract_declarator) "(" scoped(parameter_type_list)? ")"
    --     {}

    --
    ruleWithNoAction "direct_abstract_declarator -> ( save_context abstract_declarator )",

    -- 
    ruleWithNoAction $ toProdRule
      [ "direct_abstract_declarator",
        nonTerminal "option" ["direct_abstract_declarator"],
        "[", nonTerminal "option" ["assignment_expression"], "]" ],

    ruleWithNoAction $ toProdRule
      [ "direct_abstract_declarator",
        nonTerminal "option" ["direct_abstract_declarator"],
        "[", "type_qualifier_list", nonTerminal "option" ["assignment_expression"], "]" ],

    --
    ruleWithNoAction $ toProdRule
      [ "direct_abstract_declarator",
        nonTerminal "option" ["direct_abstract_declarator"],
        "[", "static",
        nonTerminal "option" ["type_qualifier_list"], "assignment_expression", "]" ],

    --
    ruleWithNoAction $ toProdRule
      [ "direct_abstract_declarator",
        nonTerminal "option" ["direct_abstract_declarator"],
        "[", "type_qualifier_list", "static", "assignment_expression", "]" ],

    --
    ruleWithNoAction $ toProdRule
      [ "direct_abstract_declarator",
        nonTerminal "option" ["direct_abstract_declarator"], "[", "*", "]" ],

    --
    ruleWithNoAction $ toProdRule
      [ "direct_abstract_declarator",
        "(", nonTerminal "option" [nonTerminal "scoped" ["parameter_type_list"]], ")" ],

    --
    ruleWithNoAction $ toProdRule
      [ "direct_abstract_declarator",
        "direct_abstract_declarator", "(", nonTerminal "option" [ nonTerminal "scoped" ["parameter_type_list"]], ")" ],


    -- c_initializer:
    -- | assignment_expression
    -- | "{" initializer_list ","? "}"
    --     {}

    ruleWithNoAction "c_initializer -> assignment_expression",

    ruleWithNoAction $ toProdRule
      [ "c_initializer",
        "{", "initializer_list", nonTerminal "option" ["comma"], "}" ],


    -- initializer_list:
    -- | designation? c_initializer
    -- | initializer_list "," designation? c_initializer
    --     {}

    ruleWithNoAction $ toProdRule
      [ "initializer_list",
        nonTerminal "option" ["designation"], " c_initializer"],


    ruleWithNoAction $ toProdRule
      [ "initializer_list",
        "initializer_list", ",", nonTerminal "option" ["designation"],  "c_initializer" ],


    -- designation:
    -- | designator_list "="
    --     {}

    ruleWithNoAction "designation -> designator_list =",


    -- designator_list:
    -- | designator_list? designator
    --     {}

    ruleWithNoAction $ toProdRule
      [ "designator_list",
        nonTerminal "option" ["designator_list"], " designator" ],


    -- designator:
    -- | "[" constant_expression "]"
    -- | "." general_identifier
    --     {}

    ruleWithNoAction "designator -> [ constant_expression ]",
    ruleWithNoAction "designator -> . general_identifier",


    -- static_assert_declaration:
    -- | "_Static_assert" "(" constant_expression "," STRING_LITERAL ")" ";"
    --     {}

    ruleWithNoAction "static_assert_declaration -> _Static_assert ( constant_expression , STRING_LITERAL ) ;",


    -- statement:
    -- | labeled_statement
    -- | scoped(compound_statement)
    -- | expression_statement
    -- | scoped(selection_statement)
    -- | scoped(iteration_statement)
    -- | jump_statement
    --     {}

    ruleWithNoAction "statement -> labeled_statement",

    ruleWithNoAction $ toProdRule
      [ "statement", nonTerminal "scoped" ["compound_statement"] ],

    ruleWithNoAction "statement -> expression_statement",

    ruleWithNoAction $ toProdRule
      [ "statement", nonTerminal "scoped" ["selection_statement"] ],

    ruleWithNoAction $ toProdRule
      [ "statement", nonTerminal "scoped" ["iteration_statement"] ],

    ruleWithNoAction "statement -> jump_statement",


    -- labeled_statement:
    -- | general_identifier ":" statement
    -- | "case" constant_expression ":" statement
    -- | "default" ":" statement
    --     {}

    ruleWithNoAction "labeled_statement -> general_identifier : statement",
    ruleWithNoAction "labeled_statement -> case constant_expression : statement",
    ruleWithNoAction "labeled_statement -> default : statement",


    -- compound_statement:
    -- | "{" block_item_list? "}"
    --     {}

    ruleWithNoAction $ toProdRule
      [ "compound_statement", "{", nonTerminal "option" ["block_item_list"], "}" ],

    -- block_item_list:
    -- | block_item_list? block_item
    --     {}

    ruleWithNoAction $ toProdRule
      [ "block_item_list", nonTerminal "option" ["block_item_list"], "block_item" ],


    -- block_item:
    -- | declaration
    -- | statement
    --     {}

    ruleWithNoAction "block_item -> declaration",
    ruleWithNoAction "block_item -> statement",


    -- expression_statement:
    -- | expression? ";"
    --     {}

    ruleWithNoAction $ toProdRule
      [ "expression_statement", nonTerminal "option" ["expression"], ";" ],


    -- selection_statement:
    -- | "if" "(" expression ")" scoped(statement) "else" scoped(statement)
    -- | "if" "(" expression ")" scoped(statement) %prec below_ELSE
    -- | "switch" "(" expression ")" scoped(statement)
    --     {}

    ruleWithNoAction $ toProdRule
      [ "selection_statement", "if", "(", "expression", ")",
        nonTerminal "scoped" ["statement"],  "else", nonTerminal "scoped" ["statement"] ],

    ruleNoActionWithPrec
      (toProdRule
        ["selection_statement",
         "if", "(", "expression", ")", nonTerminal "scoped" ["statement"]])
      "below_ELSE",


    ruleWithNoAction $ toProdRule
      [ "selection_statement",
        "switch", "(", "expression", ")", nonTerminal "scoped" ["statement"] ],


    -- iteration_statement:
    -- | "while" "(" expression ")" scoped(statement)
    -- | "do" scoped(statement) "while" "(" expression ")" ";"
    -- | "for" "(" expression? ";" expression? ";" expression? ")" scoped(statement)
    -- | "for" "(" declaration expression? ";" expression? ")" scoped(statement)
    --     {}

    ruleWithNoAction $ toProdRule
      [ "iteration_statement",
        "while", "(", "expression", ")", nonTerminal "scoped" ["statement"] ],


    ruleWithNoAction $ toProdRule
      [ "iteration_statement",
        "do", nonTerminal "scoped" ["statement"], "while", "(", "expression", ")", ";" ],


    ruleWithNoAction $ toProdRule
      [ "iteration_statement",
        "for", "(",
        nonTerminal "option" ["expression"], ";",
        nonTerminal "option" ["expression"], ";",
        nonTerminal "option" ["expression"], ")",
        nonTerminal "scoped" ["statement"] ],


    ruleWithNoAction $ toProdRule
      [ "iteration_statement",
        "for", "(", "declaration",
        nonTerminal "option" ["expression"], ";",
        nonTerminal "option" ["expression"], ")",
        nonTerminal "scoped" ["statement"] ],


    -- jump_statement:
    -- | "goto" general_identifier ";"
    -- | "continue" ";"
    -- | "break" ";"
    -- | "return" expression? ";"
    --     {}

    ruleWithNoAction "jump_statement -> goto general_identifier ;",
    ruleWithNoAction "jump_statement -> continue ;",
    ruleWithNoAction "jump_statement -> break ;",
    ruleWithNoAction $ toProdRule
      [ "jump_statement",
        "return ", nonTerminal "option" ["expression"], ";" ],


    -- translation_unit_file:
    -- | external_declaration translation_unit_file
    -- | external_declaration EOF
    --     {}

    ruleWithNoAction "translation_unit_file -> external_declaration translation_unit_file",
    ruleWithNoAction "translation_unit_file -> external_declaration EOF", -- Todo: EOF???


    -- external_declaration:
    -- | function_definition
    -- | declaration
    --     {}

    ruleWithNoAction "external_declaration -> function_definition",
    ruleWithNoAction "external_declaration -> declaration",


    -- function_definition1:
    -- | declaration_specifiers d = declarator_varname
    --     { let ctx = save_context () in
    --       reinstall_function_context d;
    --       ctx }

    rule "function_definition1 -> declaration_specifiers declarator_varname"
      (\rhs -> do ctx   <- save_context ()
                  let d =  getDecl (get rhs 2)
                  reinstall_function_context d
                  return (CtxAST ctx)),


    -- function_definition:
    -- | ctx = function_definition1 declaration_list? compound_statement
    --     { restore_context ctx }

    rule (toProdRule
      [ "function_definition",
        "function_definition1", nonTerminal "option" ["declaration_list"], " compound_statement" ])
      (\rhs -> do let ctx = getCtx (get rhs 1)
                  restore_context ctx
                  return NoAST),


    -- declaration_list:
    -- | declaration
    -- | declaration_list declaration
    --     {}

    ruleWithNoAction "declaration_list -> declaration",
    ruleWithNoAction "declaration_list -> declaration_list declaration"
  ] ++


  -- -- Standard library

  -- (-)?      
  option "argument_expression_list" ++
  -- option "comma"                    ++    -- ","?
  [ ("option_comma -> " , noAction, Nothing)
  , ("option_comma -> ,", noAction, Nothing)
  ] ++

  -- option "comma_dotdotdot"          ++    -- ", ..."?
  [ ("option_comma_dotdotdot -> "     , noAction, Nothing)
  , ("option_comma_dotdotdot -> , ...", noAction, Nothing)
  ] ++

  option (nonTerminal "init_declarator_list" [ "declarator_varname" ]) ++
  option (nonTerminal "init_declarator_list" [ "declarator_typedefname" ]) ++
  option "general_identifier" ++
  option "struct_declarator_list" ++
  option "declarator" ++
  option "general_identifier" ++
  option "type_qualifier_list" ++
  option "assignment_expression" ++
  option "type_qualifier_list" ++
  option "identifier_list" ++
  option "pointer" ++
  option "abstract_declarator" ++
  option "direct_abstract_declarator" ++
  option "assignment_expression" ++
  option (nonTerminal "scoped" [ "parameter_type_list" ]) ++
  option "designation" ++
  option "designator_list" ++
  option "block_item_list" ++
  option "expression" ++
  option "declaration_list" ++ 

  map ruleWithNoAction (
    -- list_eq1
    list_eq1 "type_specifier_unique" "declaration_specifier" ++
    list "declaration_specifier" ++
    
    list_eq1 "type_specifier_unique" "type_qualifier" ++
    list "type_qualifier" ++
    
    list_eq1 "type_specifier_unique" "alignment_specifier" ++
    list "alignment_specifier" ++

    -- list_ge1
    list_ge1 "type_specifier_nonunique" "declaration_specifier" ++
    list "declaration_specifier" ++
    
    list_ge1 "type_specifier_nonunique" "type_qualifier" ++
    list "type_qualifier" ++
    
    list_ge1 "type_specifier_nonunique" "alignment_specifier" ++
    list "alignment_specifier" ++

    -- list_eq1_eq1
    list_eq1_eq1 "typedef" "type_specifier_unique" "declaration_specifier" ++
    list_eq1 "type_specifier_unique" "declaration_specifier" ++
    list_eq1 "typedef" "declaration_specifier" ++

    -- list_eq1_ge1
    list_eq1_ge1 "typedef" "type_specifier_nonunique" "declaration_specifier" ++
    list_ge1 "type_specifier_nonunique" "declaration_specifier" ++
    list_eq1 "typedef" "declaration_specifier"
    
  )
