module Token(Token(..)) where

import Prelude hiding(EQ, LT, GT)
import TokenInterface

data Token =
    EOF
  | NAME
  | VARIABLE
  | TYPE
  | CONSTANT
  | STRING_LITERAL
  | ALIGNAS
  | ALIGNOF
  | ATOMIC
  | AUTO
  | BOOL
  | BREAK
  | CASE
  | CHAR
  | COMPLEX
  | CONST
  | CONTINUE
  | DEFAULT
  | DO
  | DOUBLE
  | ELSE
  | ENUM
  | EXTERN
  | FLOAT
  | FOR
  | GENERIC
  | GOTO
  | IF
  | IMAGINARY
  | INLINE
  | INT
  | LONG
  | NORETURN
  | REGISTER
  | RESTRICT
  | RETURN
  | SHORT
  | SIGNED
  | SIZEOF
  | STATIC
  | STATIC_ASSERT
  | STRUCT
  | SWITCH
  | THREAD_LOCAL
  | TYPEDEF
  | UNION
  | UNSIGNED
  | VOID
  | VOLATILE
  | WHILE

  | ELLIPSIS
  | ADD_ASSIGN
  | SUB_ASSIGN
  | MUL_ASSIGN
  | DIV_ASSIGN
  | MOD_ASSIGN
  | OR_ASSIGN
  | AND_ASSIGN
  | XOR_ASSIGN
  | LEFT_ASSIGN
  | RIGHT_ASSIGN
  | LEFT
  | RIGHT
  | EQEQ
  | NEQ
  | LEQ
  | GEQ
  | EQ
  | LT
  | GT
  | INC
  | DEC
  | PTR
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | PERCENT
  | BANG
  | ANDAND
  | BARBAR
  | AND
  | BAR
  | HAT
  | QUESTION
  | COLON
  | TILDE
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | LPAREN
  | RPAREN
  | SEMICOLON
  | COMMA
  | DOT
  deriving (Eq, Show)

tokenStrList :: [(Token,String)]
tokenStrList =
  [ (EOF, "$"),
    
    (NAME, "NAME"),
    (VARIABLE, "VARIABLE"), (TYPE, "TYPE"),
    (CONSTANT, "CONSTANT"), (STRING_LITERAL, "STRING_LITERAL"),

    (ALIGNAS, "_Alignas"), (ALIGNOF, "_Alignof"),
    (ATOMIC, "_Atomic"),
    (AUTO, "auto"),
    (BOOL, "bool"),
    (BREAK, "break"),
    (CASE, "case"),
    (CHAR, "char"),
    (COMPLEX, "_Complex"),
    (CONST, "const"),
    (CONTINUE, "continue"),
    (DEFAULT, "default"),
    (DO, "do"),
    (DOUBLE, "double"),
    (ELSE, "else"),
    (ENUM, "enum"),
    (EXTERN, "extern"),
    (FLOAT, "float"),
    (FOR, "for"),
    (GENERIC, "_Generic"),
    (GOTO, "goto"),
    (IF, "if"),
    (IMAGINARY, "_Imaginary"),
    (INLINE, "inline"),
    (INT, "int"),
    (LONG, "long"),
    (NORETURN, "_Noreturn"),
    (REGISTER, "register"),
    (RESTRICT, "restrict"),
    (RETURN, "return"),
    (SHORT, "short"),
    (SIGNED, "signed"),
    (SIZEOF, "sizeof"),
    (STATIC, "static"),
    (STATIC_ASSERT, "_Static_assert"),
    (STRUCT, "struct"),
    (SWITCH, "switch"),
    (THREAD_LOCAL, "_Thread_local"),
    (TYPEDEF, "typedef"),
    (UNION, "union"),
    (UNSIGNED, "unsigned"),
    (VOID, "void"),
    (VOLATILE, "volatile"),
    (WHILE, "while"),

    (ELLIPSIS, "..."),
    (ADD_ASSIGN, "+="),
    (SUB_ASSIGN, "-="),
    (MUL_ASSIGN, "*="),
    (DIV_ASSIGN, "/="),
    (MOD_ASSIGN, "%="),
    (OR_ASSIGN, "|="),
    (AND_ASSIGN, "&="),
    (XOR_ASSIGN, "^="),
    (LEFT_ASSIGN, "<<="),
    (RIGHT_ASSIGN, ">>="),
    (LEFT, "<<"),
    (RIGHT, ">>"),
    (EQEQ, "=="),
    (NEQ, "!="),
    (LEQ, "<="),
    (GEQ, ">="),
    (EQ, "="),
    (LT, "<"),
    (GT, ">"),
    (INC, "++"),
    (DEC, "--"),
    (PTR, "->"),
    (PLUS, "+"),
    (MINUS, "-"),
    (STAR, "*"),
    (SLASH, "/"),
    (PERCENT, "%"),
    (BANG, "!"),
    (ANDAND, "&&"),
    (BARBAR, "||"),
    (AND, "&"),
    (BAR, "|"),
    (HAT, "^"),
    (QUESTION, "?"),
    (COLON, ":"),
    (TILDE, "~"),
    (LBRACE, "{"),
    (RBRACE, "}"),
    (LBRACK, "["),
    (RBRACK, "]"),
    (LPAREN, "("),
    (RPAREN, ")"),
    (SEMICOLON, ";"),
    (COMMA, ","),
    (DOT, ".")
  ]

findTok tok [] = Nothing
findTok tok ((tok_,str):list)
  | tok == tok_ = Just str
  | otherwise   = findTok tok list

findStr str [] = Nothing
findStr str ((tok,str_):list)
  | str == str_ = Just tok
  | otherwise   = findStr str list

instance TokenInterface Token where
  fromToken tok =
    case findTok tok tokenStrList of
      Nothing  -> error ("fromToken: " ++ show tok)
      Just str -> str
  

