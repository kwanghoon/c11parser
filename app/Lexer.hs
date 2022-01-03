module Lexer(lexerSpec) where

import Prelude hiding (EQ,GT,LT)
import CommonParserUtil
import Token
import Text.Regex.TDFA

mkFn :: Token -> (String -> Maybe Token)
mkFn tok = \text -> Just tok

skip :: String -> Maybe Token
skip = \text -> Nothing


-- | Utilities
infixl 4 <|>

x <|> y = "(" ++ x ++ "|" ++ y ++ ")"

zeroOrMore x = "(" ++ x ++ ")*"

oneOrMore x  = "(" ++ x ++ ")+"

opt x = "(" ++ x ++ ")?"

-- | Identifiers
digit             = "[0-9]"
hexadecimal_digit = "[0-9A-Fa-f]"
nondigit          = "[_a-zA-Z]"

hex_quad =
  hexadecimal_digit
    ++ hexadecimal_digit
    ++ hexadecimal_digit
    ++ hexadecimal_digit

universal_character_name =
       "\\u" ++ hex_quad
  <|>  "\\U" ++ hex_quad ++ hex_quad

identifier_nondigit =
       nondigit
  <|>  universal_character_name

identifier =
  identifier_nondigit ++ zeroOrMore ( identifier_nondigit  <|>  digit )

-- | Whitespaces

whitespace_char_no_newline = "[ \t\012\r]"

-- | Integer constants

nonzero_digit = "[1-9]"
decimal_constant = nonzero_digit ++ zeroOrMore digit

octal_digit = "[0-7]"
octal_constant = "0" ++ zeroOrMore octal_digit

hexadecimal_prefix = "0x"  <|> "0X"
hexadecimal_constant =
  hexadecimal_prefix ++ oneOrMore hexadecimal_digit

unsigned_suffix = "[uU]"
long_suffix = "[lL]"
long_long_suffix = "ll"  <|> "LL"
integer_suffix =
       unsigned_suffix ++ opt long_suffix
  <|>  unsigned_suffix ++ long_long_suffix
  <|>  long_suffix ++ opt unsigned_suffix
  <|>  long_long_suffix ++ opt unsigned_suffix

integer_constant =
       decimal_constant ++ opt integer_suffix
  <|>  octal_constant ++ opt integer_suffix
  <|>  hexadecimal_constant ++ opt integer_suffix

-- | Floating constants

sign = "[-+]"
digit_sequence = oneOrMore digit
floating_suffix = "[flFL]"

fractional_constant =
       opt digit_sequence ++ "." ++ digit_sequence
  <|>  digit_sequence ++ "."
  
exponent_part = "[eE]" ++ opt sign ++ digit_sequence

decimal_floating_constant =
       fractional_constant ++ opt exponent_part ++ opt floating_suffix
  <|>  digit_sequence ++ exponent_part ++ opt floating_suffix

hexadecimal_digit_sequence = oneOrMore hexadecimal_digit

hexadecimal_fractional_constant =
       opt hexadecimal_digit_sequence ++ "." ++ hexadecimal_digit_sequence
  <|>  hexadecimal_digit_sequence ++ "."

binary_exponent_part = "[pP]" ++ opt sign ++ digit_sequence

hexadecimal_floating_constant =
       hexadecimal_prefix ++ hexadecimal_fractional_constant ++
         binary_exponent_part ++ opt floating_suffix
  <|>  hexadecimal_prefix ++ hexadecimal_digit_sequence ++
         binary_exponent_part ++ opt floating_suffix

-- | Preprocessing numbersa

preprocessing_number = opt "." ++ "[0-9]" ++
  zeroOrMore ("[0-9A-Za-z_.]"  <|>  "[eEpP][+-]")

-- | Character and string constants
simple_escape_sequence =
  "\\" ++ "[\\'\\\"?\\\\abfnrtv]"

octal_escape_sequence =
  "\\" ++ (octal_digit
           <|>  octal_digit ++ octal_digit
           <|>  octal_digit ++ octal_digit ++ octal_digit)

hexadecimal_escape_sequence =
  "\\x" ++ oneOrMore hexadecimal_digit

escape_sequence =
       simple_escape_sequence
  <|>  octal_escape_sequence
  <|>  hexadecimal_escape_sequence
  <|>  universal_character_name


-- | Lexical analysis specification
lexerSpec :: LexerSpec Token
lexerSpec = LexerSpec
  {
    endOfToken    = END_OF_TOKEN,
    lexerSpecList =
      [
        (oneOrMore whitespace_char_no_newline, skip),
        ("\n", skip),
--        ("\\/\\*(\\*(?!\\/)|[^*])*\\*\\/", skip),   -- rewritten /* as \/\*(\*(?!\/)|[^*])*\*\/
        ("//" ++ zeroOrMore "^\\n" ++ "\\n", skip), -- rewritten // as this

        -- ("#" ++ zeroOrMore "^(\\n)" ++ "\\n", skip), -- ignore preprocessed lines Todo: Make it work!

        (integer_constant, mkFn CONSTANT),
        (decimal_floating_constant, mkFn CONSTANT),
        (hexadecimal_floating_constant, mkFn CONSTANT),
        
        -- (preprocessing_number,
        --  error "These characters form a preprocessor number, but not a constant"),
        
        -- (("[LuU]" <|> "") ++ "'", mkFn CONSTANT ),
        (opt "[LuU]" ++ "'[^']*'", mkFn CONSTANT ),   -- rewritten as "\"[^\"]*\""  Todo: \'
        
        -- (("[LuU]" <|> "" <|> "u8") ++ "\"",  mkFn STRING_LITERAL),  
        (opt ("[LuU]" <|> "u8") ++ "\"[^\"]*\"",  mkFn STRING_LITERAL),  -- rewritten as "\"[^\"]*\""  Todo: \"
        
        ("\\.\\.\\.", mkFn ELLIPSIS ),
        ("\\+=", mkFn ADD_ASSIGN ),
        ("-=", mkFn SUB_ASSIGN ),
        ("\\*=", mkFn MUL_ASSIGN ),
        ("/=", mkFn DIV_ASSIGN ),
        ("%=", mkFn MOD_ASSIGN ),
        ("\\|=", mkFn OR_ASSIGN ),
        ("&=", mkFn AND_ASSIGN ),
        ("^=", mkFn XOR_ASSIGN ),
        ("<<=", mkFn LEFT_ASSIGN ),
        (">>=", mkFn RIGHT_ASSIGN ),
        ("<<", mkFn LEFT ),
        (">>", mkFn RIGHT ),
        ("==", mkFn EQEQ ),
        ("!=", mkFn NEQ ),
        ("<=", mkFn LEQ ),
        (">=", mkFn GEQ ),
        ("=", mkFn EQ ),
        ("<", mkFn LT ),
        (">", mkFn GT ),
        ("\\+\\+", mkFn INC ),
        ("--", mkFn DEC ),
        ("->", mkFn PTR ),
        ("\\+", mkFn PLUS ),
        ("-", mkFn MINUS ),
        ("\\*", mkFn STAR ),
        ("/", mkFn SLASH ),
        ("%", mkFn PERCENT ),
        ("!", mkFn BANG ),
        ("&&", mkFn ANDAND ),
        ("\\|\\|", mkFn BARBAR ),
        ("&", mkFn AND ),
        ("\\|", mkFn BAR ),
        ("\\^", mkFn HAT ),
        ("\\?", mkFn QUESTION ),
        (":", mkFn COLON ),
        ("~", mkFn TILDE ),
        ("{" <|> "<%", mkFn LBRACE ),
        ("}" <|> "%>", mkFn RBRACE ),
        ("\\["  <|>  "<:", mkFn LBRACK ),
        ("\\]"  <|>  ":>", mkFn RBRACK ),
        ("\\(", mkFn LPAREN ),
        ("\\)", mkFn RPAREN ),
        (";", mkFn SEMICOLON ),
        (",", mkFn COMMA ),
        ("\\.", mkFn DOT ),
        ("_Alignas", mkFn ALIGNAS ),
        ("_Alignof", mkFn ALIGNOF ),
        ("_Atomic", mkFn ATOMIC ),
        ("_Bool", mkFn BOOL ),
        ("_Complex", mkFn COMPLEX ),
        ("_Generic", mkFn GENERIC ),
        ("_Imaginary", mkFn IMAGINARY ),
        ("_Noreturn", mkFn NORETURN ),
        ("_Static_assert", mkFn STATIC_ASSERT ),
        ("_Thread_local", mkFn THREAD_LOCAL ),
        ("auto", mkFn AUTO ),
        ("break", mkFn BREAK ),
        ("case", mkFn CASE ),
        ("char", mkFn CHAR ),
        ("const", mkFn CONST ),
        ("continue", mkFn CONTINUE ),
        ("default", mkFn DEFAULT ),
        ("do", mkFn DO ),
        ("double", mkFn DOUBLE ),
        ("else", mkFn ELSE ),
        ("enum", mkFn ENUM ),
        ("extern", mkFn EXTERN ),
        ("float", mkFn FLOAT ),
        ("for", mkFn FOR ),
        ("goto", mkFn GOTO ),
        ("if", mkFn IF ),
        ("inline", mkFn INLINE ),
        ("int", mkFn INT ),
        ("long", mkFn LONG ),
        ("register", mkFn REGISTER ),
        ("restrict", mkFn RESTRICT ),
        ("return", mkFn RETURN ),
        ("short", mkFn SHORT ),
        ("signed", mkFn SIGNED ),
        ("sizeof", mkFn SIZEOF ),
        ("static", mkFn STATIC ),
        ("struct", mkFn STRUCT ),
        ("switch", mkFn SWITCH ),
        ("typedef", mkFn TYPEDEF ),
        ("union", mkFn UNION ),
        ("unsigned", mkFn UNSIGNED),
        ("void", mkFn VOID),
        ("volatile", mkFn VOLATILE),
        ("while", mkFn WHILE),
        (identifier, mkFn NAME)
      ]

  }
