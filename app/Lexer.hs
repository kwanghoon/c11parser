module Lexer where -- (c_lexer, init_c_lexer_state, lexerSpec, Lexer_State)

import Prelude hiding (EQ,GT,LT)
import CommonParserUtil
import Token
import Terminal
import Context

import Text.Regex.TDFA
import qualified Control.Monad.Trans.State.Lazy as ST
import Control.Monad.Trans.Class
import qualified Data.Map as Map

-- import Debug.Trace (trace)

mkFn :: Token -> LexAction Token IO LPS -- (String -> Maybe Token)
mkFn tok = \text -> return $ Just tok

skip :: LexAction Token IO LPS          -- String -> Maybe Token
skip = \text -> return Nothing

printAndSkip = \text -> do {- lift (putStrLn text); -} return Nothing

multiLineCommentBegin :: LexAction Token IO LPS          -- String -> Maybe Token
multiLineCommentBegin = \text0 -> -- /*
  --trace ("multiLineCommentBegin" ++ text0) $
    do  (state_parm_, line, col, text) <- ST.get
        let (newLine, newCol, newText) = mlc (tail (tail text)) line (col+2)
        -- lift $ putStrLn text0
        -- lift $ putStrLn (show line ++ ", " ++ show col ++ ", " ++ text)
        -- lift $ putStrLn (show newLine ++ ", " ++ show newCol ++ ", " ++ newText)
        ST.put (state_parm_, newLine, newCol, newText)
        return Nothing

  where
    mlc [] line col = (line, col, [])
    mlc ('*':'/':text) line col = (line, col+2, text)
    mlc ('\n':text) line col = mlc text (line+1) col
    mlc ('\r':text) line col = mlc text (line+1) col
    mlc (_:text) line col = mlc text line (col+1)

init_string_literal :: LexAction Token IO LPS          -- String -> Maybe Token
init_string_literal = \text0 -> -- /*
  do  (state_parm_, line, col, text) <- ST.get
      (newLine, newCol, newText, newStr) <- isl (tail text) line (col+1) ""
      ST.put (state_parm_, newLine, newCol, newText)
      -- lift (putStrLn ("init_string_literal: " ++ newStr) ) 
      (mkFn STRING_LITERAL ("\"" ++ newStr ++ "\""))

  where
    isl [] line col accu = return (line, col, [], reverse accu)
    isl ('\n':text) line col accu = return (line+1, col, text, reverse accu)
    isl ('\r':text) line col accu = return (line+1, col, text, reverse accu)
    isl ('\"':text) line col accu = return (line, col+1, text, reverse accu)
    isl ('\\':ch:text) line col accu = isl text line (col+2) (ch : '\\' : accu)
    isl (ch:text) line col accu = isl text line (col+1) (ch : accu)

-- | Utilities
infixl 4 <|>

x <|> y = paren (x ++ "|" ++ y)     -- "(" ++ x ++ "|" ++ y ++ ")"

zeroOrMore x = paren x ++ "*"     -- "(" ++ x ++ ")*"

oneOrMore x  = paren x ++ "+"     -- "(" ++ x ++ ")+"

opt x = paren x ++ "?"            -- "(" ++ x ++ ")?"

paren x = "(" ++ x ++ ")"

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
  identifier_nondigit
    ++ zeroOrMore ( identifier_nondigit  <|>  digit )

-- | Whitespaces

whitespace_char_no_newline = "[ \t\n\r]" -- "[ \t\012\r]"

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
       opt digit_sequence ++ "\\." ++ digit_sequence
  <|>  digit_sequence ++ "\\."
  
exponent_part = "[eE]" ++ opt sign ++ digit_sequence

decimal_floating_constant =
       fractional_constant ++ opt exponent_part ++ opt floating_suffix
  <|>  digit_sequence ++ exponent_part ++ opt floating_suffix

hexadecimal_digit_sequence = oneOrMore hexadecimal_digit

hexadecimal_fractional_constant =
       opt hexadecimal_digit_sequence ++ "\\." ++ hexadecimal_digit_sequence
  <|>  hexadecimal_digit_sequence ++ "\\."

binary_exponent_part = "[pP]" ++ opt sign ++ digit_sequence

hexadecimal_floating_constant =
       hexadecimal_prefix ++ hexadecimal_fractional_constant ++
         binary_exponent_part ++ opt floating_suffix
  <|>  hexadecimal_prefix ++ hexadecimal_digit_sequence ++
         binary_exponent_part ++ opt floating_suffix

-- | Preprocessing numbersa

preprocessing_number = opt "\\." ++ "[0-9]" ++
  zeroOrMore ("[0-9A-Za-z_\\.]"  <|>  "[eEpP][+-]")

-- | Character and string constants
simple_escape_sequence =
--  "\\" ++ "[\\'\\\"?\\\\abfnrtv]"
  "\\\\" ++ "[\\\'\\\"?abfnrtv]"

octal_escape_sequence =
  "\\\\(" ++ octal_digit ++
        "|" ++ octal_digit ++ octal_digit ++
        "|" ++  octal_digit ++ octal_digit ++ octal_digit ++ ")"

hexadecimal_escape_sequence =
  "\\\\x" ++ oneOrMore hexadecimal_digit

escape_sequence =
       simple_escape_sequence 
  <|>  octal_escape_sequence
  <|>  hexadecimal_escape_sequence
  <|>  universal_character_name

char_or_escape_sequence =
       escape_sequence
  <|>  "[^\\\']"

-- | Lexical analysis specification
lexerSpec :: LexerSpec Token IO LPS
lexerSpec = LexerSpec
  {
    endOfToken    = EOF,
    lexerSpecList =
      [
        ("#" ++ zeroOrMore "[^\n]" ++ "[\n]", printAndSkip), -- ignore preprocessed lines Todo: Make it work!

        (oneOrMore whitespace_char_no_newline, skip),  -- with newline!

        ("/\\*", multiLineCommentBegin),

        ("//" ++ zeroOrMore "[^\n]" ++ "[\n]", skip ), -- rewritten // as this

        -- -- A hack!
        -- ("\\(", mkFn LPAREN ),
        -- (":", mkFn COLON ),
        -- ("\\+", mkFn PLUS ),
        
        (decimal_floating_constant, mkFn CONSTANT),
        (integer_constant, mkFn CONSTANT),
        (hexadecimal_floating_constant, mkFn CONSTANT),
        
        -- -- (preprocessing_number,
        -- --  error "These characters form a preprocessor number, but not a constant"),
        
        -- -- (("[LuU]" <|> "") ++ "'", mkFn CONSTANT ),
        -- (opt "[LuU]" ++ "'[^']*'", mkFn CONSTANT ),   -- rewritten as "\"[^\"]*\""  Todo: \'
        
        -- -- (("[LuU]" <|> "" <|> "u8") ++ "\"",  mkFn STRING_LITERAL),  
        -- (opt ("[LuU]" <|> "u8") ++ "\"[^\"]*\"",  mkFn STRING_LITERAL),  -- rewritten as "\"[^\"]*\""  Todo: \"

        -- (("[LuU]" <|> "") ++ "'", mkFn CONSTANT ),
        (opt "[LuU]" ++ "\'" ++ char_or_escape_sequence ++ "\'", mkFn CONSTANT ),   -- rewritten as "\"[^\"]*\""  Todo: \'
        
        -- (("[LuU]" <|> "" <|> "u8") ++ "\"",  mkFn STRING_LITERAL),  
        -- (opt ("[LuU]" <|> "u8") ++ "\"[^(\\\")]*\"",  mkFn STRING_LITERAL),  -- rewritten as "\"[^\"]*\""  Todo: \"
        -- (opt ("[LuU]" <|> "u8") ++ "\"(?:[^\"\\]|\\.)*\"",  mkFn STRING_LITERAL),  -- rewritten as "\"[^\"]*\""  Todo: \"
        -- (opt ("[LuU]" <|> "u8") ++ "\"(\\.|[^\"])*\"",  mkFn STRING_LITERAL),  -- rewritten as "\"[^\"]*\""  Todo: \"
        (opt ("[LuU]" <|> "u8") ++ "\"",  init_string_literal),  -- rewritten as "\"[^\"]*\""  Todo: \"
	
        ("\\.\\.\\.", mkFn ELLIPSIS ),
        ("\\+=", mkFn ADD_ASSIGN ),
        ("-=", mkFn SUB_ASSIGN ),
        ("\\*=", mkFn MUL_ASSIGN ),
        ("/=", mkFn DIV_ASSIGN ),
        ("%=", mkFn MOD_ASSIGN ),
        ("\\|=", mkFn OR_ASSIGN ),
        ("&=", mkFn AND_ASSIGN ),
        ("\\^=", mkFn XOR_ASSIGN ),
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
--         ("_Alignas", mkFn ALIGNAS ),
--         ("_Alignof", mkFn ALIGNOF ),
--         ("_Atomic", mkFn ATOMIC ),
--         ("_Bool", mkFn BOOL ),
--         ("_Complex", mkFn COMPLEX ),
--         ("_Generic", mkFn GENERIC ),
--         ("_Imaginary", mkFn IMAGINARY ),
--         ("_Noreturn", mkFn NORETURN ),
--         ("_Static_assert", mkFn STATIC_ASSERT ),
--         ("_Thread_local", mkFn THREAD_LOCAL ),
--         ("auto", mkFn AUTO ),
--         ("break", mkFn BREAK ),
--         ("case", mkFn CASE ),
--         ("char", mkFn CHAR ),
--         ("const", mkFn CONST ),
--         ("continue", mkFn CONTINUE ),
--         ("default", mkFn DEFAULT ),
-- --        ("do", mkFn DO ),
--         ("double", mkFn DOUBLE ),
--         ("do", mkFn DO ),        
--         ("else", mkFn ELSE ),
--         ("enum", mkFn ENUM ),
--         ("extern", mkFn EXTERN ),
--         ("float", mkFn FLOAT ),
--         ("for", mkFn FOR ),
--         ("goto", mkFn GOTO ),
--         ("if", mkFn IF ),
--         ("inline", mkFn INLINE ),
--         ("int", mkFn INT ),
--         ("long", mkFn LONG ),
--         ("register", mkFn REGISTER ),
--         ("restrict", mkFn RESTRICT ),
--         ("return", mkFn RETURN ),
--         ("short", mkFn SHORT ),
--         ("signed", mkFn SIGNED ),
--         ("sizeof", mkFn SIZEOF ),
--         ("static", mkFn STATIC ),
--         ("struct", mkFn STRUCT ),
--         ("switch", mkFn SWITCH ),
--         ("typedef", mkFn TYPEDEF ),
--         ("union", mkFn UNION ),
--         ("unsigned", mkFn UNSIGNED),
--         ("void", mkFn VOID),
--         ("volatile", mkFn VOLATILE),
--         ("while", mkFn WHILE),

        -- (preprocessing_number,
        --  error "These characters form a preprocessor number, but not a constant"),
	
        (identifier, keywordOrIdentifier)
      ]

  }

keywordOrIdentifier text =
  -- trace ("keywordOrIdentifier : " ++ text) $
    case Map.lookup text keywords of
      Nothing  -> mkFn NAME text
      Just tok -> mkFn tok text

keywords :: Map.Map String Token
keywords =
  Map.fromList
    [ ("_Alignas", ALIGNAS ),
      ("_Alignof", ALIGNOF ),
      ("_Atomic", ATOMIC ),
      ("_Bool", BOOL ),
      ("_Complex", COMPLEX ),
      ("_Generic", GENERIC ),
      ("_Imaginary", IMAGINARY ),
      ("_Noreturn", NORETURN ),
      ("_Static_assert", STATIC_ASSERT ),
      ("_Thread_local", THREAD_LOCAL ),
      ("auto", AUTO ),
      ("break", BREAK ),
      ("case", CASE ),
      ("char", CHAR ),
      ("const", CONST ),
      ("continue", CONTINUE ),
      ("default", DEFAULT ),
      ("do", DO ),
      ("double", DOUBLE ),
      ("else", ELSE ),
      ("enum", ENUM ),
      ("extern", EXTERN ),
      ("float", FLOAT ),
      ("for", FOR ),
      ("goto", GOTO ),
      ("if", IF ),
      ("inline", INLINE ),
      ("int", INT ),
      ("long", LONG ),
      ("register", REGISTER ),
      ("restrict", RESTRICT ),
      ("return", RETURN ),
      ("short", SHORT ),
      ("signed", SIGNED ),
      ("sizeof", SIZEOF ),
      ("static", STATIC ),
      ("struct", STRUCT ),
      ("switch", SWITCH ),
      ("typedef", TYPEDEF ),
      ("union", UNION ),
      ("unsigned", UNSIGNED),
      ("void", VOID),
      ("volatile", VOLATILE),
      ("while", WHILE),
      ("__builtin_va_arg", BUILTIN_VA_ARG)
    ]


-- | An enriched lexer

init_c_lexer_state = SRegular

c_lexer flag =
  do terminal <- c_lexer' flag
     -- lift (putStrLn (terminalToString terminal))
     return terminal
     
c_lexer' flag =
  do (lps,line,col,text) <- ST.get
     let st = lexer_state lps

     case st of
       SIdent id -> 
         do ST.put (lps{lexer_state=SRegular},line,col,text)
            
            b <- is_typedefname id

            if b
              then return $ Terminal (fromToken TYPE) line col (Just TYPE)
              else return $ Terminal (fromToken VARIABLE) line col (Just VARIABLE)

       _ -> -- SAtomic or SRegular   Todo: Not support SAtomic
         do terminal <- aLexer lexerSpec flag

            (lps_,line_,col_,text_) <- ST.get
            
            let maybeToken = terminalToMaybeToken terminal
            let id         = terminalToSymbol     terminal
            
            case (st, maybeToken) of
              (_, Just NAME) ->
                do ST.put (lps_{lexer_state=SIdent id},line_,col_,text_)
                   return terminal

              (_, _) ->
                do ST.put (lps_{lexer_state=SRegular},line_,col_,text_)
                   return terminal

     
-- c_lexer_with (SAtomic)   = x
-- c_lexer_with (SRegular)  = x

--   do terminal <- aLexer lexerSpec
     

