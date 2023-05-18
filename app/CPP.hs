module CPP (removeAttributes) where

import Text.Regex.TDFA

cpp :: IO ()
cpp = do
  let input = "This is a __attribute__((some_attribute)) example __attribute__((with_nested(parentheses))) text."
  let output = removeAttributes input
  putStrLn output
  -- Output: "This is a  example  text."

removeAttributes :: String -> String
removeAttributes [] = []
removeAttributes input =
  let input' = firstMatch (getMatches input) input
  in  head input' : removeAttributes (tail input')

getMatches:: String -> [(String,String->String)]
getMatches input =
  [ ((input =~ ("\\`" ++ pat)) :: String, act :: String ->String)
  | (pat,act) <- patternActions ]

firstMatch [] input = input
firstMatch (("",_):matches)  input = firstMatch matches input
firstMatch ((matched,act):_) input = act (drop (length matched) input) 


-- Patterns for GCC extensions
patternActions = 
  [ (pat_attribute, removeMatchedDblParentheses 0)
  , (pat_typeof, \x -> " int " ++ removeMatchedDblParentheses 0 x )
  , (pat_restrict__,\x->x)
  , (pat_restrict,\x->x)
  , (pat_asm__, removeMatchedDblParentheses 0)
  , (pat_asm, removeMatchedDblParentheses 0)
  , (pat_extension,\x->x)
  , (pat_signed,\x->x)
  , (pat_inline,\x->x)
  , (pat_inline__,\x->x)
  ]

pat_attribute = "__attribute__" :: String
pat_typeof = "__typeof" :: String
pat_restrict__ = "__restrict__" :: String
pat_restrict = "__restrict" :: String
pat_asm__ = "__asm__" :: String
pat_asm = "__asm" :: String
pat_extension = "__extension__" :: String
pat_signed = "__signed__" :: String
pat_inline = "__inline" :: String
pat_inline__ = "__inline__" :: String


removeMatchedDblParentheses :: Int -> String -> String
removeMatchedDblParentheses n ( '(':text ) =
  removeMatchedDblParentheses (n+1) text
  
removeMatchedDblParentheses n ( ')':text ) =
  if n-1 == 0 then text
  else removeMatchedDblParentheses (n-1) text

removeMatchedDblParentheses n ( c:text ) =
  removeMatchedDblParentheses n text

removeMatchedDblParentheses n text =
  error $ "Attributes error: not matching parenthesis: " ++ text