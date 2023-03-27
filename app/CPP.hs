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
  let matches = [ ((input =~ pat) :: (String,String,String), act :: String ->String)
                | (pat,act) <- patternActions ]
  in firstMatch matches input
    
  where
    patternActions = 
      [ (pat_attribute, removeMatchedDblParentheses 0)
      , (pat_restrict,\x->x) ]
    pat_attribute = "__attribute__" :: String
    pat_restrict = "__restrict" :: String

    firstMatch [] input = head input : removeAttributes (tail input)
    firstMatch ((("",matched,post),act):matches) input =
      removeAttributes (act input)
    firstMatch (((_,matched,post),act):matches) input =
      firstMatch matches input

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