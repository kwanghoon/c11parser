module Context where

import CommonParserUtil

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Control.Monad.Trans.State.Lazy as ST
import Control.Monad.Trans.Class

-- | Lexer and parser states
data LPS = LPS { lexer_state :: Lexer_State, name_set :: Context }

-- The size of the set must not exceed maxBound::Int. Violation of
-- this condition is not detected and if the size limit is exceeded,
-- its behaviour is undefined.

emptyContext = Set.empty

-- (* This declares [id] as a typedef name. *)
-- let declare_typedefname id =
--   current := StringSet.add id !current

declare_typedefname :: String -> ST.StateT (LexerParserState LPS) IO ()
declare_typedefname id =
  do (lps,line,col,text) <- ST.get
     let new_set = Set.insert id (name_set lps)
     ST.put (lps{name_set=new_set},line,col,text)

-- (* This declares [id] as a variable (hence un-declares it as a typedef name). *)
-- let declare_varname id =
--   current := StringSet.remove id !current

declare_varname :: String -> ST.StateT (LexerParserState LPS) IO ()
declare_varname id = 
  do (lps,line,col,text) <- ST.get
     let new_set = Set.delete id (name_set lps)
     ST.put (lps{name_set=new_set},line,col,text)

-- (* This tests whether [id] is known as a typedef name. *)
-- let is_typedefname id =
--   StringSet.mem id !current

is_typedefname :: String -> ST.StateT (LexerParserState LPS) IO Bool
is_typedefname id = 
  do (lps,_,_,_) <- ST.get
     return (Set.member id (name_set lps))

-- (* A context is just a set of identifiers. It is the set of typedef
--    names that are now visible. *)
-- type context =
--   StringSet.t

-- | Parser state

type Context = Set.Set String

-- (* This takes a snapshot of the current context. *)
-- let save_context () =
--   !current

save_context :: () -> ST.StateT (LexerParserState LPS) IO Context
save_context () =
  do (lps,_,_,_) <- ST.get
     return (name_set lps)

-- (* This re-installs a snapshot as the current context. *)
-- let restore_context snapshot =
--   current := snapshot

restore_context :: Set.Set String -> ST.StateT (LexerParserState LPS) IO ()
restore_context snapshot = 
  do (lps,line,col,text) <- ST.get
     ST.put (lps{name_set=snapshot},line,col,text)


-- | Lexer state

data Lexer_State =
    SRegular
  | SAtomic
  | SIdent String
