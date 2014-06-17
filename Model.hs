
{-#LANGUAGE LambdaCase, OverloadedStrings, TypeFamilies #-}

module Source.TypeNormalizer.Model where


import Data.Hashable
import GHC.Generics 
import Data.Map.Strict (Map,(!),lookup,fromList,member,fromListWith,empty)
import Data.List(sort,nub) -- nub, take that shit out of here!
import Data.Hashable
import Control.Applicative


data ContextType = ContextType Constraint Type 
                 deriving (Show,Eq,Ord)

-- A rose tree :
data Type  = Node Label [Type]   
           deriving (Show,Eq,Ord)

data Label = Basic   Base    -- A.k.a.  a -> b , (a,b) or Either a b 
           | Closed  String  -- A.k.a.  Foo a b
           | Open    Int     -- A.k.a.  foo a b  
           deriving (Show,Eq,Ord)

data Base = Exp   -- A.k.a  a -> b 
          | Prod  -- A.k.a  (a,b)
          | Sum   -- A.k.a  Either a b  
          deriving (Show,Eq,Ord,Enum,Bounded)

{- In the special case of Exp notice: 

   Node (Base Exp) [base,arg1,arg2,arg3,arg4...argN ] => equivalent to => Node (Base Exp) [base, Node (Base Prod) [arg1,arg2,arg3,arg4...argN]]

   Node (Base Exp) [base] or Node (Base Exp) []       => ilegal combination 

-}



type Constraint  = Type -- I know it's not buuuut, well, they have they same syntax somehow so we can reuse code easily this way...


