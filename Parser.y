
{
{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}

module Source.TypeNormalizer.Parser where


import Data.Char
import Prelude hiding (lookup)
import Data.Map(Map, insert,lookup,member,maxView)
import qualified Data.Map as M
import Control.Applicative hiding (Const)
import Data.List (mapAccumL)
import Control.Arrow
import Data.Maybe


import Source.Lexeme.StateMachine
import Source.TypeNormalizer.Model



}



%name calc
%tokentype { Token Int}
%error { parseError }
%monad { Either ParseError }



%token 
      '('                {Op             }
      ')'                {Cp             }
      '['                {OBrac          }
      ']'                {CBrac          }
      '~'                {TypeEqual      }
      tuple_             {DesugaredTuple }
      app_               {DesugaredApp   }     
      either_            {Either         }
      comma              {Comma          }
      required           {Required       }
      unit_              {Unit           }
      construct_         {Mayus $$       }
      vartype_           {Minus $$       } 
      app                {App            }
%%


ContexLevel  : TopLevel                        { ContexType (Node (Closed "()") []) $1               }
             | TopLevel required TopLevel      { ContexType $1                      $3               }

TopLevel     : ArgLevel                        { $1                                                  }
             | ArgLevel app TopLevel           { Node (Basic Exp)    [$3,$1]                         }
             | ArgLevel '~' ArgLevel           { Node (Closed "~")   [$1,$3]                         }

ArgLevel     : SimpleLevel                     { $1                                                  }
             | construct_  List                { Node  (Closed $1 )  $2                              }
             | vartype_    List                { Node  (Open   $1 )  $2                              }
             | app_        List                { Node  (Basic Exp)   $2                              }
             | tuple_      List                { Node (Closed "(,)") $2                              }              

SimpleLevel  : unit_                           { Node  (Basic  Prod)  []                             }
             | vartype_                        { Node  (Open   $1)    []                             }
             | construct_                      { Node  (Closed $1)    []                             }
             | app_                            { Node  (Closed "(->)")[]                             }

             | tuple_                          { Node  (Closed "(,)") []                             }
             | either_ SimpleLevel SimpleLevel { Node  (Basic  Sum)  [$2,$3]                         }

             | '[' TopLevel ']'                { Node  (Closed "[]") [$2]                            }  
             | '[' TopLevel CommaList ']'      { Node  (Closed "[]") [Node  (Basic Prod)  ($2 : $3)] }
            
             | '(' TopLevel ')'                { $2                                                  }  
             | '(' TopLevel CommaList ')'      { Node  (Basic Prod)  ($2 : $3)                       }



CommaList    : comma TopLevel CommaList        {$2 : $3                                              }
             | comma TopLevel                  {[$2]                                                 }


List         : SimpleLevel List                { $1 : $2                                             }
             | SimpleLevel                     {[$1]                                                 }

{

-- TODO, better error handling....
parseError :: [Token Int] -> Either ParseError a
parseError _ = Left SyntaxticError




data Token a = Op
             | Cp
             | OBrac
             | CBrac
             | App
             | Either
             | Comma
             | Required
             | TypeEqual
             | DesugaredTuple
             | DesugaredApp
             | Unit 
             | Mayus String
             | Minus a
             deriving Show



parse:: String -> Either ParseError ContexType
parse str = calc =<< (transform <$> lexer str)


transform::[Token String] -> [Token Int] 
transform = snd.mapAccumL go  M.empty
     where
       go acc = \case 
                   Minus x 
                     | Just n <- lookup x acc -> (acc, Minus n)
                     | otherwise              -> let n =  maybe (-1) fst (maxView acc) + 1
                                                  in (insert x n acc, Minus n ) 

                   Op               -> (acc , Op      )      
                   Cp               -> (acc , Cp      )
                   OBrac            -> (acc , OBrac   )      
                   CBrac            -> (acc , CBrac   )        
                   App              -> (acc , App     )
                   TypeEqual        -> (acc , TypeEqual      )
                   DesugaredTuple   -> (acc , DesugaredTuple )        
                   DesugaredApp     -> (acc , DesugaredApp   )
                   Either           -> (acc , Either  )                 
                   Comma            -> (acc , Comma   )
                   Required         -> (acc , Required)          
                   Unit             -> (acc , Unit    )          
                   Mayus x          -> (acc , Mayus x )    

data ParseError = LexicalError | SyntaxticError |EmptyContents deriving (Show,Eq)
 
-- TODO: add oneOf into the state machine....
oneOf :: Alternative f => [f a] -> f a
oneOf = foldr (<|>) empty


-- some strange behaviour using lexeme when individual lexeme definition order is changed
-- (Already in the TODO list for improving Source.Lexeme.StateMachine)
lexeme:: StateMachine Char (Token String)
lexeme = oneOf [ Comma            <$  element ','
               , DesugaredApp     <$  string  "(->)"
               , Unit             <$  element '(' <* element ')' 
               , Either           <$  string "Either"
               , App              <$  string "->"
               , Required         <$  string "=>"
               , TypeEqual        <$  element '~'
               , DesugaredTuple   <$  string  "(,)"
               , Op               <$  element '('
               , Cp               <$  element ')'
               , OBrac            <$  element '['
               , CBrac            <$  element ']'
               
               , getMayus         <$> such isUpper 
                                  <*> many (such validChar) 
                                  <*> optional (element '#')
               
               , getMinus         <$> (such isLower         <|> element '_') 
                                  <*> many (such validChar)
               ]
  where
    --getMayus::Char -> String -> Maybe Char -> Token String 
    getMayus c str hash = Mayus $ c : str ++ maybe "" return hash
    getMinus c str = Minus $ c : str 
    validChar c = isAlphaNum c || c == '_' || c == '\'' 


lexer str = let regex = many $  many (such isSpace) *> lexeme <* many (such isSpace)
             in case tokenize regex str of
               Nothing -> Left (LexicalError)
               Just [] -> Left EmptyContents
               Just xs -> Right xs

}










