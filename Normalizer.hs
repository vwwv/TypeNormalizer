
{-#LANGUAGE LambdaCase, OverloadedStrings, TypeFamilies #-}

module Source.TypeNormalizer.Normalizer where




import GHC.Generics 
import Data.Map.Strict (Map,(!),lookup,fromList,member,fromListWith,empty)
import Data.List(sort,nub) -- nub, take that shit out of here!
import Data.Hashable
import Control.Applicative hiding (empty)
import Control.Monad
import qualified Data.Set as S 
import Data.List (group,permutations,groupBy,mapAccumL)

-- import Source.TypeNormalizer.Parser -- for debuging...
import Source.TypeNormalizer.Model








normalize::ContextType -> (Int,ContextType)
normalize (ContextType a b) = let type_               = ContextType (shapeNormalize a) (shapeNormalize b)
                                  (n,permutations) =  usefullCopmutations type_
                               
                               in ( n
                                  , fst$getTheBest type_ permutations
                                  )




shapeNormalize::Type -> Type
shapeNormalize = \case
               Node (Closed "Maybe") [a]        -> shapeNormalize         $ Node (Basic Sum) [Node (Basic Prod) [],a] 

               Node (Basic  Sum)     xs         -> (shapeNormalizeSum.map shapeNormalize)     xs
               
               Node (Basic  Prod)    xs         -> (shapeNormalizeProduct.map shapeNormalize) xs

               Node (Basic  Exp)   (base:args)  -> let args' = map shapeNormalize args
                                                       base' = shapeNormalize base 

                                                    in shapeNormalizeExp base' args' 

               Node (Basic  Exp)   _            -> error "this shape is ilegal!"


               Node fun              xs         -> let y = Node fun $ map shapeNormalize        xs -- to normalize or to not normalize :s
                                                    in Node (Basic Sum) [Node (Basic Prod) [y]]

--------------------------------------------------------------------------------------------------------------------------------------------
-- here, it is assumed, the inner arguments are already normalized....

shapeNormalizeSum::[Type] -> Type
shapeNormalizeSum xs = Node (Basic Sum) [ y | Node _ ys <- xs
                                       , y         <- ys
                                       ] 

shapeNormalizeProduct::[Type]->Type 
shapeNormalizeProduct = \case 
                      Node _ ys : xs -> Node (Basic Sum) [ Node (Basic Prod) (y++z) | let Node _ zs  =  shapeNormalizeProduct xs 
                                                                                    , Node _ y       <- ys 
                                                                                    , Node _ z       <- zs
                                                                                    ]                    

                      []            -> Node (Basic Sum) [Node (Basic Prod) []]

-- take a look and see if this works....the tricky part! here we go!
-- TODO, check what happen in corner casses..
shapeNormalizeExp::Type -> [Type] -> Type
shapeNormalizeExp base args = Node (Basic Sum) [ Node (Basic Prod) [ Node (Basic Exp) (base'': (acc++branch))
                                                              | base'            <- cleanBase base 
                                                              , Node _ arg_      <- args
                                                              , Node _ branch    <- arg_
                                                              , let (base'',acc) =  descompose base'
                                                              ]
                                          ]
cleanBase::Type -> [Type]
cleanBase x = case x of
               Node base [Node base_ products] -> products
               y                               -> [y] 

descompose::Type -> (Type,[Type])
descompose x = case x of 
                Node (Basic Exp) (base:arg) -> (base,arg)
                y                           -> (y   ,[] )


------------------------------------------------------------------------------------------------------------------------------------------------
-------------- sorting:
specialSort :: Type -> Type 
specialSort x = case x of
                 Node (Basic Exp) (x:xs)  -> Node (Basic Exp)  . (x:).sort $ fmap specialSort xs
                 Node base        xs      -> Node base         .      sort $ fmap specialSort xs 




getTheBest::ContextType -> [Permutation] -> (ContextType,Permutation)
getTheBest (ContextType x y) permutations = foldl min (compute zeroPermutation) [ compute p | p <- permutations]
   where
    compute p = ( ContextType (specialSort$ use p x) (specialSort$ use p y) 
                , p
                )

zeroPermutation::Permutation
zeroPermutation = fromList $ [(i,i)|i <- [0..10]]


use::Permutation->Type -> Type 
use p t = case t of 
            Node (Open n) xs  -> Node (Open$permute n) $ fmap (use p) xs
            Node base xs      -> Node base $ fmap (use p) xs
  where

   permute x 
     | member x p = p!x   
     | otherwise  = error $ show (p,x)-- p!x -- quitar de ahi...

type Permutation = Map Int Int




-----------------------------------------------------------------------------------------------
usefullCopmutations:: ContextType -> (Int,[Permutation])
usefullCopmutations (ContextType x y) = generatePermutations.dropRepeated $ usefull x ++ usefull y

{-
 Plenty room for improvement, buuuut, would it worth it?
-}
usefull::Type -> [[Int]]
usefull =  \case
                
                Node (Basic  Exp)   (base:args)  -> usefull base ++ expand args
                
                Node (Basic  Exp)   _            -> error "this shape is ilegal!!!"
                
                Node (Basic  _  )   xs           -> expand xs
                
                Node (Closed _  )   xs           -> expand xs

                Node (Open   x  )   xs           -> [x]: expand xs  
   
   where

    expand::[Type] -> [[Int]]
    expand list = [  (join.usefull) =<< samePriority
                  | samePriority <- parcialSort list
                  ]

{-
 Plenty room for improvement, buuuut, would it worth it?
-}
parcialSort::[Type]->[[Type]]
parcialSort = groupBy fun .sort
   where
    fun (Node a _ ) (Node b _ ) = a == b 




dropRepeated::[[Int]]-> [[Int]]
dropRepeated = snd.mapAccumL fun S.empty
   where
    fun used xs = let xs'   = nub$filter (not.flip S.member used) xs
                      used' = foldr S.insert used xs

                   in (used',xs')

generatePermutations::[[Int]] -> (Int,[Permutation])
generatePermutations options = ( product $ fmap (factorial.length)options
                               , fmap (fromList.zip ( nub$join options) ) (select$fmap permutations options)
                               ) -- nub shouldnt be neccesary -----> Something to Check!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                 -- !!!!!!!!!!!!!!!!!!!!!!!!
        where

          select (x:xs) = [ a++as
                          | a  <- x
                          , as <- select xs
                          ] 

          select []     = [[]]

          factorial n   = product [1..n]

------------------------------------------------------------------------------------------------------------------------------------









