{-# LANGUAGE LambdaCase #-}

module Source.TypeNormalizer.TestSuite where

import Source.TypeNormalizer.Model
import Source.TypeNormalizer.Normalizer
import Source.TypeNormalizer.Parser
import Control.Monad
import Data.List hiding (insert)
import Data.Set hiding (filter,foldr,foldl,fromList)
import Data.Map (Map,fromList)
------------------------------------------------------------------
-- Testing the parser:












------------------------------------------------------------------
-- Testing the normalizer:



equivalent::ContextType -> Bool
equivalent (ContextType a b) = let (x,y) = ( shapeNormalize a, shapeNormalize b)
                   
                                in   getTheBest (ContextType x y) (toPermutation$openVar x y)
                                     ==
                                     getTheBest (ContextType x y) (snd$usefullCopmutations x y)

               
toPermutation::[Int]->[Permutation] 
toPermutation x = fmap (fromList.zip x) $ permutations x

openVar::Type -> Type -> [Int]
openVar a b = (take 1=<<).group.sort $ openVar' a ++ openVar' b
   where
    openVar' x = case x of 
                   Node (Open n) xs  -> n : (openVar' =<<xs)
                   Node base xs      -> (openVar' =<<xs)      


getNormalForm::ContextType -> (ContextType,Permutation)
getNormalForm (ContextType x y )= let permut    = toPermutation  $ openVar x y
                                      (x',y')   = (shapeNormalize x, shapeNormalize y)

                                   in getTheBest (ContextType x' y') permut



------------------------------------------------------------------
-- Other Tests:





