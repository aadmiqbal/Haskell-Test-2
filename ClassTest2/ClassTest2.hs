-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest2 (stateTrib, runStateTrib, writeLeaves, collapse, mapLeavesWithAddress, toQuadTree, fromQuadTree) where

import Data.List
import Data.Char

import Control.Monad.State
import Control.Monad.Writer

import Types

-- Question 1

stateTrib :: Integer -> State (Integer,Integer,Integer) ()
stateTrib n| n==1 = pure ()
           | otherwise = do
                            modify (\(z,y,x) -> ((x+y+z),z,y))
                            stateTrib (n-1)

runStateTrib :: Integer -> Integer
runStateTrib n =
  let ((),(a,b,c)) = runState (stateTrib n) (1,0,0)
  in a

-- Question 2

writeLeaves :: Bin a b -> Writer [Either a b] ()
writeLeaves (Lf a) = do
                        tell [Left a]
                        pure ()
writeLeaves (Nd x l r) = do
                            writeLeaves l
                            tell [Right x]
                            writeLeaves r
                            pure ()
                        

-- Question 3

collapse :: Bin (Bin a b) b -> Bin a b
collapse (Lf (Nd x a b) ) = (Nd x (a) (b))
collapse (Lf (Lf a)) = (Lf a)
collapse (Nd x a b) = (Nd x (collapse a) (collapse b))

-- Question 4

mapLeavesWithAddress :: (a -> Address -> c) -> Bin a b -> Bin c b
mapLeavesWithAddress = undefined

-- Question 5

toQuadTree :: Image -> QuadTree
toQuadTree [[x]] = (P x)
toQuadTree [(x:y:z:a:[])] = N (toQuadTree [[x]]) (toQuadTree [[y]]) (toQuadTree [[z]]) (toQuadTree [[a]]) 



fromQuadTree :: QuadTree -> Image
fromQuadTree (N (P a) (P b) (P c) (P d)) = [[a,b,c,d]]
fromQuadTree (N a b c d) = (fromQuadTree a) ++ (fromQuadTree b) ++ (fromQuadTree c) ++ (fromQuadTree d)
fromQuadTree (P x) = [[x]]
