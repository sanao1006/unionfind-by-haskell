{-# LANGUAGE BangPatterns , BlockArguments ,FlexibleContexts ,FlexibleInstances ,OverloadedStrings ,TypeApplications ,MultiParamTypeClasses ,TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

import Control.Monad ((<$!>),replicateM,replicateM_,forM_,when,(<=<),liftM2,liftM,unless)
import Data.Maybe (fromJust)
import Data.Array.IO(IOUArray,IOArray)
import Data.Array.Unboxed(UArray,Ix)
import Control.Monad.ST
import Data.Array.ST(STArray,STUArray)
import qualified Data.Array.MArray     as MA
import qualified Data.Array.IArray     as A
import qualified Data.ByteString.Char8 as BS
import System.IO.Unsafe(unsafePerformIO)
import Data.Function(fix)
import qualified Data.IORef as IOR
-- ==================================================================

data UnionFind = UnionFind{
    parent :: IOUArray Int Int,
    rank :: IOUArray Int Int
} 

initUF :: Int -> IO  UnionFind
{-# INLINE initUF #-}
initUF !n = UnionFind <$!> (MA.newArray(0, n-1)(-1) :: IO(IOUArray Int Int)) <*> (MA.newArray (0, n-1) 1 :: IO(IOUArray Int Int))

rootUF :: UnionFind -> Int -> IO Int
{-# INLINE rootUF #-}
rootUF !uf !x = do
    px <- MA.readArray (parent uf) x
    rootUF' uf px (return x)
        where
          rootUF' !uf !px !x
              | px == -1 = x
              | otherwise = rootUF uf px

sameUF :: UnionFind -> Int -> Int -> IO Bool
{-# INLINE sameUF #-}
sameUF !uf !x !y = (==) <$!> (rootUF uf x) <*> (rootUF uf y)

uniteUF :: UnionFind -> Int -> Int -> IO()
{-# INLINE uniteUF #-}
uniteUF !uf !x !y = do
    !a <- rootUF uf x
    !b <- rootUF uf y
    !ra <- MA.readArray (rank uf) a
    !rb <- MA.readArray (rank uf) b
    uniteUF' uf a b ra rb
        where
            uniteUF' !uf !a !b !ra !rb
                | a == b = pure()
                | otherwise = do
                    case ra `compare` rb of
                        LT -> do
                            MA.writeArray (parent uf) a b
                        GT -> do
                            MA.writeArray (parent uf) b a
                        EQ -> do
                            MA.writeArray (parent uf) a b
                            modifyArray(rank uf) b (+1)

groupUF :: UnionFind -> IO Int
groupUF uf = do
  b <- MA.freeze(parent uf)::IO(UArray Int Int)
  groupUF' (A.elems b) 0
  where
    groupUF':: [Int] -> Int ->IO Int
    groupUF' [] ans = return ans
    groupUF' (a:as) ans
      | a < 0 = groupUF' as (ans+1)
      | otherwise = groupUF' as ans

modifyArray :: (MA.MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
{-# INLINE modifyArray #-}
modifyArray !a !i !f = do
  !ai <- MA.readArray a i
  MA.writeArray a i (f ai)

    
 
