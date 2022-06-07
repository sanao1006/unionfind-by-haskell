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

modifyArray :: (MA.MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
{-# INLINE modifyArray #-}
modifyArray !a !i !f = do
  !ai <- MA.readArray a i
  MA.writeArray a i (f ai)

main :: IO ()
main = do
    [!n, !q] <- input @[Int]
    !tuv <- inputs @[[Int]]
    !uf <- initUF n
    forM_ tuv $ \[!t,!u,!v] -> do
        !same <- sameUF uf u v
        case t of
          0 -> uniteUF uf u v
          1 -> print $ if same then 1 else 0
    

-- ==================================================================

type Bs = BS.ByteString
type Height = Int
type Width = Int
type MutableArray2D any = IOUArray (Height,Width) any
type Array2D any = UArray (Height, Width) any

-- 入力 =====================================================================================================================================================
class (Read a) => Input a where
  read' :: BS.ByteString -> a
  readArray2D :: Height -> Width -> BS.ByteString -> a

input :: Input a => IO a
{-# INLINE input #-}
input = read' <$!> BS.getLine

inputs :: Input a => IO a
{-# INLINE inputs #-}
inputs = read' <$!> BS.getContents

inputArray :: Input a => Int -> Int -> IO a
{-# INLINE inputArray #-}
inputArray = flip flip BS.getContents . ((<$>) .) . readArray2D

instance Input Bs where
  read' = id
instance Input Int where
  read' = fst . fromJust . BS.readInt
instance Input Integer where
  read' = (read @Integer) . BS.unpack
instance Input Double where
  read' = read . BS.unpack
instance Input [Bs] where
  read' = BS.words
instance Input [Int] where
  read' = map ((read @Int) . BS.unpack) . BS.words
instance Input [Double] where
  read' = map ((read @Double) . BS.unpack) . BS.words
instance Input [(Int, Bs)] where
  read' = map ((\[a, b] -> (fst (fromJust (BS.readInt a)), b)) . BS.words) . BS.lines
instance Input [(Bs, Int)] where
  read' = map ((\[a, b] -> (a, fst (fromJust (BS.readInt b)))) . BS.words) . BS.lines
instance Input [[Bs]] where
  read' = map BS.words . BS.lines
instance Input [[Int]] where
  read' = map (map (fst . fromJust . BS.readInt) . BS.words) . BS.lines
instance Input [[Double]] where
  read' = map (map ( (read @Double) . BS.unpack) . BS.words) . BS.lines
instance Input (Array2D Int) where
  readArray2D = flip flip ((map (fst . fromJust . BS.readInt) . BS.words) <=< BS.lines) . (((.) . A.listArray . ((0, 0) ,)) .) . (. subtract 1) . (,) . subtract 1
instance Input (Array2D Double) where
  readArray2D height width = A.listArray ((0, 0), (height - 1, width - 1)) . concatMap (map ((read @Double) . BS.unpack) . BS.words) . BS.lines
instance Input (Array2D Char) where
 readArray2D = flip flip (BS.unpack <=< BS.lines) . (((.) . A.listArray . ((0, 0) ,)) .) . (. subtract 1) . (,) . subtract 1
-- ============================================================================================================================================================
