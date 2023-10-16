{-# LANGUAGE LambdaCase, ViewPatterns #-}
module RatBC.HomeLab2.Huffman where

import qualified Data.Map as M
import Data.List (group, sort)
import Data.Word
import Data.Bits
import Data.List.Split
import qualified Data.PQueue.Min as PQ
import Data.Function (on)

data Tree a
    = Node Int (Tree a) (Tree a)
    | Leaf Int a
    deriving Show

instance Eq (Tree a) where
    (==) = (==) `on` treeFreq

instance Ord (Tree a) where
    compare = compare `on` treeFreq

treeFreq :: Tree a -> Int
treeFreq = \case
    Node freq _ _ -> freq
    Leaf freq _ -> freq

build :: (Ord a) => [a] -> Tree a
build xs = go prio0
  where
    freqs = [ Leaf freq x | occs@(x:_) <- group . sort $ xs, let freq = length occs ]
    prio0 = foldr PQ.insert PQ.empty freqs

    go prio = case PQ.minView prio of
        Nothing -> error "Empty input"
        Just (t1, prio') -> case PQ.minView prio' of
            Nothing -> t1
            Just (t2, prio'') -> go $ PQ.insert (Node (treeFreq t1 + treeFreq t2) t1 t2) prio''

data Bit
    = B0
    | B1
    deriving (Eq, Show)

toBytes :: [Bit] -> [Word8]
toBytes = map toByte . chunksOf 8
  where
    toByte = foldr (\b x -> (x `shiftR` 1) .|. if b == B1 then 0x80 else 0x00) 0

encoder :: (Ord a) => Tree a -> (a -> [Bit])
encoder t = \x -> table M.! x
  where
    table = M.fromList $ go t

    go = \case
        Leaf _ x -> [(x, [])]
        Node _ t1 t2 -> [ (x, B0:bs) | (x, bs) <- go t1 ] <> [ (x, B1:bs) | (x, bs) <- go t2 ]

decoder :: Tree a -> ([Bit] -> [a])
decoder t = go t
  where
    go t' = \case
        [] -> case t' of
            Leaf _ x -> [x]
            Node{} -> error "Partial code word"
        b:bs -> case t' of
            Leaf _ x -> x : go t (b:bs)
            Node _ t1 t2 -> go (if b == B0 then t1 else t2) bs
