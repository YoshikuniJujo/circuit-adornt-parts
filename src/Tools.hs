{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Data.Word

log2 :: (Integral n, Integral m) => n -> m
log2 i = i2 0
	where i2 j
		| j < 0 = error "circuit-adornt.Tools.log2.i2 j | j < 0"
		| 2 ^ j >= i = j
		| otherwise = i2 $ j + 1

binary :: (a, a) -> Word16 -> [[a]]
binary _ n | n < 1 = [[]]
binary (o, i) n = binary (o, i) (n - 1) >>= (<$> [(o :), (i :)]) . flip ($)

listToTuple2 :: [a] -> (a, a)
listToTuple2 [x, y] = (x, y)
listToTuple2 _ = error "circuit-adornt.Tools.listToTuple2 _"

listToTuple3 :: [a] -> (a, a, a)
listToTuple3 [x, y, z] = (x, y, z)
listToTuple3 _ = error "circuit-adornt.Tools.listToTuple3 _"

listToTuple4 :: [a] -> (a, a, a, a)
listToTuple4 [x, y, z, w] = (x, y, z, w)
listToTuple4 _ = error "circuit-adornt.Tools.listToTuple4 _"
