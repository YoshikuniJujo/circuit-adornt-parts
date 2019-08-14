{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Adornt.Parts (
	xorGate, nandGate, norGate, andNotBGate, orNotBGate,
	multiple ) where

import Control.Arrow
import Data.Word

import Circuit.Adornt.Builder

nandGate, norGate :: CircuitBuilder Wire21
nandGate = do
	(aa, ab, ao) <- andGate
	(ni, no) <- notGate
	connectWire64 ao ni
	return (aa, ab, no)

norGate = do
	(oa, ob, oo) <- orGate
	(ni, no) <- notGate
	connectWire64 oo ni
	return (oa, ob, no)

xorGate :: CircuitBuilder Wire21
xorGate = do
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(aa, ab, ao) <- andGate
	(na, nb, no) <- norGate
	(ad, nor, xo) <- norGate
	connectWire64 aout `mapM_` [aa, na]
	connectWire64 bout `mapM_` [ab, nb]
	connectWire64 ao ad
	connectWire64 no nor
	return (ain, bin, xo)

andNotBGate, orNotBGate :: CircuitBuilder Wire21
andNotBGate = do
	(ni, no) <- notGate
	(aa, ab, ao) <- andGate
	connectWire64 no ab
	return (aa, ni, ao)

orNotBGate = do
	(ni, no) <- notGate
	(aa, ab, ao) <- orGate
	connectWire64 no ab
	return (aa, ni, ao)

multiple :: CircuitBuilder Wire21 -> Word16 -> CircuitBuilder ([IWire], OWire)
multiple _ 0 = ([] ,) <$> constGate 0
multiple _ 1 = first (: []) <$> idGate
multiple g 2 = (\(a, b, o) -> ([a, b], o)) <$> g
multiple g n = do
	(is1, o1) <- multiple g (n `div` 2)
	(is2, o2) <- multiple g (n - n `div` 2)
	(a, b, o) <- g
	connectWire64 o1 a
	connectWire64 o2 b
	return (is1 ++ is2, o)
