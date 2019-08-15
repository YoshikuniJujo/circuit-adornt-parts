{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Adornt.Parts (
	-- * Simple Parts
	xorGate, nandGate, norGate, andNotBGate, orNotBGate,
	-- * 2, 3, 4 Input Wires
	andGate3, andGate4, orGate3, orGate4, xorGate3, xorGate4,
	mux2, mux3, mux4,
	-- * Multiple Input Wires
	multiple, decoder, multiplexer ) where

import Circuit.Adornt.Builder

import Circuit.Adornt.PartsBasic
import Tools

andGate3, orGate3, xorGate3 :: CircuitBuilder Wire31
[andGate3, orGate3, xorGate3] = multi3 <$> [andGate, orGate, xorGate]

andGate4, orGate4, xorGate4 :: CircuitBuilder Wire41
[andGate4, orGate4, xorGate4] = multi4 <$> [andGate, orGate, xorGate]

multi3 :: CircuitBuilder Wire21 -> CircuitBuilder Wire31
multi3 g = do
	(is, o) <- multiple g 3
	let	(i0, i1, i2) = listToTuple3 is
	return (i0, i1, i2, o)

multi4 :: CircuitBuilder Wire21 -> CircuitBuilder Wire41
multi4 g = do
	(is, o) <- multiple g 4
	let	(i0, i1, i2, i3) = listToTuple4 is
	return (i0, i1, i2, i3, o)

mux2 :: CircuitBuilder Wire31
mux2 = do
	(sl, is, o) <- multiplexer 2
	let	(i0, i1) = listToTuple2 is
	return (sl, i0, i1, o)

mux3 :: CircuitBuilder Wire41
mux3 = do
	(sl, is, o) <- multiplexer 3
	let	(i0, i1, i2) = listToTuple3 is
	return (sl, i0, i1, i2, o)

mux4 :: CircuitBuilder Wire51
mux4 = do
	(sl, is, o) <- multiplexer 4
	let	(i0, i1, i2, i3) = listToTuple4 is
	return (sl, i0, i1, i2, i3, o)
