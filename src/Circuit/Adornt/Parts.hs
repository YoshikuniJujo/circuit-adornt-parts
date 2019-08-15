{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Adornt.Parts (
	-- * Simple Parts
	xorGate, nandGate, norGate, andNotBGate, orNotBGate,
	-- * 2, 3, 4 Input Wires
	mux2, mux3, mux4,
	-- * Multiple Input Wires
	multiple, decoder, multiplexer ) where

import Circuit.Adornt.Builder

import Circuit.Adornt.PartsBasic
import Tools

mux2 :: CircuitBuilder Wire31
mux2 = do
	(sl, is, o) <- multiplexer 2
	let	(i0, i1) = listToTuple2 is
	return (sl, i0, i1, o)

mux3 :: CircuitBuilder (IWire, IWire, IWire, IWire, OWire)
mux3 = do
	(sl, is, o) <- multiplexer 3
	let	(i0, i1, i2) = listToTuple3 is
	return (sl, i0, i1, i2, o)

mux4 :: CircuitBuilder (IWire, IWire, IWire, IWire, IWire, OWire)
mux4 = do
	(sl, is, o) <- multiplexer 4
	let	(i0, i1, i2, i3) = listToTuple4 is
	return (sl, i0, i1, i2, i3, o)
