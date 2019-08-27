{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Adornt.Parts (
	-- * Simple Parts
	xorGate, xorGate', nandGate, norGate, andNotBGate, orNotBGate,
	-- * 2, 3, 4 Input Wires
	andGate3, andGate4, orGate3, orGate4, xorGate3, xorGate4,
	mux2, mux2', mux3, mux3', mux4, mux4',
	-- * Multiple Input Wires
	multiple, multiple', decoder, multiplexer, multiplexer',
	-- * PLA
	pla8,
	-- * Zero Detector
	zeroDetector,
	-- * Carry Lookahead
	carries,
	-- * Memory
	srlatch, dlatch, dflipflop
	) where

import Data.Word

import Circuit.Adornt.Builder

import Circuit.Adornt.PartsBasic
import CarryLookahead2
import Tools

xorGate' :: CircuitBuilder Wire21
xorGate' = do
	abo@(a, b, o) <- xorGate
	putNamedBlock "xor" [a, b] [o]
	return abo

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

mux2, mux2' :: CircuitBuilder Wire31
mux2 = mux2Gen multiplexer
mux2' = mux2Gen multiplexer'

mux2Gen :: (Word16 -> CircuitBuilder (IWire, [IWire], OWire)) ->
	CircuitBuilder Wire31
mux2Gen mx = do
	(sl, is, o) <- mx 2
	let	(i0, i1) = listToTuple2 is
	return (sl, i0, i1, o)

mux3, mux3' :: CircuitBuilder Wire41
mux3 = mux3Gen multiplexer
mux3' = mux3Gen multiplexer'

mux3Gen :: (Word16 -> CircuitBuilder (IWire, [IWire], OWire)) ->
	CircuitBuilder Wire41
mux3Gen mx = do
	(sl, is, o) <- mx 3
	let	(i0, i1, i2) = listToTuple3 is
	return (sl, i0, i1, i2, o)

mux4, mux4' :: CircuitBuilder Wire51
mux4 = mux4Gen multiplexer
mux4' = mux4Gen multiplexer'

mux4Gen :: (Word16 -> CircuitBuilder (IWire, [IWire], OWire)) ->
	CircuitBuilder Wire51
mux4Gen mx = do
	(sl, is, o) <- mx 4
	let	(i0, i1, i2, i3) = listToTuple4 is
	return (sl, i0, i1, i2, i3, o)

multiple' :: BlockName -> CircuitBuilder Wire21 -> Word16 -> CircuitBuilder ([IWire], OWire)
multiple' nm gt n = do
	ios@(is, o) <- multiple gt n
	putNamedBlock (nm ++ "_" ++ show n) is [o]
	return ios

multiplexer' :: Word16 -> CircuitBuilder (IWire, [IWire], OWire)
multiplexer' n = do
	ios@(s, is, o) <- multiplexer n
	putNamedBlock ("mux_" ++ show n) (s : is) [o]
	return ios
