{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Adornt.Parts (nandGate, norGate) where

import Circuit.Adornt.Builder

norGate, nandGate :: CircuitBuilder Wire21
norGate = do
	(oa, ob, oo) <- orGate
	(ni, no) <- notGate
	connectWire64 oo ni
	return (oa, ob, no)

nandGate = do
	(aa, ab, ao) <- andGate
	(ni, no) <- notGate
	connectWire64 ao ni
	return (aa, ab, no)
