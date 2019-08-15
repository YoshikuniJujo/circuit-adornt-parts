{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Adornt.PartsBasic (
	xorGate, nandGate, norGate, andNotBGate, orNotBGate,
	multiple, decoder, multiplexer, pla8, zeroDetector,
	srlatch, dlatch, dflipflop ) where

import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.List
import Data.Bool
import Data.Word

import qualified Data.Bits as B

import Circuit.Adornt.Builder

import Tools

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

decoderGen :: Word16 -> CircuitBuilder ([IWire], [OWire])
decoderGen n = do
	(is, ois) <- unzip <$> fromIntegral m `replicateM` idGate
	(ias, oas) <- unzip <$> fromIntegral n `replicateM` multiple andGate m
	zipWithM_ ((sequence_ .) . flip (zipWith3 id) ois)
		(binary (inverse, obverse) m) ias
	return (is, oas)
	where m = log2 n

decoder :: Word16 -> CircuitBuilder (IWire, [OWire])
decoder n = do
	(iin, iout) <- idGate
	(is, decs) <- decoderGen n
	for_ (zip [0 ..] is) $ \(ix, iw) -> connectWire (iout, 1, ix) (iw, 1, 0)
	return (iin, decs)

inverse, obverse :: OWire -> IWire -> CircuitBuilder ()
inverse o i = do
	(ni, no) <- notGate
	zipWithM_ connectWire64 [o, no] [ni, i]
obverse = connectWire64

multiplexer :: Word16 -> CircuitBuilder (IWire, [IWire], OWire)
multiplexer n = do
	(slin, douts) <- decoder n
	(as, bs, os) <- unzip3 <$> fromIntegral n `replicateM` andGate
	(ois, oo) <- multiple orGate n
	zipWithM_ connectWire0_64 douts as
	zipWithM_ connectWire64 os ois
	return (slin, bs, oo)

pla8 :: [(Word8, Word8)] -> CircuitBuilder (IWire, OWire)
pla8 tbl_ = do
	(iin, iout) <- idGate
	(iss, outs) <- plaGen 8 tbl
	(oin, oout) <- idGate
	connectWireOutToIns iout iss
	connectWireOutsToIn outs oin
	z <- constGate 0
	connectWire (z, 56, 0) (oin, 56, 8)
	return (iin, oout)
	where
	tbl = (numToBits 8 *** numToBits 8) <$> tbl_
	numToBits :: B.Bits n => Word8 -> n -> [Bit]
	numToBits c n = map (bool O I . B.testBit n) [0 .. fromIntegral c - 1]
	connectWireOutToIns o is = zipWithM_ (\n i -> connectWire (o, 1, n) (i, 1, 0)) [0 ..] is
	connectWireOutsToIn os i = zipWithM_ (\n o -> connectWire (o, 1, 0) (i, 1, n)) [0 ..] os

data Bit = O | I deriving (Show, Eq)

plaGen :: Word8 -> [([Bit], [Bit])] -> CircuitBuilder ([IWire], [OWire])
plaGen n_ tbl = do
	(iins, iouts) <- unzip <$> n `replicateM` idGate
	rs <- pla1 iouts `mapM` trSep tbl
	return (iins, rs)
	where
	n = fromIntegral n_
	trSep = transpose . map sepSnd
	sepSnd (x, ys) = zip (repeat x) ys

pla1 :: [OWire] -> [([Bit], Bit)] -> CircuitBuilder OWire
pla1 iouts tbl1 = do
	fbs <- frbk0 `mapM` iouts
	let	ws = map (zipWith bitIndex fbs) ons
	r <- allOr =<< mapM allAnd ws
	return r
	where
	ons = fst <$> filter ((== I) . snd) tbl1
	allAnd os = do
		(as, o) <- multiple andGate . fromIntegral $ length os
		zipWithM_ connectWire64 os as
		return o
	allOr os = do
		(xs, o) <- multiple orGate . fromIntegral $ length os
		zipWithM_ connectWire64 os xs
		return o
	bitIndex (x, _) O = x
	bitIndex (_, y) I = y
	frbk0 o = do
		(ni, no) <- notGate
		connectWire0 o ni
		return  (no, o)

zeroDetector :: CircuitBuilder (IWire, OWire)
zeroDetector = do
	(iin, iout) <- idGate
	(ois, oo) <- multiple orGate 64
	(ni, no) <- notGate
	for_ (zip [0 ..] ois) $ \(i, oi) -> connectWire (iout, 1, i) (oi, 1, 0)
	connectWire0 oo ni
	(oin, oout) <- idGate
	connectWire0 no oin
	cz <- constGate 0
	connectWire (cz, 63, 0) (oin, 63, 1)
	return (iin, oout)

srlatch :: CircuitBuilder Wire22
srlatch = do
	(r, nqin, qout) <- norGate
	(s, qin, nqout) <- norGate
	connectWire64 nqout nqin
	connectWire64 qout qin
	return (r, s, qout, nqout)

dlatch :: CircuitBuilder Wire22
dlatch = do
	(cin, cout) <- idGate
	(din, dout) <- idGate
	(cr, dr, rout) <- andNotBGate
	(cs, ds, sout) <- andGate
	(r, s, q, nq) <- srlatch
	connectWire0_64 cout `mapM_` [cr, cs]
	connectWire64 dout `mapM_` [dr, ds]
	connectWire64 rout r
	connectWire64 sout s
	return (cin, din, q, nq)

dflipflop :: CircuitBuilder Wire22
dflipflop = do
	(cin, cout) <- idGate
	(ni, no) <- notGate
	connectWire0 cout ni
	(cm, dm, qm, _nqm) <- dlatch
	(cs, ds, qs, nqs) <- dlatch
	connectWire0 cout cm
	connectWire0 no cs
	connectWire64 qm ds
	return (cin, dm, qs, nqs)
