{-|
Copyright  :  (C) 2019, Ben Gamari
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

IO Buffer primitives for Xilinx FPGAs

For more information about the Xilinx IO buffer primitives see:
    * 7 Series FPGAs: SelectIO Resources User Guide,
      UG471 (v1.10)
      https://www.xilinx.com/support/documentation/user_guides/ug471_7Series_SelectIO.pdf
-}

{-# LANGUAGE CPP              #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif

module Clash.Xilinx.IOB
  ( -- * LVDS
    DiffPair
  , diffPairPortNames
  , lvdsInput
  , lvdsOutput
    -- ** Clocks
  , lvdsClockInput
  , lvdsClockOutput
  )
where

import Clash.Explicit.Prelude

data DiffPair a = DiffPair { diffQ, diffQInv :: a }

-- | Do not use to destructure.
instance Bundle (DiffPair a) where
    type Unbundled dom (DiffPair a) = DiffPair (Signal dom a)
    unbundle x = DiffPair (fmap diffQ x) (fmap diffQInv x)
    bundle x = DiffPair <$> diffQ x <*> diffQInv x

toDiffPair :: Bool -> DiffPair Bool
toDiffPair b = DiffPair b (not b)

diffPairPortNames :: PortName
diffPairPortNames = PortProduct "diff" [PortName "q", PortName "qn"]

-- | The Xilinx IBUFDS differential input buffer primitive.
lvdsInput :: DiffPair (Signal dom Bool) -> Signal dom Bool
lvdsInput = lvdsInput#

lvdsInput# :: DiffPair (Signal dom Bool) -> Signal dom Bool
lvdsInput# = diffQ
{-# NOINLINE lvdsInput# #-}

-- | The Xilinx OBUFDS differential output buffer primitive.
lvdsOutput :: Signal dom Bool -> Signal dom (DiffPair Bool)
lvdsOutput = lvdsOutput#

lvdsOutput# :: Signal dom Bool -> Signal dom (DiffPair Bool)
lvdsOutput# = fmap toDiffPair
{-# NOINLINE lvdsOutput# #-}

-- | The Xilinx OBUFDS differential output buffer primitive for clock signals.
lvdsClockOutput :: Clock dom gated -> DiffPair (Clock dom gated)
lvdsClockOutput x = DiffPair x x
{-# NOINLINE lvdsClockOutput #-}

-- | The Xilinx OBUFDS differential output buffer primitive for clock signals.
lvdsClockInput :: DiffPair (Clock dom 'Source) -> Clock dom 'Source
lvdsClockInput (DiffPair x _) = x
{-# NOINLINE lvdsClockInput #-}
