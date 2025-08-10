{-# LANGUAGE DataKinds #-}

module T2988A where

import Clash.Prelude
import Prelude ()

testBench :: Signal System Bool
testBench =
    let x = pure $ repeat 0xab_dead_beef
    in (== repeat 0xdead_beef) <$> topEntity x

topEntity
    :: Signal System (Vec 4 (BitVector 40))
    -> Signal System (Vec 4 (BitVector 32))
topEntity = f

f
    :: Signal System (Vec n (BitVector 40))
    -> Signal System (Vec n (BitVector 32))
f x = fmap (fmap truncateB) x
{-# OPAQUE f #-}
