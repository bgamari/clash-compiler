{-# LANGUAGE DataKinds #-}

module T2988B where

import Clash.Prelude
import Prelude ()

testBench :: Signal System Bool
testBench =
    let x = pure $ repeat $ repeat 0xab_dead_beef
    in (== repeat 0xdead_beef) <$> topEntity x

topEntity
    :: Signal System (Vec 4 (Vec 4 (Unsigned 32)))
    -> Signal System (Vec 4 (Unsigned 32))
topEntity = f

f
    :: Signal System (Vec n (Vec 4 (Unsigned 32)))
    -> Signal System (Vec n (Unsigned 32))
f x = fmap (fmap head) x
{-# OPAQUE f #-}

