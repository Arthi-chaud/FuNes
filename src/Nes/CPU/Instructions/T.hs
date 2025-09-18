module Nes.CPU.Instructions.T (tax, tay, txa, tya, txs, tsx) where

import Nes.CPU.Instructions.After
import Nes.CPU.Monad
import Nes.CPU.State

-- | Transfer Register A to X
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#TAX
tax :: CPU r ()
tax = transferToRegister A X

-- | Transfer Register A to Y
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#TAY
tay :: CPU r ()
tay = transferToRegister A Y

-- | Transfer Register X to A
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#TXA
txa :: CPU r ()
txa = transferToRegister X A

-- | Transfer Register Y to A
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#TYA
tya :: CPU r ()
tya = transferToRegister Y A

-- | Transfer Register X to S
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#TXS
txs :: CPU r ()
txs = transferToRegister X S

-- | Transfer Register S to X
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#TSX
tsx :: CPU r ()
tsx = transferToRegister S X

transferToRegister :: Register -> Register -> CPU r ()
transferToRegister src dest = do
    value <- getRegister src
    setRegister dest value
    setZeroAndNegativeFlags value
