module Nes.CPU.Instructions.Transfer (
    -- * Transfer to/from register A
    tax,
    tay,
    txa,
    tya,

    -- * Transfter between register X and Status
    txs,
    tsx,
) where

import Control.Monad
import Nes.CPU.Instructions.After (setZeroAndNegativeFlags)
import Nes.CPU.Monad
import Nes.CPU.State
import Nes.Memory (Byte)

-- | Transfer Register A to X
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#TAX
tax :: CPU r ()
tax = transferToRegister A X >>= setZeroAndNegativeFlags

-- | Transfer Register A to Y
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#TAY
tay :: CPU r ()
tay = transferToRegister A Y >>= setZeroAndNegativeFlags

-- | Transfer Register X to A
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#TXA
txa :: CPU r ()
txa = transferToRegister X A >>= setZeroAndNegativeFlags

-- | Transfer Register Y to A
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#TYA
tya :: CPU r ()
tya = transferToRegister Y A >>= setZeroAndNegativeFlags

-- | Transfer Register X to S
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#TXS
txs :: CPU r ()
txs = void $ transferToRegister X S

-- | Transfer Register S to X
--
-- https://www.nesdev.org/obelisk-6502-guide/reference.html#TSX
tsx :: CPU r ()
tsx = void $ transferToRegister S X >>= setZeroAndNegativeFlags

transferToRegister :: Register -> Register -> CPU r Byte
transferToRegister src dest = do
    value <- getRegister src
    setRegister dest value
    return value
