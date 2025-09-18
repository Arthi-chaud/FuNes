module Nes.CPU.Instructions.Map (opcodeMap) where

import Data.ByteString
import Data.Map (Map, fromList)
import Nes.CPU.Instructions.Addressing
import Nes.CPU.Instructions.B
import Nes.CPU.Instructions.CL
import Nes.CPU.Instructions.CMP
import Nes.CPU.Instructions.DE
import Nes.CPU.Instructions.IN
import Nes.CPU.Instructions.JMP
import Nes.CPU.Instructions.LD
import Nes.CPU.Instructions.SE
import Nes.CPU.Instructions.ST
import Nes.CPU.Instructions.TA
import Nes.CPU.Monad
import Nes.Memory (Byte)

type OpCodeEntry r = (ByteString, AddressingMode -> CPU r (), AddressingMode)

-- | Maps op code to the function that executes it and the addressing mode
opcodeMap :: Map Byte (OpCodeEntry r)
opcodeMap =
    fromList
        [ (0x85, ("STA", sta, ZeroPage))
        , (0x95, ("STA", sta, ZeroPageX))
        , (0x8D, ("STA", sta, Absolute))
        , (0x9D, ("STA", sta, AbsoluteX))
        , (0x99, ("STA", sta, AbsoluteY))
        , (0x81, ("STA", sta, IndirectX))
        , (0x91, ("STA", sta, IndirectY))
        , (0x86, ("STX", stx, ZeroPage))
        , (0x96, ("STX", stx, ZeroPageX))
        , (0x8E, ("STX", stx, Absolute))
        , (0x84, ("STY", sty, ZeroPage))
        , (0x94, ("STY", sty, ZeroPageX))
        , (0x8c, ("STY", sty, Absolute))
        , (0xa9, ("LDA", lda, Immediate))
        , (0xa5, ("LDA", lda, ZeroPage))
        , (0xb5, ("LDA", lda, ZeroPageX))
        , (0xad, ("LDA", lda, Absolute))
        , (0xbd, ("LDA", lda, AbsoluteX))
        , (0xb9, ("LDA", lda, AbsoluteY))
        , (0xa1, ("LDA", lda, IndirectX))
        , (0xb1, ("LDA", lda, IndirectY))
        , (0xa2, ("LDX", ldx, Immediate))
        , (0xa6, ("LDX", ldx, ZeroPage))
        , (0xb6, ("LDX", ldx, ZeroPageY))
        , (0xae, ("LDX", ldx, Absolute))
        , (0xbe, ("LDX", ldx, AbsoluteY))
        , (0xa0, ("LDY", ldy, Immediate))
        , (0xa4, ("LDY", ldy, ZeroPage))
        , (0xb4, ("LDY", ldy, ZeroPageX))
        , (0xac, ("LDY", ldy, Absolute))
        , (0xbc, ("LDY", ldy, AbsoluteX))
        , (0xe6, ("INC", inc, ZeroPage))
        , (0xf6, ("INC", inc, ZeroPageX))
        , (0xee, ("INC", inc, Absolute))
        , (0xfe, ("INC", inc, AbsoluteX))
        , (0xc6, ("DEC", dec, ZeroPage))
        , (0xd6, ("DEC", dec, ZeroPageX))
        , (0xce, ("DEC", dec, Absolute))
        , (0xde, ("DEC", dec, AbsoluteX))
        , (0x50, ("BVC", bvc, Relative))
        , (0x70, ("BVS", bvs, Relative))
        , (0x90, ("BCC", bcc, Relative))
        , (0xb0, ("BCS", bcs, Relative))
        , (0xf0, ("BEQ", beq, Relative))
        , (0xd0, ("BNE", bne, Relative))
        , (0x30, ("BMI", bmi, Relative))
        , (0x10, ("BPL", bpl, Relative))
        , (0xc9, ("CMP", cmp, Immediate))
        , (0xc5, ("CMP", cmp, ZeroPage))
        , (0xd5, ("CMP", cmp, ZeroPageX))
        , (0xcd, ("CMP", cmp, Absolute))
        , (0xdd, ("CMP", cmp, AbsoluteX))
        , (0xd9, ("CMP", cmp, AbsoluteY))
        , (0xc1, ("CMP", cmp, IndirectX))
        , (0xd1, ("CMP", cmp, IndirectY))
        , (0xe0, ("CPX", cpx, Immediate))
        , (0xe4, ("CPX", cpx, ZeroPage))
        , (0xec, ("CPX", cpx, Absolute))
        , (0xc0, ("CPY", cpy, Immediate))
        , (0xc4, ("CPY", cpy, ZeroPage))
        , (0xcc, ("CPY", cpy, Absolute))
        , (0x4c, ("JMP", jmp, Absolute))
        , (0x6c, ("JMP", jmp, Indirect))
        , -- W/o addressing
          (0x18, ("CLC", const clc, None))
        , (0xd8, ("CLD", const cld, None))
        , (0x58, ("CLD", const cli, None))
        , (0xB8, ("CLD", const clv, None))
        , (0x38, ("SEC", const sec, None))
        , (0xf8, ("SED", const sed, None))
        , (0x78, ("SEI", const sei, None))
        , (0xca, ("DEX", const dex, None))
        , (0x88, ("DEY", const dey, None))
        , (0xaa, ("TAX", const tax, None))
        , (0xa8, ("TAY", const tay, None))
        , (0xe8, ("INX", const inx, None))
        , (0xc8, ("INY", const iny, None))
        , (0xea, ("NOP", const $ pure (), None))
        , -- Note: for this one, the intepreter is responsible for breaking
          (0x00, ("BRK", const $ pure (), None))
        ]
