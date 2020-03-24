{-# LANGUAGE BinaryLiterals #-}
module Opcodes where

import qualified Data.Map.Strict as Map

type CodeMap = Map.Map String Int

nullaryCodes :: CodeMap
nullaryCodes = Map.fromList [
        ("ASR", 0b0111000100000001), -- ALU
        ("DEC", 0b0111101100000000),
        ("INC", 0b0000000000000000),
        ("LSL", 0b0101100000000000),
        ("LSR", 0b0111000100000000),
        ("NEG", 0b0010011100000000),
        ("NOT", 0b0010110100000000),
        ("RLC", 0b0101000000000000),
        ("ROL", 0b0101001000000000),
        ("ROR", 0b0111000100000010),
        ("RRC", 0b0111000100000011),
        ("STI", 0b0111111110000001), -- Flag
        ("CLI", 0b0000011101101001),
        ("STU", 0b0111111100100010),
        ("CLU", 0b0000011111001010),
        ("STC", 0b0111111100001100),
        ("CLC", 0b0000011111100100),
        ("TAX", 0b0000011110000000), -- Index register
        ("TXA", 0b0110011100000001),
        ("INX", 0b0000010110000000),
        ("DEX", 0b0000110110000000),
        ("TAS", 0b0000011101010000),
        ("TSA", 0b0110011100000000),
        ("INS", 0b0000011001000000),
        ("DES", 0b0000111001000000),
        ("RTS", 0b0001111100000000), -- Subroutine & stack
        ("POPF", 0b0000001000000000),
        ("PUSHF", 0b0000111000000000),
        ("NOP", 0b0001111110000000)  -- Misc
    ]

absoluteDataCodes :: CodeMap
absoluteDataCodes = Map.fromList [
        ("ADC", 0b01100000), --ALU
        ("ADD", 0b01101000),
        ("AND", 0b01000100),
        ("CMP", 0b00110000),
        ("OR", 0b01110100),
        ("SBB", 0b00011000),
        ("SUB", 0b00010000),
        ("TST", 0b01001100),
        ("XOR", 0b00110100),
        ("LDD", 0b10000000), -- Load/store
        ("STD", 0b10100000)
    ]

immediateValueCodes :: CodeMap
immediateValueCodes = Map.fromList [
        ("ADCI", 0b01100011), -- ALU
        ("ADDI", 0b01101011),
        ("ANDI", 0b01000111),
        ("CMPI", 0b00110011),
        ("SBBI", 0b00011011),
        ("SUBI", 0b00010011),
        ("TSTI", 0b01001111),
        ("XORI", 0b00110111),
        ("LDI", 0b10001001)  --Load/store
   ]

relativeJumpCodes :: CodeMap
relativeJumpCodes = Map.fromList [
        ("JA", 0b10001000),
        ("JAE", 0b10001100),
        ("JNC", 0b10001100),
        ("JB", 0b10001111),
        ("JC", 0b10001111),
        ("JBE", 0b10001011),
        ("JE", 0b10011111),
        ("JZ", 0b10011111),
        ("JG", 0b10101111),
        ("JGE", 0b10111011),
        ("JL", 0b10111000),
        ("JLE", 0b10101100),
        ("JNE", 0b10011100),
        ("JNZ", 0b10011100),
        ("JNS", 0b10011000),
        ("JNU", 0b10111100),
        ("JNV", 0b10101000),
        ("JS", 0b10011011),
        ("JU", 0b10111111),
        ("JV", 0b10101011)
    ]
