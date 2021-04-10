{-# LANGUAGE QuasiQuotes #-}

module Output where

import Structure (Instruction (..), Register (..))
import Text.RawString.QQ (r)

compileToNASM :: [Instruction] -> String
compileToNASM instructions = prelude ++ inner ++ footer
  where
    inner = unlines $ zipWith toNASM [0 ..] instructions

label :: Int -> String
label n = "i" ++ show n

regToX86 :: Register -> String
regToX86 A = "r9"
regToX86 B = "r10"

toNASM :: Int -> Instruction -> String
toNASM n instruction = unlines [label n ++ ":", instToX86 n instruction]

opRegister :: String
opRegister = "r8"

instToX86 :: Int -> Instruction -> String
instToX86 _ (Half reg) =
  unlines
    [ "mov eax, " ++ regToX86 reg,
      "mov edx, 0",
      "mov "
        ++ opRegister
        ++ ", 2",
      "div " ++ opRegister,
      "mov "
        ++ regToX86 reg
        ++ ", eax"
    ]
instToX86 _ (Triple reg) =
  unlines
    [ "mov eax, " ++ regToX86 reg,
      "mov " ++ opRegister ++ ", 3",
      "mul " ++ opRegister,
      "mov "
        ++ regToX86 reg
        ++ ", eax"
    ]
instToX86 _ (Incr reg) = "inc " ++ regToX86 reg
instToX86 n (Jump offset) = "jmp " ++ label (n + offset)
instToX86 n (JumpIfEven reg offset) =
  -- from https://stackoverflow.com/a/49116885/149987
  -- the test instruction
  --
  -- test al, 1
  -- checks if the lowest bit of AL/AX/EAX/RAX is set. If it is, the number is odd.
  -- This can be checked using the Jcc instructions, especially those testing the ?ZERO flag with
  --
  -- JNZ target    ; jump if odd  = lowest bit set
  -- JZ  target    ; jump if even = lowest bit clear = zero
  unlines
    [ "test " ++ regToX86 reg ++ ", 1",
      "jz " ++ label (n + offset)
    ]
instToX86 n (JumpIfOne reg offset) =
  unlines
    [ "cmp " ++ regToX86 reg ++ ", 1",
      "je " ++ label (n + offset)
    ]

prelude :: String
prelude =
  [r|section .text
global main
extern printf

main:
  mov |]
    ++ regToX86 A
    ++ [r|, 0
  mov |]
    ++ regToX86 B
    ++ [r|, 0
|]

-- TODO: i48 is hardcoded hax
footer :: String
footer =
  [r|
i48:
end:
  mov esi, |]
    ++ regToX86 B
    ++ [r|
  mov edi, message
  mov eax, 0
  call printf
  ret

message db "%d", 10, 0
|]
