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

toX86 :: Register -> String
toX86 A = "ebx" -- eax and edx already used by operations
toX86 B = "ecx"

toNASM :: Int -> Instruction -> String
toNASM n instruction = unlines [label n ++ ":", body n instruction]

opRegister :: String
opRegister = "edx"

body :: Int -> Instruction -> String
body _ (Half reg) =
  unlines
    [ "mov eax, " ++ toX86 reg,
      "mov edx, 2",
      "div edx",
      "mov "
        ++ toX86 reg
        ++ ", eax"
    ]
body _ (Triple reg) =
  unlines
    [ "mov eax, " ++ toX86 reg,
      "mov edx, 3",
      "mul edx",
      "mov "
        ++ toX86 reg
        ++ ", eax"
    ]
body _ (Incr reg) = "inc " ++ toX86 reg
body n (Jump offset) = "jmp " ++ label (n + offset)
body n (JumpIfEven reg offset) =
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
    [ "test " ++ toX86 reg ++ ", 1",
      "jz " ++ label (n + offset)
    ]
body n (JumpIfOne reg offset) =
  unlines
    [ "cmp " ++ toX86 reg ++ ", 1",
      "je " ++ label (n + offset)
    ]

prelude :: String
prelude =
  [r|BITS 32;
section .text
global main
extern printf

main:
  mov eax, 0
  mov ebx, 0
|]

footer :: String
footer =
  [r|end:
  push eax
  push message
  call printf
  add esp, 8
  ret

  message db "%d", 10, 0
|]
