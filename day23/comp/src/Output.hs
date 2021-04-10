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
toX86 A = "r9"
toX86 B = "r10"

toNASM :: Int -> Instruction -> String
toNASM n instruction = unlines [label n ++ ":", body n instruction]

opRegister :: String
opRegister = "r8"

body :: Int -> Instruction -> String
body _ (Half reg) =
  unlines
    [ "mov eax, " ++ toX86 reg,
      "mov " ++ opRegister ++ ", 2",
      "div " ++ opRegister,
      "mov "
        ++ toX86 reg
        ++ ", eax"
    ]
body _ (Triple reg) =
  unlines
    [ "mov eax, " ++ toX86 reg,
      "mov " ++ opRegister ++ ", 3",
      "mul " ++ opRegister,
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
  [r|section .text
global main
extern printf

main:
  mov |]
    ++ toX86 A
    ++ [r|, 0
  mov |]
    ++ toX86 B
    ++ [r|, 0
|]

-- TODO: i48 is hardcoded hax
footer :: String
footer =
  [r|
i48:
end:
  mov esi, |]
    ++ toX86 B
    ++ [r|
  mov edi, message
  mov eax, 0
  call printf
  ret

message db "%d", 10, 0
|]
