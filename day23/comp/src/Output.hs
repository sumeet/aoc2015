{-# LANGUAGE QuasiQuotes #-}

module Output where

import Structure (Instruction (..), Register (..))
import Text.RawString.QQ (r)

compileToNASM :: [Instruction] -> String
compileToNASM instructions = prelude ++ inner ++ footer
  where
    inner = unlines $ zipWith (toNASM totalNumInstructions) [0 ..] instructions
    totalNumInstructions = length instructions

toNASM :: Int -> Int -> Instruction -> String
toNASM totalNumInstructions n instruction =
  unlines
    [ jumpLabel totalNumInstructions n ++ ":",
      instToX86 totalNumInstructions n instruction
    ]

jumpLabel :: Int -> Int -> String
jumpLabel totalNumInstructions n =
  if n < totalNumInstructions
    then "i" ++ show n
    else endLabel

endLabel :: String
endLabel = "end"

regToX86 :: Register -> String
regToX86 A = "r9"
regToX86 B = "r10"

opRegister :: String
opRegister = "r8"

instToX86 :: Int -> Int -> Instruction -> String
instToX86 totalNumInstructions n instruction =
  let label = jumpLabel totalNumInstructions
   in case instruction of
        Half reg ->
          unlines
            [ "mov rax, " ++ regToX86 reg,
              "mov rdx, 0",
              "mov "
                ++ opRegister
                ++ ", 2",
              "div " ++ opRegister,
              "mov "
                ++ regToX86 reg
                ++ ", rax"
            ]
        Triple reg ->
          unlines
            [ "mov rax, " ++ regToX86 reg,
              "mov " ++ opRegister ++ ", 3",
              "mul " ++ opRegister,
              "mov "
                ++ regToX86 reg
                ++ ", rax"
            ]
        Incr reg -> "inc " ++ regToX86 reg
        Jump offset -> "jmp " ++ label (n + offset)
        -- from https://stackoverflow.com/a/49116885/149987
        -- the test instruction
        --
        -- test al, 1
        -- checks if the lowest bit of AL/AX/EAX/RAX is set. If it is, the number is odd.
        -- This can be checked using the Jcc instructions, especially those testing the ?ZERO flag with
        --
        -- JNZ target    ; jump if odd  = lowest bit set
        -- JZ  target    ; jump if even = lowest bit clear = zero
        JumpIfEven reg offset ->
          unlines
            [ "test " ++ regToX86 reg ++ ", 1",
              "jz " ++ label (n + offset)
            ]
        JumpIfOne reg offset ->
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

footer :: String
footer =
  "\n" ++ endLabel ++ ":"
    ++ [r|
  mov rsi, |]
    ++ regToX86 B
    ++ [r|
  mov rdi, message
  mov rax, 0
  call printf
  ret

message db "%d", 10, 0
|]
