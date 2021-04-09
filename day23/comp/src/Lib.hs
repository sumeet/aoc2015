{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Data.Maybe (fromJust)
import Output
import Structure (Instruction (..), Register (..))
import Text.RawString.QQ (r)

data CPU = CPU
  { a :: Int,
    b :: Int,
    instructions :: [Instruction],
    ptr :: Int
  }
  deriving (Show)

initCPU :: [Instruction] -> CPU
initCPU instructions = CPU {a = 0, b = 0, instructions, ptr = 0}

modifyReg :: Register -> CPU -> (Int -> Int) -> CPU
modifyReg reg cpu@CPU {a, b} changefunc = updateReg changefunc
  where
    updateReg changefunc = case reg of
      A -> cpu {a = changefunc a}
      B -> cpu {b = changefunc b}

getReg :: Register -> CPU -> Int
getReg A CPU {a} = a
getReg B CPU {b} = b

runAll :: CPU -> CPU
runAll cpu@CPU {ptr, instructions} = if isAtEnd then cpu else runAll $ interp cpu
  where
    -- "The program exits when it tries to run an instruction beyond the ones defined."
    isAtEnd = ptr >= length instructions

interp :: CPU -> CPU
interp cpu@CPU {instructions, ptr} = case thisInstruction of
  Half reg -> modifyReg reg next (`div` 2)
  Triple reg -> modifyReg reg next (* 3)
  Incr reg -> modifyReg reg next (+ 1)
  Jump offset -> cpu {ptr = ptr + offset}
  JumpIfEven reg offset -> if even $ getReg reg cpu then cpu {ptr = ptr + offset} else next
  JumpIfOne reg offset -> if getReg reg cpu == 1 then cpu {ptr = ptr + offset} else next
  where
    thisInstruction = instructions !! ptr
    next = cpu {ptr = ptr + 1}

parseRegister :: Char -> Maybe Register
parseRegister 'a' = Just A
parseRegister 'b' = Just B
parseRegister _ = Nothing

parseOffset :: String -> Int
parseOffset ('+' : n) = read n
parseOffset n = read n

parseInstruction :: String -> Maybe Instruction
parseInstruction ['h', 'l', 'f', ' ', reg] = Half <$> parseRegister reg
parseInstruction ['t', 'p', 'l', ' ', reg] = Triple <$> parseRegister reg
parseInstruction ['i', 'n', 'c', ' ', reg] = Incr <$> parseRegister reg
parseInstruction ('j' : 'm' : 'p' : ' ' : offset) = Just $ Jump $ parseOffset offset
parseInstruction ('j' : 'i' : 'e' : ' ' : reg : ',' : ' ' : offset) = JumpIfEven <$> parseRegister reg <*> pure (parseOffset offset)
parseInstruction ('j' : 'i' : 'o' : ' ' : reg : ',' : ' ' : offset) = JumpIfOne <$> parseRegister reg <*> pure (parseOffset offset)
parseInstruction _ = Nothing

run :: IO ()
run = putStr $ compileToNASM $ map (fromJust . parseInstruction) $ lines input

part1 :: IO ()
part1 = print $ b $ runAll cpu
  where
    cpu = initCPU $ map (fromJust . parseInstruction) $ lines input

part2 :: IO ()
part2 = print $ b $ runAll cpu {a = 1}
  where
    cpu = initCPU $ map (fromJust . parseInstruction) $ lines input

sample :: String
sample =
  [r|inc a
jio a, +2
tpl a
inc a|]

input :: String
input =
  [r|jio a, +22
inc a
tpl a
tpl a
tpl a
inc a
tpl a
inc a
tpl a
inc a
inc a
tpl a
inc a
inc a
tpl a
inc a
inc a
tpl a
inc a
inc a
tpl a
jmp +19
tpl a
tpl a
tpl a
tpl a
inc a
inc a
tpl a
inc a
tpl a
inc a
inc a
tpl a
inc a
inc a
tpl a
inc a
tpl a
tpl a
jio a, +8
inc b
jie a, +4
tpl a
inc a
jmp +2
hlf a
jmp -7|]
