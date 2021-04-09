module Structure where

data Register = A | B deriving (Show)

data Instruction
  = Half Register
  | Triple Register
  | Incr Register
  | Jump Int
  | JumpIfEven Register Int
  | JumpIfOne Register Int
  deriving (Show)
