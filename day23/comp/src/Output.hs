module Output where

import Lib (Instruction(..), Register)

output :: Instruction -> String
output (Half reg) = ""
output (Triple reg) = ""
output (Incr reg) = ""
output (Jump offset) = ""
output (JumpIfEven reg offset) = ""
output (JumpIfOne reg offset) = ""
