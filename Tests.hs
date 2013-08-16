import GCode

import System.IO

prog1 = GAssign (GCell 101) (G_Add (GCell 102) (G_Int 42))

main = do
  hPutBuilder stdout $ gopGen prog1
  