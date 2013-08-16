import GOperator

import Data.ByteString.Lazy.Char8 as S
import Blaze.ByteString.Builder

prog1 = GAssign (GCell 101) (G_Add (G_Read $ GCell 102) (G_Int 42))

main = do
  S.putStrLn $ toLazyByteString $ gopGen prog1
