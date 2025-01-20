-- |

module TempRough where
import QuickSpec
import Data.Time

signatures :: [Sig]
signatures =
  [ con "0" (0 :: Int)
  , con "+" ((+) :: Int -> Int -> Int)
  , con "not" (not :: Bool -> Bool)
  , con "and" ((&&) :: Bool -> Bool -> Bool)
  , con "or" ((||) :: Bool -> Bool -> Bool)
  , template "comp-id" "?F(?G(X))=X"
  , template "op-id-elem" "?F(X,?G) = X"
  , template "op-elem-id" "?F(?G,X) = X"
  , template "commutative" "?F(X,Y) = ?F(Y,X)"
  ]

time = do
  start <- getCurrentTime
  roughSpec signatures
  qqTime <- getCurrentTime
  quickSpec signatures
  qsTime <- getCurrentTime
  print (diffUTCTime qqTime start)
  print (diffUTCTime qsTime qqTime)
