import Tetris
import Control.Monad (mapM_)
import Data.List (sort)
data Test t = Test t t deriving (Eq, Show)

main :: IO ()
main = do
  runTests spliceBoardAtTests
  runTests overlapsAtTests
  runTests blocksTests
  runTests clearLinesTests

(=?~) :: a -> a -> (Test a)
(=?~) expected expression = Test expected expression
infixr 9 =?~

runTests :: (Eq a, Show a) => [Test a] -> IO ()
runTests = mapM_ (\(Test expect expr)-> assert expect expr)
  where
    assert :: (Eq a, Show a) => a -> a -> IO ()
    assert expected got = if expected == got
                          then return ()
                          else error ("\nexpected: " ++ (show expected) ++
                                      "\ngot     : " ++ (show got) ++ "\n")

spliceBoardAtTests :: [Test Board]
spliceBoardAtTests =
  [
   board [[f, e]] =?~ spliceBoardAt (board [[e, e]])
                                           (0,0)
                                           (board [[f,e]])
  , board [[f, f]] =?~ spliceBoardAt (board [[e, f]])
                                           (0,0)
                                           (board [[f, e]])
  , board [[e, e]
          ,[e, f]
          ,[e, e]]
    =?~ spliceBoardAt (board [[e, e]
                             ,[e, e]
                             ,[e, e]
                             ])
                             (1,1)
                             (board [[f]])
  , board [[e, e, e]
          ,[e, f, f]
          ,[e, f, e]
          ]
    =?~
      spliceBoardAt (board [[e, e, e]
                           ,[e, f, e]
                           ,[e, e, e]
                           ])
                      (1,1)
                       (board [[f,f]
                              ,[f,e]])
  ]

overlapsAtTests =
  [
  True =?~ overlapsAt (board [[e]]) (-1,0) (board [[f]])
  ,True =?~ overlapsAt (board [[e]]) (0,-1) (board [[f]])
  ,True =?~ overlapsAt (board [[e, e]]) (1,0) (board [[f, f]])
  ,True =?~ overlapsAt (board [[e, e]
                              ,[e, e]]) (0,1)
                        (board [[e, f]
                               ,[e, f]])
  ,False =?~ overlapsAt (board [[e]]) (0,0) (board [[e]])
  ,False =?~ overlapsAt (board [[f]]) (0,0) (board [[e]])
  ,False =?~ overlapsAt (board [[f, f]
                              ,[e, f]])
                               (0,0)
                               (board [[e, e]
                                      ,[f, e]])
  ,True =?~ overlapsAt (board [[f]]) (0,0) (board [[f]])
  ,False =?~ overlapsAt (board [[f, f, f]
                              ,[f, e, f]
                              ,[f, f, f]
                              ]) (1,1) (board [[f]])
  ,True =?~ overlapsAt (board [[f, f, f]
                              ,[f, e, f]
                              ,[f, f, f]
                              ]) (1,2) (board [[f]])
  ]


blocksTests =
  [
   board [[f, f],[f,f]] =?~ rotated 0 (Block O)
  ,board [[f, f, f]
         ,[e, f, e]] =?~ rotated 0 (Block T)
  ,board [[f, e]
         ,[f, f]
         ,[f, e]] =?~ rotated 1 (Block T)
  ,board [[e, f, e]
         ,[f, f, f]] =?~ rotated 2 (Block T)
  ,board [[e, f]
         ,[f, f]
         ,[e ,f]] =?~ rotated 3 (Block T)
  ,rotated 4 (Block T) =?~ rotated 0 (Block T)
  ,rotated (-1) (Block T) =?~ rotated 3 (Block T)
  ]

clearLinesTests =
  [
   (1, board [[e, e]]) =?~ clearLines (board [[f, f]])
  ,(1, board [[e, e],
              [f, e]]) =?~ clearLines (board [[f, e]
                                             ,[f, f]])
  ,(2, board [[e, e],
              [e, e]]) =?~ clearLines (board [[f, f]
                                             ,[f, f]])
  ]


e = emptyCell
f = fullCell
