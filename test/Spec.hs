import Tetris.Board
import Control.Monad (mapM_)
import Data.List (sort)
data Test t = Test t t deriving (Eq, Show)

main :: IO ()
main = do
  runTests showBoardTests
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

showBoardTests :: [Test String]
showBoardTests =
  [
   "" =?~ showBoard (board [[]])
  ,".#.\n" =?~ showBoard (board [[empty, full, empty]])
  ,".#.\n..#\n"
    =?~
      showBoard (board [[empty, full, empty], [empty, empty, full]])
  ,".##\n.#.\n#..\n"
    =?~
      showBoard (board [[empty, full, full]
                       ,[empty, full, empty]
                       ,[full, empty, empty]
                       ])
  ]

spliceBoardAtTests :: [Test Board]
spliceBoardAtTests =
  [
   board [[full, empty]] =?~ spliceBoardAt (board [[empty, empty]])
                                           (0,0)
                                           (board [[full,empty]])
  , board [[full, full]] =?~ spliceBoardAt (board [[empty, full]])
                                           (0,0)
                                           (board [[full, empty]])
  , board [[empty, empty]
          ,[empty, full]
          ,[empty, empty]]
    =?~ spliceBoardAt (board [[empty, empty]
                             ,[empty, empty]
                             ,[empty, empty]
                             ])
                             (1,1)
                             (board [[full]])
  , board [[empty, empty, empty]
          ,[empty, full, full]
          ,[empty, full, empty]
          ]
    =?~
      spliceBoardAt (board [[empty, empty, empty]
                           ,[empty, full, empty]
                           ,[empty, empty, empty]
                           ])
                      (1,1)
                       (board [[full,full]
                              ,[full,empty]])
  ]

overlapsAtTests =
  [
  True =?~ overlapsAt (board [[empty]]) (-1,0) (board [[full]])
  ,True =?~ overlapsAt (board [[empty]]) (0,-1) (board [[full]])
  ,True =?~ overlapsAt (board [[empty, empty]]) (1,0) (board [[full, full]])
  ,True =?~ overlapsAt (board [[empty, empty]
                              ,[empty, empty]]) (0,1)
                        (board [[empty, full]
                               ,[empty, full]])
  ,False =?~ overlapsAt (board [[empty]]) (0,0) (board [[empty]])
  ,False =?~ overlapsAt (board [[full]]) (0,0) (board [[empty]])
  ,False =?~ overlapsAt (board [[full, full]
                              ,[empty, full]])
                               (0,0)
                               (board [[empty, empty]
                                      ,[full, empty]])
  ,True =?~ overlapsAt (board [[full]]) (0,0) (board [[full]])
  ,False =?~ overlapsAt (board [[full, full, full]
                              ,[full, empty, full]
                              ,[full, full, full]
                              ]) (1,1) (board [[full]])
  ,True =?~ overlapsAt (board [[full, full, full]
                              ,[full, empty, full]
                              ,[full, full, full]
                              ]) (1,2) (board [[full]])
  ]


blocksTests =
  [
   board [[full, full],[full,full]] =?~ rotated 0 (Block O)
  ,board [[full, full, full]
         ,[empty, full, empty]] =?~ rotated 0 (Block T)
  ,board [[full, empty]
         ,[full, full]
         ,[full, empty]] =?~ rotated 1 (Block T)
  ,board [[empty, full, empty]
         ,[full, full, full]] =?~ rotated 2 (Block T)
  ,board [[empty, full]
         ,[full, full]
         ,[empty ,full]] =?~ rotated 3 (Block T)
  ,rotated 4 (Block T) =?~ rotated 0 (Block T)
  ,rotated (-1) (Block T) =?~ rotated 3 (Block T)
  ]

clearLinesTests =
  [
   (1, board [[empty, empty]]) =?~ clearLines (board [[full, full]])
  ,(1, board [[empty, empty],
              [full, empty]]) =?~ clearLines (board [[full, empty]
                                                    ,[full, full]])
  ,(2, board [[empty, empty],
              [empty, empty]]) =?~ clearLines (board [[full, full]
                                                     ,[full, full]])
  ]
