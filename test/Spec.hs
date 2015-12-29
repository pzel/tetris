import Tetris.Board
import Control.Monad (mapM_)
import Data.List (sort)
data Test t = T t t deriving (Eq, Show)

main :: IO ()
main = do
  runTests showBoardTests
  runTests spliceBoardAtTests
  runTests overlapsAtTests

(=?~) :: a -> a -> (Test a)
(=?~) expected expression = T expected expression
infixr 9 =?~

runTests :: (Eq a, Show a) => [Test a] -> IO ()
runTests = mapM_ (\(T expect expr)-> assert expect expr)
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
