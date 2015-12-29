import Tetris.Board
import Control.Monad (mapM_)
import Data.List (sort)
data Test t = T t t deriving (Eq, Show)

main :: IO ()
main = do
  runTests showBoardTests
  runTests offsetTests
  runTests spliceBoardAtTests


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
  ," # \n" =?~ showBoard (board [[empty, full, empty]])
  ," # \n  #\n"
    =?~
      showBoard (board [[empty, full, empty], [empty, empty, full]])
  ," ##\n # \n#  \n"
    =?~
      showBoard (board [[empty, full, full]
                       ,[empty, full, empty]
                       ,[full, empty, empty]
                       ])
  ]

offsetTests :: [Test [((Int,Int),Char)]]
offsetTests =
  [
   [((1,1), 'a')] =?~ offsetPairs (1,1) [['a']]
  ,[((1,1), 'a'), ((1,2), 'b')] =?~ offsetPairs (1,1) [['a','b']]
  ,[((1,1), 'a'), ((1,2), 'b'),((2,1),'c'),((2,2),'d')]
    =?~ sort (offsetPairs (1,1) [['a','b'],['c','d']])
  ]

spliceBoardAtTests :: [Test Board]
spliceBoardAtTests =
  [
   board [[full, empty]] =?~ spliceBoardAt (board [[empty, empty]])
                                           (1,1)
                                           [[full,empty]]
  , board [[empty, empty]
          ,[empty, full]
          ,[empty, empty]
          ]
    =?~
      spliceBoardAt (board [[empty, empty]
                           ,[empty, empty]
                           ,[empty, empty]
                           ])
                      (2,2)
                      [[full]]
  , board [[empty, empty, empty]
          ,[empty, full, full]
          ,[empty, full, empty]
          ]
    =?~
      spliceBoardAt (board [[empty, empty, empty]
                           ,[empty, full, empty]
                           ,[empty, empty, empty]
                           ])
                      (2,2)
                       [[full,full]
                       ,[full,empty]
                       ]
  ]
