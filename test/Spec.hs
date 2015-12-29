import Tetris.Board
import Control.Monad (mapM_)
data Test t = T t t deriving (Eq, Show)

main :: IO ()
main = do
  runTests showBoardTests
  runTests spliceBoardAtTests

(=?~) :: a -> a -> (Test a)
(=?~) expected expression = T expected expression
infixl 7 =?~

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
          ,[empty, empty, empty]
          ]
    =?~
      spliceBoardAt (board [[empty, empty, empty]
                           ,[empty, full, empty]
                           ,[empty, empty, empty]
                           ])
                      (2,2)
                       [[full,full]]
  ]
