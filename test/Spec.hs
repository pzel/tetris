import Tetris
import Control.Monad (mapM_)

main :: IO ()
main = do
  runTests rotationTests >> runTests drawTests

runTests :: (Eq a, Show a) => [(a,a)] -> IO ()
runTests = mapM_ (\(expect, expr)-> assert expect expr)
  where
    assert :: (Eq a, Show a) => a -> a -> IO ()
    assert expected got = if expected == got
                          then return ()
                          else error ("\nexpected: " ++ (show expected) ++
                                      "\ngot     : " ++ (show got) ++ "\n")
rotationTests :: [(Rotation, Rotation)]
rotationTests =
  [
   (Deg0, mkRotation 0)
  ,(Deg90, mkRotation 1)
  ,(Deg180, mkRotation 2)
  ,(Deg270, mkRotation 3)
  ,(Deg0, mkRotation 4)]

drawTests :: [(BlockDrawing, BlockDrawing)]
drawTests =
  [
   (drawBlock J, rotate (mkRotation 0) (drawBlock J))
  ,([[On, Of, Of], [On, On, On]], rotate (mkRotation 1) (drawBlock J))
  ,([[On, On], [On, Of], [On, Of]], rotate (mkRotation 2) (drawBlock J))
  ,([[On, On, On], [Of, Of, On]], rotate (mkRotation 3) (drawBlock J))
  ]
