import Tetris
import Control.Monad (mapM_)
type Test t = (t,t)

main :: IO ()
main = do
  runTests rotationTests

runTests :: (Eq a, Show a) => [(a,a)] -> IO ()
runTests = mapM_ (\(expect, expr)-> assert expect expr)
  where
    assert :: (Eq a, Show a) => a -> a -> IO ()
    assert expected got = if expected == got
                          then return ()
                          else error ("\nexpected: " ++ (show expected) ++
                                      "\ngot     : " ++ (show got) ++ "\n")
rotationTests :: [Test BlockDrawing]
rotationTests =
  [
   (drawBlock J, rotate 0 J)
  ,([[On, On, On], [Of, Of, On]], rotate 1 J)
  ,([[On, On], [On, Of], [On, Of]], rotate 2 J)
  ,([[On, Of, Of], [On, On, On]], rotate 3 J)
  ,(drawBlock J, rotate 4 J)
  ,(rotate (-1) J, rotate 3 J)
  ,(rotate (-2) J, rotate 2 J)
  ,(rotate (-3) J, rotate 1 J)
  ,(rotate (-4) J, rotate 0 J)
  ,(rotate (-8) J, rotate 8 J)
  ]
