import Data.Word
import Data.IORef

playAgain :: IO ()
playAgain = 
do 
  putStrLn "Who killed the student?"
  s <- getLine 
  putStrLn ("You said "++s++" killed the student.")

  putStrLn "What weapon did (s)he use?"
  s <- getLine 
  putStrLn ("You said (s)he used "++s++".")

  putStrLn "Where did (s)he kill the student?"
  s <- getLine 
  putStrLn ("You said (s)he killed the student in "++s++".")
