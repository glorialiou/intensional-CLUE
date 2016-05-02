--Melissa Grueter
--Gloria Liou

module CLUE where 

import Data.Word
import Data.IORef
import System.Random
import EAI
import FSynF
import Model
import Model2
import TCOM

--evaluate guesses using intensional worlds
evalGuess :: [Char] -> World -> Bool
evalGuess "Bruce" world = iSent (Sent Bruce (VP1 Is (NP1 The Killer))) world
evalGuess "Chen" world = iSent (Sent Chen (VP1 Is (NP1 The Killer))) world
evalGuess "Greenberg" world = iSent (Sent Greenberg (VP1 Is (NP1 The Killer))) world
evalGuess "Kauchak" world = iSent (Sent Kauchak (VP1 Is (NP1 The Killer))) world
evalGuess "Wu" world = iSent (Sent Wu (VP1 Is (NP1 The Killer))) world
evalGuess "CS52" world = iSent (Sent CS52 (VP1 Is (NP1 The Weapon))) world
evalGuess "CS62" world = iSent (Sent CS62 (VP1 Is (NP1 The Weapon))) world
evalGuess "CS81" world = iSent (Sent CS81 (VP1 Is (NP1 The Weapon))) world
evalGuess "Systems" world = iSent (Sent Systems (VP1 Is (NP1 The Weapon))) world
evalGuess "Algs" world = iSent (Sent Algs (VP1 Is (NP1 The Weapon))) world
evalGuess "Edmunds" world = iSent (Sent Edmunds (VP1 Is (NP1 The Crimescene))) world
evalGuess "Lincoln" world = iSent (Sent Lincoln (VP1 Is (NP1 The Crimescene))) world
evalGuess "Skyspace" world = iSent (Sent Skyspace (VP1 Is (NP1 The Crimescene))) world
evalGuess "Frary" world = iSent (Sent Frary (VP1 Is (NP1 The Crimescene))) world
evalGuess "Frank" world = iSent (Sent Frank (VP1 Is (NP1 The Crimescene))) world

--thins the set of worlds in the case of an incorrect guess
thinWorldsIncorrect :: [Char] -> [World] -> [World]
thinWorldsIncorrect guess [] = []
thinWorldsIncorrect guess (w:ws) =
                    if evalGuess guess w
                       then thinWorldsIncorrect guess ws
                    else (w:(thinWorldsIncorrect guess ws)) 

--thins the set of worlds in the case of a correct guess 
thinWorldsCorrect :: [Char] -> [World] -> [World]
thinWorldsCorrect guess [] = []
thinWorldsCorrect guess (w:ws) =
                  if evalGuess guess w
                     then (w:(thinWorldsCorrect guess ws))
                  else thinWorldsCorrect guess ws 
            

storyLine :: IO ()
storyLine = do
   putStrLn ("Welcome to Pomona's CS department!")
   s <- getLine
   putStrLn ("We're excited for you to join us!")
   s <- getLine
   putStrLn ("...well...sort of...we have way too many majors.")
   s <- getLine
   putStrLn ("Late nights, white board scribbles, flowing tears...")
   s <- getLine
   putStrLn ("nothing out of the ordinary.")
   s <- getLine
   putStrLn ("But last night, something out of the ordinary happened.")
   s <- getLine
   putStrLn ("A student was killed. The body was found right here, in this very room.\n")
   s <- getLine

   putStrLn ("Who killed the student?")
   s <- getLine
   putStrLn ("Was it...")
   s <- getLine
   putStrLn ("Professor Bruce?")
   s <- getLine
   putStrLn ("Professor Chen?")
   s <- getLine
   putStrLn ("Professor Greenberg?")
   s <- getLine
   putStrLn ("Professor Kauchak?")
   s <- getLine
   putStrLn ("Professor Wu?\n")
   s <- getLine

   putStrLn ("Which class did (s)he use?")
   s <- getLine
   putStrLn ("Was it...")
   s <- getLine
   putStrLn ("CS52?")
   s <- getLine
   putStrLn ("CS62?")
   s <- getLine
   putStrLn ("CS81?")
   s <- getLine
   putStrLn ("Systems?")
   s <- getLine
   putStrLn ("Algorithms?\n")
   s <- getLine

   putStrLn ("Where did (s)he kill the student?")
   s <- getLine
   putStrLn ("Was it...")
   s <- getLine
   putStrLn ("Edmunds?")
   s <- getLine
   putStrLn ("Lincoln?")
   s <- getLine
   putStrLn ("Skyspace?")
   s <- getLine
   putStrLn ("Frary?")
   s <- getLine
   putStrLn ("Frank?\n")
   s <- getLine
   putStrLn ("Help us solve the mystery!\n\n")


guessProfessor :: World -> [World] -> IO ()
guessProfessor c w = do
   putStrLn ("Who killed the student?\nEnter 'Bruce', 'Chen', 'Greenberg', 'Kauchak', or 'Wu':")
   s <- getLine
   if s /= "Bruce" && s /= "Chen" && s /= "Greenberg" && s /= "Kauchak" && s /= "Wu"
      then
         do putStrLn ("Huh? That's not a Pomona CS professor! Try again...\n")
            guessProfessor c w
   else if evalGuess s c
      then
         do putStrLn ("Correct! "++s++" was the cold-blooded killer.\n")
            guessWeapon c (thinWorldsCorrect s w)
   else 
      do putStrLn (""++s++" is innocent! How dare you suspect them!\n")
         guessWeapon c (thinWorldsIncorrect s w)


guessWeapon :: World -> [World] -> IO ()
guessWeapon c w = do
   putStrLn "Which class did (s)he use?\nEnter 'CS52', 'CS62', 'CS81', 'Systems', or 'Algs':"
   s <- getLine
   if s /= "CS52" && s /= "CS62" && s /= "CS81" && s /= "Systems" && s /= "Algs"
      then
         do putStrLn ("Possibly...but that's not one of options! Try again...\n")
            guessWeapon c w
   else if evalGuess s c
      then
         do putStrLn ("Correct! "++s++" was the murder weapon...cruel and unusual punishment indeed!\n")
            guessLocation c (thinWorldsCorrect s w)
   else
      do putStrLn ("Please, "++s++" is easy! That class never killed anybody.\n")
         guessLocation c (thinWorldsIncorrect s w)


guessLocation :: World -> [World] -> IO ()
guessLocation c w = do
   putStrLn "Where did (s)he kill the student?\nEnter 'Edmunds', 'Lincoln', 'Skyspace', 'Frary', or 'Frank':"
   s <- getLine
   if s /= "Edmunds" && s /= "Lincoln" && s /= "Skyspace" && s /= "Frary" && s /= "Frank"
      then
         do putStrLn ("Interesting choice, but not one of the locations. Try again...\n")
            guessLocation c w
   else if evalGuess s c
      then
         do putStrLn ("Correct! The "++s++"...the perfect place to kill someone!\n")
            promptGuess c (thinWorldsCorrect s w)   
   else
      do putStrLn (""++s++"? Nothing ever happens there.")
         putStrLn ("Let's try that again. Hurry up, before another CS student gets killed.\n")
         promptGuess c (thinWorldsIncorrect s w)


promptGuess :: World -> [World] -> IO ()
promptGuess c w = do
   putStrLn "Are you ready to solve the mystery?"
   putStrLn "Enter 'yes' to guess, any other key to start over:"
   s <- getLine
   if (s == "yes")
      then
         do guess c w
   else
      do prompt c w

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'

guess :: World -> [World] -> IO ()
guess c w = do
   putStrLn "Who killed the student, what did (s)he use, and where did it happen?"
   putStrLn "Format: professor,class,location (no spaces)"
   s <- getLine
   let x = (wordsWhen (==',') s)!!0
   let y = (wordsWhen (==',') s)!!1
   let z = (wordsWhen (==',') s)!!2
   if (x /= "Bruce" && x /= "Chen" && x /= "Greenberg" && x /= "Kauchak" && x /= "Wu") ||
      (y /= "CS52" && y /= "CS62" && y /= "CS81" && y /= "Systems" && y /= "Algs") ||
      (z /= "Edmunds" && z /= "Lincoln" && z /= "Skyspace" && z /= "Frary" && z /= "Frank")
      then
         do putStrLn ("You definitely didn't format that correctly.\n")
            guess c w
   else if evalGuess x c && evalGuess y c && evalGuess z c
      then putStrLn ("CONGRATULATIONS! You saved Pomona's CS department!\n")
   else if (length w == 1)
      then putStrLn ("It's too late...you didn't catch "++x++" in time.")
   else
      do putStrLn ("Not quite...\n")
         -- thin worlds using thinWorldsIncorrect (or Correct) for each of the guesses
         prompt c w


prompt :: World -> [World] -> IO ()
prompt c w = do
   putStrLn ("Enter to play!")
   s <- getLine
   guessProfessor c w


main = do
   --storyLine
   x <- getStdRandom (randomR (1, 125)) :: IO Int
   --print x
   let correct = genCorrectWorld x
   --print correct
   prompt correct worlds
