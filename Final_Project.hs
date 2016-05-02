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

--thin the set of possible worlds in the case of an incorrect guess
thinWorldsIncorrect :: [Char] -> [World] -> [World]
thinWorldsIncorrect guess [] = []
thinWorldsIncorrect guess (w:ws) =
                    if evalGuess guess w
                       then thinWorldsIncorrect guess ws
                    else (w:(thinWorldsIncorrect guess ws)) 

--thin the set of possible worlds in the case of a correct guess 
thinWorldsCorrect :: [Char] -> [World] -> [World]
thinWorldsCorrect guess [] = []
thinWorldsCorrect guess (w:ws) =
                  if evalGuess guess w
                     then (w:(thinWorldsCorrect guess ws))
                  else thinWorldsCorrect guess ws 
            
--background story for the game
storyLine :: IO ()
storyLine = do
   putStrLn ("Welcome to Pomona's CS department, we're excited for you to join us!")
   s <- getLine
   putStrLn ("...well...sort of...we have way too many majors.")
   s <- getLine
   putStrLn ("Late nights, white board scribbles, flowing tears...")
   s <- getLine
   putStrLn ("nothing out of the ordinary.")
   s <- getLine
   putStrLn ("But last night, something out of the ordinary happened.")
   s <- getLine
   putStrLn ("A student was killed, and the body was found here, in this very room.\n")
   s <- getLine

   putStrLn ("Who killed the student?")
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
   putStrLn ("Frank?")
   s <- getLine
   
   putStrLn ("Time is limited, students are in danger...")
   putStrLn ("A professor is on the loose!")
   s <- getLine
   putStrLn ("Are you ready to save Pomona's CS department?")

--initial prompt to start game
prompt :: World -> [World] -> Int -> Int -> IO ()
prompt c w current best = do
   putStrLn ("Enter to play!")
   s <- getLine
   guessProfessor c w current best

--guess professor
guessProfessor :: World -> [World] -> Int -> Int -> IO ()
guessProfessor c w current best = do
   putStrLn ("Who killed the student?\nEnter 'Bruce', 'Chen', 'Greenberg', 'Kauchak', or 'Wu':")
   s <- getLine
   if s /= "Bruce" && s /= "Chen" && s /= "Greenberg" && s /= "Kauchak" && s /= "Wu"
      then
         do putStrLn ("Huh? That's not a Pomona CS professor! Try again...\n")
            guessProfessor c w current best
   else if evalGuess s c
      then
         do putStrLn ("Correct! "++s++" was the cold-blooded killer.\n")
            guessWeapon c (thinWorldsCorrect s w) current best
   else 
      do putStrLn (""++s++" is innocent! How dare you suspect them!\n")
         guessWeapon c (thinWorldsIncorrect s w) current best

--guess weapon
guessWeapon :: World -> [World] -> Int -> Int -> IO ()
guessWeapon c w current best = do
   putStrLn "Which class did (s)he use?\nEnter 'CS52', 'CS62', 'CS81', 'Systems', or 'Algs':"
   s <- getLine
   if s /= "CS52" && s /= "CS62" && s /= "CS81" && s /= "Systems" && s /= "Algs"
      then
         do putStrLn ("Hm...that's not one of options! Try again...\n")
            guessWeapon c w current best
   else if evalGuess s c
      then
         do putStrLn ("Correct! "++s++" was the weapon...cruel and unusual punishment indeed!\n")
            guessLocation c (thinWorldsCorrect s w) current best
   else
      do putStrLn ("Please, "++s++" is easy! That class never killed anybody.\n")
         guessLocation c (thinWorldsIncorrect s w) current best

--guess location
guessLocation :: World -> [World] -> Int -> Int -> IO ()
guessLocation c w current best = do
   putStrLn "Where did (s)he kill the student?\nEnter 'Edmunds', 'Lincoln', 'Skyspace', 'Frary', or 'Frank':"
   s <- getLine
   if s /= "Edmunds" && s /= "Lincoln" && s /= "Skyspace" && s /= "Frary" && s /= "Frank"
      then
         do putStrLn ("Interesting choice, but not one of the locations. Try again...\n")
            guessLocation c w current best
   else if evalGuess s c
      then
         do putStrLn ("Correct! "++s++"...the perfect place to kill someone!\n")
            promptGuess c (thinWorldsCorrect s w) current best 
   else
      do putStrLn (""++s++"? Nothing ever happens there.")
         putStrLn ("Please, let's try again. Hurry up before another CS student gets killed.\n")
         promptGuess c (thinWorldsIncorrect s w) current best

--give player option to make complete guess
promptGuess :: World -> [World] -> Int -> Int -> IO ()
promptGuess c w current best = do
   putStrLn "Are you ready to make a guess?"
   putStrLn "Enter 'yes' to guess, any other key to start over:"
   s <- getLine
   if s == "yes"
      then
         do guess c w (current+1) best
   else
      do guessProfessor c w (current+1) best

--break string using delimiters
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'

--make complete guess
guess :: World -> [World] -> Int -> Int -> IO ()
guess c w current best = do
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
         do putStrLn ("You definitely didn't format that correctly. Try again...\n")
            guess c w current best
   else if evalGuess x c && evalGuess y c && evalGuess z c
      then
         do putStrLn ("CONGRATULATIONS! You saved Pomona's CS department!\n")
            score c w current best
   else if (length w == 1) --only one possible world left
      then
         do putStrLn ("It's too late...you didn't save the department in time.")
            continueGame c w current best
   else
      do putStrLn ("Not quite...\n") --CHANGE THIS
         guessProfessor c w current best

--present current score and best score
score :: World -> [World] -> Int -> Int -> IO ()
score c w current best = do
   if current < best
      then
        do putStrLn ("New best score: "++show current++" rounds")
           continueGame c w current current
   else
      do putStrLn ("Score: "++show current++", Best score: "++show best++"")
         continueGame c w current best

--final prompt to continue or end game
continueGame :: World -> [World] -> Int -> Int -> IO ()
continueGame c w current best = do
   putStrLn "Play again?"
   putStrLn "Enter 'yes' to play again, any other key to exit:"
   s <- getLine
   if s == "yes"
      then 
         do x <- getStdRandom (randomR (1, 125)) :: IO Int
            let correct = genCorrectWorld x
            prompt correct w current best
   else
      do putStrLn ("Thank you for playing!")

main = do
   storyLine
   x <- getStdRandom (randomR (1, 125)) :: IO Int
   let correct = genCorrectWorld x
   prompt correct worlds 0 125
