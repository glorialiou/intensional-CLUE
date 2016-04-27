--Melissa Grueter
--Gloria Liou

module CLUE where 

import Data.Word
import Data.IORef
import System.Random
import Data.Time.Clock.POSIX
import EAI
import FSynF
import Model
import Model2
import TCOM

--evaluate guesses using intensional worlds
evalGuess :: [Char] -> World -> Bool
evalGuess "Bruce" world = ((iBruce world) == killer)
evalGuess "Chen" world = ((iChen world) == killer)
evalGuess "Greenberg" world = ((iGreenberg world) == killer)
evalGuess "Kauchak" world = ((iKauchak world) == killer)
evalGuess "Wu" world = ((iWu world) == killer)
evalGuess "CS52" world = ((iCS52 world) == weapon)
evalGuess "CS62" world = ((iCS62 world) == weapon)
evalGuess "CS81" world = ((iCS81 world) == weapon)
evalGuess "Systems" world = ((iSystems world) == weapon)
evalGuess "Algs" world = ((iAlgs world) == weapon)
evalGuess "Edmunds" world = ((iEdmunds world) == crimescene)
evalGuess "Lincoln" world = ((iLincoln world) == crimescene)
evalGuess "Skyspace" world = ((iSkyspace world) == crimescene)
evalGuess "Frary" world = ((iFrary world) == crimescene)
evalGuess "Frank" world = ((iFrank world) == crimescene)

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
            print (thinWorldsCorrect s w)
            guessWeapon c (thinWorldsCorrect s w)
   else 
      do putStrLn (""++s++" is innocent! How dare you suspect them!\n")
         print (thinWorldsIncorrect s w)
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
            print (thinWorldsCorrect s w)
            guessLocation c (thinWorldsCorrect s w)
   else
      do putStrLn ("Please, "++s++" is easy! That class never killed anybody.\n")
         print (thinWorldsIncorrect s w)
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
            print (thinWorldsCorrect s w)
            promptGuess c (thinWorldsCorrect s w)   
   else
      do putStrLn (""++s++"? Nothing ever happens there.")
         putStrLn ("Let's try that again. Hurry up, before another CS student gets killed.\n")
         print (thinWorldsIncorrect s w)
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
   if evalGuess ((wordsWhen (==',') s)!!0) c &&
      evalGuess ((wordsWhen (==',') s)!!1) c &&
      evalGuess ((wordsWhen (==',') s)!!2) c
      then putStrLn ("CONGRATULATIONS! You saved Pomona's CS department!\n")
      --remainingWorlds = correctWorld
   else
      do putStrLn ("Not quite...\n")
         -- thin worlds using thinWorldsIncorrect for each of the guesses 
         prompt c w


prompt :: World -> [World] -> IO ()
prompt c w = do
   putStrLn ("Enter to play!")
   s <- getLine
   guessProfessor c w


main = do
   storyLine
   x <- getStdRandom (randomR (1, 125)) :: IO Int
   print x
   let correct = genCorrectWorld x
   print correct
   prompt correct worlds

--data Clue = Professor Professor | Class Class | Location Location deriving Show
--data Professor = Bruce | Chen | Greenberg | Kauchak | Wu deriving Show
--data Class = CS52 | CS62 | CS81 | Systems | Algs deriving Show
--data Location = Edmunds | Lincoln | Skyspace | Frary | Frank deriving Show

--rollDice :: IO Int
--rollDice = getStdRandom (randomR (1,125))

--rand =  get a random number 1-125
--rand = do
--  x <- getStdRandom (randomR (1, 125)) :: IO Int --x is an Int
--  let result = genCorrectWorld x
--  return result

--randomNumbers :: Int -> [Int]
--randomNumbers seed = take 1 . randomRs (1, 125) . mkStdGen $ seed

--random3 :: Int -> [Int]
--random3 i = let g = (mkStdGen i)
--                (n1, g1) = next g
--                (n2, g2) = next g1
--                (n3, g3) = next g2
--            in [n1,n2,n3]

--pick a world based on a number input
--genWorld 1 = W1
--genWorld 2 = W2

--We should pick a world from the set of possible worlds, i.e. W109
--genPossibleWorlds

--correct world = world chosen using random number
--correctWorld = genWorld rand
--correctWorld = W47

--genCorrectWorld _ = W47 --moved to EAI

--make seed random
--genProf = case (randomNumbers 1) of
--         [1] -> Bruce
--         [2] -> Chen
--         [3] -> Greenberg
--         [4] -> Kauchak
--         [5] -> Wu

--genClass = case (randomNumbers 1) of
--         [1] -> CS52
--         [2] -> CS62
--         [3] -> CS81
--         [4] -> Systems
--         [5] -> Algs

--genLocation = case (randomNumbers 1) of
--         [1] -> Edmunds
--         [2] -> Lincoln
--         [3] -> Skyspace
--         [4] -> Frary 
--         [5] -> Frank

--remainingworlds should be the set of worlds
--remainingWorlds = worlds
