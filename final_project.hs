--Melissa Grueter
--Gloria Liou

import Data.Word
import Data.IORef
import System.Random

data Clue = Professor Professor | Class Class | Location Location deriving Show
data Professor = Bruce | Chen | Greenberg | Kauchak | Wu deriving Show
data Class = CS52 | CS62 | CS81 | Systems | Algs deriving Show
data Location = Beanbag | Hall | Office | Lab | Edmunds101 deriving Show
data World = World {p :: Professor, c :: Class, l :: Location} deriving Show

randomNumbers :: Int -> [Int]
randomNumbers seed = take 1 . randomRs (1, 5) . mkStdGen $ seed

--make seed random
genProf = case (randomNumbers 1) of
         [1] -> Bruce
         [2] -> Chen
         [3] -> Greenberg
         [4] -> Kauchak
         [5] -> Wu

genClass = case (randomNumbers 1) of
         [1] -> CS52
         [2] -> CS62
         [3] -> CS81
         [4] -> Systems
         [5] -> Algs

genLocation = case (randomNumbers 1) of
         [1] -> Beanbag
         [2] -> Hall
         [3] -> Office
         [4] -> Lab
         [5] -> Edmunds101

genWorld = World {p = genProf, c = genClass, l = genLocation}

worldToString :: World -> [String]
worldToString (World {p = prof, c = clas, l = loca}) = [show prof, show clas, show loca]

--genPossibleWorlds

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
   putStrLn ("A student was killed. The body was found right here, in the middle of Edmunds.\n")
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
   putStrLn ("The beanbag in the lounge?")
   s <- getLine
   putStrLn ("The second floor hallway of Edmunds?")
   s <- getLine
   putStrLn ("Lori's office?")
   s <- getLine
   putStrLn ("The downstairs lab?")
   s <- getLine
   putStrLn ("Edmunds 101?\n")
   s <- getLine
   putStrLn ("Help us solve the mystery!\n\n")

professor :: IO ()
professor = do
   putStrLn ("Who killed the student?\nEnter 'Bruce', 'Chen', 'Greenberg', 'Kauchak', or 'Wu':")
   s <- getLine
   if s /= "Bruce" && s /= "Chen" && s /= "Greenberg" && s /= "Kauchak" && s /= "Wu"
      then
         do putStrLn ("Huh? That's not a Pomona CS professor! Try again...\n")
            professor
   else if s /= worldToString(genWorld)!!0
      then putStrLn (""++s++" is innocent! How dare you suspect them!\n")
   else putStrLn ("Correct! "++s++" was the cold-blooded killer.\n")

weapon :: IO ()
weapon = do
   putStrLn "Which class did (s)he use?\nEnter 'CS52', 'CS62', 'CS81', 'Systems', or 'Algs':"
   s <- getLine
   if s /= "CS52" && s /= "CS62" && s /= "CS81" && s /= "Systems" && s /= "Algs"
      then
         do putStrLn ("Possibly...but that's not one of options! Try again...\n")
            weapon
   else if s /= worldToString(genWorld)!!1
      then putStrLn ("Please, "++s++" is easy! That class never killed anybody.\n")
   else putStrLn ("Correct! "++s++" was the murder weapon...cruel and unusual torture indeed!\n")

location :: IO ()
location = do
   putStrLn "Where did (s)he kill the student?\nEnter 'Beanbag', 'Hall', 'Office', 'Lab', or 'Edmunds101':"
   s <- getLine
   if s /= "Beanbag" && s /= "Hall" && s /= "Office" && s /= "Lab" && s /= "Edmunds101"
      then
         do putStrLn ("Interesting choice, but not one of the locations. Try again...\n")
            location
   else if s /= worldToString(genWorld)!!2
      then
         do putStrLn ("The "++s++"? Nothing ever happens there.")
            putStrLn ("Let's try that again. Hurry up, before another CS student gets killed.\n")
   else putStrLn ("Correct! The "++s++"...the perfect place to kill someone!\n")

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'

promptGuess :: IO ()
promptGuess = do
   putStrLn "Are you ready to solve the mystery?"
   putStrLn "Enter 'yes' to guess, any other key to start over:"
   s <- getLine
   if (s == "yes")
      then
         do guess
   else
      do prompt

guess :: IO ()
guess = do
   putStrLn "Who killed the student, what did (s)he use, and where did it happen?"
   putStrLn "Format: professor,class,location: (no spaces)"
   s <- getLine
   if (wordsWhen (==',') s)!!0 == worldToString(genWorld)!!0 &&
      (wordsWhen (==',') s)!!1 == worldToString(genWorld)!!1 &&
      (wordsWhen (==',') s)!!2 == worldToString(genWorld)!!2
      then putStrLn ("CONGRATULATIONS! You saved Pomona's CS department!\n")
   else
      do putStrLn ("Not quite...\n")
         prompt

--thin possible worlds
prompt :: IO ()
prompt = do
   s <- getLine
   professor
   weapon
   location
   promptGuess


main = do
   storyLine
   --genPossibleWords
   prompt
