--Melissa Grueter
--Gloria Liou

import Data.Word
import Data.IORef
import System.Random

data Professor = Bruce | Chen | Greenberg | Kauchak | Wu deriving Show
data Class = CS52 | CS62 | CS81 | Systems | Algs deriving Show
data Location = Beanbag | Hall | Office | Lab | Edmunds101 deriving Show

randomNumbers :: Int -> [Int]
randomNumbers seed = take 1 . randomRs (1, 5) . mkStdGen $ seed

--data World = (Professor * Class * Location) 
--genCorrectWorld

--genCorrectWorld = do

--prof = randomNumbers 47
--weapon = randomNumbers 181
--location = randomNumbers 52

genCorrectProf = case (randomNumbers 4) of
                           [1] -> Bruce
                           [2] -> Chen
                           [3] -> Greenberg
                           [4] -> Kauchak
                           [5] -> Wu
                           
genCorrectClass = case (randomNumbers 5) of
                           [1] -> CS52
                           [2] -> CS62
                           [3] -> CS81
                           [4] -> Systems
                           [5] -> Algs
                           
genCorrectLocation = case (randomNumbers 3) of
                           [1] -> Beanbag
                           [2] -> Hall
                           [3] -> Office
                           [4] -> Lab
                           [5] -> Edmunds101


--data World = World {prof :: Professor, weapon :: Class, location :: Location}

prof = genCorrectProf
weapon = genCorrectClass
location = genCorrectLocation

--genCorrectWorld = World {prof, weapon, location}
     

--from random number pick prof/class/room

--genPossibleWorlds

prompt :: IO ()
prompt = do

--thin possible worlds

 putStrLn ("Who killed the student?\nEnter 'Bruce', 'Chen', 'Greenberg', 'Kauchak', or 'Wu':")
 s <- getLine
 if s /= "bruce"
    then
        do putStrLn (""++s++" is innocent! How dare you suspect them!")
 else putStrLn ("Correct! "++s++" was the cold-blooded killer.")

 putStrLn "Which class did (s)he use?\nEnter 'CS52', 'CS62', 'CS81', 'Systems', or 'Algs':"
 s <- getLine
 if s /= "systems"
    then
        do putStrLn ("Please, "++s++" is easy! That class never killed anybody.")
 else putStrLn ("Correct! "++s++" was the murder weapon... cruel and unusual torture indeed!")

 putStrLn "Where did (s)he kill the student?\nEnter 'Beanbag', 'Hall', 'Office', 'Lab', or 'Edmunds101':"
 s <- getLine 
 if s /= "beanbag"
    then
        do putStrLn ("The "++s++"? Nothing ever happens there.")
           putStrLn ("Let's try that again. Hurry up, before another CS student gets killed.")
           prompt 
 else putStrLn ("Correct! The "++s++"... the perfect place to kill someone!")

--how do we print this one line at a time?
--make this funnier LOL
main = do
 putStrLn ("Welcome to Pomona's CS department!")
 putStrLn ("We're excited for you to join us!")
 putStrLn ("... well ... sort of ... we have way too many majors.")
 putStrLn ("Late nights, white board scribbles, flowing tears...")
 putStrLn ("nothing out of the ordinary.")
 putStrLn ("But last night, something out of the ordinary happened.")
 putStrLn ("A student was killed. The body was found right here, in the middle of Edmunds.\n")
 
 putStrLn ("Who killed the student?")
 putStrLn ("Was it...")
 putStrLn ("Professor Bruce?")
 putStrLn ("Professor Chen?")
 putStrLn ("Professor Greenberg?")
 putStrLn ("Professor Kauchak?")
 putStrLn ("Professor Wu?\n")
 
 putStrLn ("Which class did (s)he use?")
 putStrLn ("Was it...")
 putStrLn ("CS52?")
 putStrLn ("CS62?")
 putStrLn ("CS81?")
 putStrLn ("Systems?")
 putStrLn ("Algorithms?\n")

 putStrLn ("Where did (s)he kill the student?")
 putStrLn ("Was it...")
 putStrLn ("The beanbag in the lounge?")
 putStrLn ("The second floor hallway of Edmunds?")
 putStrLn ("Lori's office?")
 putStrLn ("The downstairs lab?")
 putStrLn ("Edmunds 101?\n")

 --genCorrectWorld
 --genPossibleWords
 prompt

