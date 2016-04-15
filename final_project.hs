import Data.Word
import Data.IORef

prompt :: IO ()
prompt = do 
 putStrLn ("Who killed the student?\nEnter 'bruce', 'chen', 'greenberg', 'kauchak', or 'wu':")
 s <- getLine 
 putStrLn ("You said "++s++" killed the student.")

 putStrLn "Which class did (s)he use?\nEnter '52', '62', '81', 'systems', or 'algs':"
 s <- getLine 
 putStrLn ("You said (s)he used "++s++".")

 putStrLn "Where did (s)he kill the student?\nEnter 'beanbag', 'hall', 'office', 'lab', or 'edmunds101':"
 s <- getLine 
 putStrLn ("You said (s)he killed the student in "++s++".")

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
 
 prompt
