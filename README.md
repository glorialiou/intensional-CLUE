# intensional-CLUE

Below are some suggestions for testing our code.

- To play the game, simply follow the prompts!

- To see how possible worlds are eliminated throughout the game, uncomment statements with the message "UNCOMMENT TO TEST." You may also want to open the file EAI.hs to see what the definitions of the professors/courses/locations are in each world, in order to verify the printed lists of worlds remaining. Worlds in which the definition of a professor is professor' are worlds in which that professor is the killer, and likewise for courses and locations.   

- To see how our grammar is used to check guesses using iSent, see the evalGuess function at the top of clue.hs. You could use sentences used there as test sentences if you wish to test them outside of gameplay.

- Finally, when looking through our code, note that where our code is mixed in with code from the text, we have annotated our additions as in the example below, to make it easier to distinguish.
-- *** CLUE: Added ... 
