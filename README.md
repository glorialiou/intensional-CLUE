# intensional-CLUE

Below are some suggestions for testing our code.


- To play the game, simply follow the prompts!


- To see how possible worlds are eliminated throughout the game, uncomment statements with the message "UNCOMMENT TO TEST." You may also want to open the file EAI.hs to see what the definitions of the professors/courses/locations are in each world, in order to verify the printed lists of worlds remaining. Worlds in which the definition of a professor is professor' are worlds in which that professor is the killer, and likewise for courses and locations.   


- To see how our grammar is used to check guesses using iSent, you can use the test sentences below. Replace "world" with any world from W1 to W125. To verify the correctness of the result, the definitions of the professors, courses, and locations in EAI.hs will be useful, as described in the suggestion above.  

iSent (Sent Bruce (VP1 Is (NP1 The Killer))) world
iSent (Sent Chen (VP1 Is (NP1 The Killer))) world
iSent (Sent Greenberg (VP1 Is (NP1 The Killer))) world
iSent (Sent Kauchak (VP1 Is (NP1 The Killer))) world
iSent (Sent Wu (VP1 Is (NP1 The Killer))) world
iSent (Sent CS52 (VP1 Is (NP1 The Weapon))) world
iSent (Sent CS62 (VP1 Is (NP1 The Weapon))) world
iSent (Sent CS81 (VP1 Is (NP1 The Weapon))) world
iSent (Sent Systems (VP1 Is (NP1 The Weapon))) world
iSent (Sent Algs (VP1 Is (NP1 The Weapon))) world
iSent (Sent Edmunds (VP1 Is (NP1 The Crimescene))) world
iSent (Sent Lincoln (VP1 Is (NP1 The Crimescene))) world
iSent (Sent Skyspace (VP1 Is (NP1 The Crimescene))) world
iSent (Sent Frary (VP1 Is (NP1 The Crimescene))) world
iSent (Sent Frank (VP1 Is (NP1 The Crimescene))) world


- Finally, when looking through our code, note that where our code is mixed in with code from the text, we have annotated our additions as in the example below, to make it easier to distinguish.
-- *** CLUE: Added ... 
