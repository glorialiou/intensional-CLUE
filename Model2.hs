--Melissa Grueter
--Gloria Liou

module Model2 where 
-- An alternative model for chapter 6
import Data.List
import Model

-- NOTE: Where our code is mixed in with code from the text, we have annotated
-- our additions as in the example below, to make it easier to distinguish.
-- *** CLUE: Added ...

-- *** CLUE: Added definitions of profs', courses', and locations'
snowWhite', bruce', chen', greenberg', kauchak', wu', cs52', cs62', cs81',
            systems', algs' :: Entity

snowWhite' = T

--Definitions of profs (in worlds where they are the killer, K)
bruce' = K
chen' = K
greenberg' = K 
kauchak' = K
wu' = K

--Definitions of courses (in worlds where they are the weapon, W)
cs52' = W
cs62' = W
cs81' = W
systems' = W
algs' = W

--Definitions of locations (in worlds where they are the scene of the crime, S)
edmunds' = S
lincoln' = S
skyspace' = S
frary' = S
frank' = S


girl', boy', princess', dwarf', giant', 
  child', person', man', woman', thing':: OnePlacePred
girl'     = list2OnePlacePred [S,T,A,D,G]
boy'      = list2OnePlacePred [M,Y]
princess' = list2OnePlacePred [E,S]
dwarf'    = list2OnePlacePred [B,R]
giant'    = list2OnePlacePred []
wizard'   = list2OnePlacePred [W,V]
child'    = \ x -> (girl' x  || boy' x)
person'   = \ x -> (child' x || princess' x || dwarf' x 
                             || giant' x    || wizard' x) 
man'      = \ x -> (dwarf' x || giant' x    || wizard' x) 
woman'    = \ x -> princess' x 
thing'    = \ x -> not (person' x || x == Unspec)

laugh', cheer', shudder' :: OnePlacePred
laugh'   = list2OnePlacePred [A,G,E,T]
cheer'   = list2OnePlacePred [M,D]
shudder' = list2OnePlacePred []
