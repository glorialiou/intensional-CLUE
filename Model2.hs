
module Model2 where 
-- An alternative model for chapter 6
import Data.List
import Model

snowWhite', killer, weapon, crimescene :: Entity
--, bruce, chen, greenberg, kauchak, wu, cs52, cs62, cs81, systems, algs
snowWhite' = T

--NOT CURRENTLY IN USE BUT WE SHOULD PROBABLY DO IT THIS WAY DON'T DELETE
--bruce' = K
--chen' = K
--greenberg' = K 
--kauchak' = K
--wu' = K

--cs52' = W
--cS62' = W
--cs81' = W
--systems' = W
--algs' = W

--We should come back to these and see if we want to assign entities differently 
killer = K
weapon = W
crimescene = S

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


