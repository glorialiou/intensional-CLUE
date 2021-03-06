--Melissa Grueter
--Gloria Liou

module Model where 

import Data.List

-- NOTE: Where our code is mixed in with code from the text, we have annotated
-- our additions as in the example below, to make it easier to distinguish.
-- *** CLUE: Added ...

-- Datatype for a domain of values
data Entity = A | B | C | D | E | F | G
            | H | I | J | K | L | M | N 
            | O | P | Q | R | S | T | U 
            | V | W | X | Y | Z | Unspec
     deriving (Eq,Show,Bounded,Enum)

-- A list of all the entities
entities :: [Entity]
entities =  [minBound..maxBound] 


-- constants that will be interpreted as elements of Entity
-- *** CLUE: Added constants for profs, courses, and locations
snowWhite, alice, dorothy, goldilocks, littleMook, atreyu, bruce, chen,
           greenberg, kauchak, wu, cs52, cs62, cs81, systems, algs, edmunds,
           lincoln, skyspace, frary, frank :: Entity
snowWhite  = S
alice      = A
dorothy    = D
goldilocks = G 
littleMook = M
atreyu     = Y

--Professors 
bruce = P
chen = P
greenberg = P
kauchak = P
wu = P

--Courses
cs52 = C
cs62 = C
cs81 = C
systems = C
algs = C

--Locations
edmunds = L
lincoln = L
skyspace = L
frary = L
frank = L

-- Types for predicates on the model.  Notice all are "Curried"
type OnePlacePred   = Entity -> Bool
type TwoPlacePred   = Entity -> Entity -> Bool
type ThreePlacePred = Entity -> Entity -> Entity -> Bool

-- Convert a list of entities to a characteristic function for the list
-- This makes it easy to define relations by specifying the elements of type
-- Entity that makes them true.
list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs = \ x -> elem x xs

-- Sample one-place predicates representing nouns 
girl, boy, princess, dwarf, giant, wizard, sword, dagger :: OnePlacePred

-- Define characteristic functions for each unary relation
girl     = list2OnePlacePred [S,A,D,G]
boy      = list2OnePlacePred [M,Y]
princess = list2OnePlacePred [E]
dwarf    = list2OnePlacePred [B,R]
giant    = list2OnePlacePred [T]
wizard   = list2OnePlacePred [W,V]
sword    = list2OnePlacePred [F]
dagger   = list2OnePlacePred [X]

-- Predicates defined from earlier predicates
child, person, man, woman, male, female, thing :: OnePlacePred

child  = \ x -> (girl x  || boy x)
person = \ x -> (child x || princess x || dwarf x 
                         || giant x    || wizard x) 
man    = \ x -> (dwarf x || giant x || wizard x) 
woman  = \ x -> princess x 
male   = \ x -> (man x || boy x) 
female = \ x -> (woman x || girl x)
thing  = \ x -> not (person x || x == Unspec)

-- Sample one place predicates (unary relations) representing verbs
laugh, cheer, shudder :: OnePlacePred
-- Characteristic functions for intransitive verbs
laugh   = list2OnePlacePred [A,G,E]
cheer   = list2OnePlacePred [M,D]
shudder = list2OnePlacePred [S]

-- *** CLUE: Added definitions of killer, crimescene, and weapon
killer = list2OnePlacePred [K]
crimescene = list2OnePlacePred [S]
weapon = list2OnePlacePred [W] 

-- *** CLUE: Added is
-- Sample two place predicates representing transitive verbs
love, admire, help, defeat, is :: TwoPlacePred
-- characteristic functions for binary relations for transitive verbs
love   = curry (`elem` [(Y,E),(B,S),(R,S)])
is = curry (`elem` [(x,x) | x <- entities]) 
admire = curry (`elem` [(x,G) | x <- entities, person x])
help   = curry (`elem` [(W,W),(V,V),(S,B),(D,M)])
defeat = curry (`elem` [(x,y) | x <- entities, 
                                y <- entities,
                                dwarf x && giant y]
                    ++ [(A,W),(A,V)])
        

-- Transform a function that takes three arguments into its curried form
curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x,y,z)

-- Sample three place predicates representing ditransitive verbs
give :: ThreePlacePred
give = curry3 (`elem` [(T,S,X),(A,E,S)])
-- define (Curried) characteristic function for kill
kill :: ThreePlacePred
kill = curry3 (`elem` [(Y,T,F),(Unspec,D,X),
                       (Unspec,M,Unspec)])

-- Convert a transitive verb into its passive form
-- When make passive, replae subject by unspecified.
passivize :: TwoPlacePred -> OnePlacePred
passivize r = \ x -> r Unspec x

-- Converted function to a new form where the first and second arguments
-- must be the same
self ::  (a -> a -> b) -> a -> b
self p = \ x -> p x x 
