--Melissa Grueter
--Gloria Liou

module FSynF where
import Data.List

-- NOTE: Where our code is mixed in with code from the text, we have annotated
-- our additions as in the example below, to make it easier to distinguish.
-- *** CLUE: Added ...

-- Code for section 4.1
data Column = A' | B' | C' | D' | E' 
            | F' | G' | H' | I' | J'
              deriving (Eq,Ord,Show,Enum)

type Row    = Int 
type Attack = (Column,Row) 

data Ship = Battleship | Frigate | Submarine | Destroyer 
            deriving Show

data Reaction = Missed | Hit Ship | Sunk Ship | LostBattle 
                deriving Show 

type Turn = (Attack,Reaction) 

data Colour   = Red | Yellow | Blue | Green | Orange 
                deriving (Eq,Show,Bounded,Enum)

data Answer   = Black | White deriving (Eq,Show)

type Pattern  = [Colour]
type Feedback = [Answer] 

-- Code for section 4.2

-- Data types for different parts of speech
data Sent = Sent NP VP deriving Show

-- *** CLUE: Added professors, courses, and locations
data NP   = SnowWhite  | Alice  | Dorothy  | Goldilocks 
          | LittleMook | Atreyu | Everyone | Someone 
          | Bruce      | Chen   | Greenberg| Kauchak 
          | Wu         | CS52   | CS62     | CS81    
          | Systems    | Algs   | Edmunds  | Lincoln
          | Skyspace   | Frary  | Frank    | NP1 DET CN | NP2 DET RCN 
          deriving Show
data DET  = The | Every | Some | No | Most
          deriving Show

-- *** CLUE: Added Killer, Weapon, and Crimescene
data CN   = Girl   | Boy   | Princess | Dwarf | Giant 
          | Wizard | Sword | Dagger | Killer | Weapon | Crimescene deriving Show
          
data ADJ  = Fake deriving Show
data RCN  = RCN1 CN That VP | RCN2 CN That NP TV
          | RCN3 ADJ CN
          deriving Show
data That = That deriving Show
data VP   = Laughed | Cheered | Shuddered 
          | VP1 TV NP | VP2 DV NP NP
          | VP3 AV To INF -- | VP4 TV NP In NP
          deriving Show

-- *** CLUE: Added Is
data TV   = Loved   | Admired | Helped 
          | Defeated | Caught | Is
          deriving Show 

data DV   = Gave deriving Show
data AV   = Hoped | Wanted deriving Show 
data INF  = Laugh | Sheer | Shudder | INF TINF NP deriving Show
data TINF = Love | Admire | Help | Defeat | Catch 
            deriving Show 
data To   = To deriving Show

-- Section 4.4
-- Formulas of propositional logic
data Form =  P String | Ng Form | Cnj [Form] | Dsj [Form] 
            deriving Eq

instance Show Form where 
 show (P name) = name 
 show (Ng  f)  = '-': show f
 show (Cnj fs) = '&': show fs
 show (Dsj fs) = 'v': show fs

-- Sample formulas
form1, form2 :: Form
form1 = Cnj [P "p", Ng (P "p")]
form2 = Dsj [P "p1", P "p2", P "p3", P "p4"]

-- Section 4.6
-- Datatype for variables
type Name     = String 
type Index    = [Int]
data Variable = Variable Name Index deriving (Eq,Ord)

instance Show Variable where 
  show (Variable name [])  = name
  show (Variable name [i]) = name ++ show i
  show (Variable name is ) = name ++ showInts is
     where showInts []     = "" 
           showInts [i]    = show i  
           showInts (i:is) = show i ++ "_" ++ showInts is

-- sample variables
x, y, z :: Variable
x = Variable "x" []
y = Variable "y" []
z = Variable "z" []

-- Datatype for formulas of predicate logic
data Formula a = Atom String [a]
               | Eq a a
               | Neg  (Formula a)
               | Impl (Formula a) (Formula a) 
               | Equi (Formula a) (Formula a)
               | Conj [Formula a]
               | Disj [Formula a] 
               | Forall Variable (Formula a)
               | Exists Variable (Formula a)
               deriving Eq

instance Show a => Show (Formula a) where 
  show (Atom s [])   = s
  show (Atom s xs)   = s ++ show xs 
  show (Eq t1 t2)    = show t1 ++ "==" ++ show t2
  show (Neg form)    = '~' : (show form)
  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>" 
                           ++ show f2 ++ ")"
  show (Equi f1 f2)  = "(" ++ show f1 ++ "<=>" 
                           ++ show f2 ++ ")"
  show (Conj [])     = "true" 
  show (Conj fs)     = "conj" ++ show fs 
  show (Disj [])     = "false" 
  show (Disj fs)     = "disj" ++ show fs 
  show (Forall v f)  = "A " ++  show v ++ (' ' : show f)
  show (Exists v f)  = "E " ++  show v ++ (' ' : show f)

-- Sample formulas
formula0 = Atom "R" [x,y]
formula1 = Forall x (Atom "R" [x,x])
formula2 = Forall x 
            (Forall y
              (Impl (Atom "R" [x,y]) (Atom "R" [y,x])))

-- Terms of predicate logic
data Term = Var Variable | Struct String [Term] 
            deriving (Eq,Ord)

instance Show Term where 
  show (Var v)       = show v 
  show (Struct s []) = s
  show (Struct s ts) = s ++ show ts

-- Sample terms
tx, ty, tz :: Term 
tx = Var x
ty = Var y
tz = Var z

-- Is the term a variable
isVar :: Term -> Bool
isVar (Var _) = True
isVar _       = False

-- Return a list of variables in the term
varsInTerm :: Term -> [Variable]
varsInTerm (Var v)       = [v]
varsInTerm (Struct s ts) = varsInTerms ts

-- Return a list of variables in a list of terms
varsInTerms :: [Term] -> [Variable]
varsInTerms = nub . concat . map varsInTerm
