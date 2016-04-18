module EAI where 
import FSynF
import Model
import Model2
import TCOM

data World = W1 | W2 | W3 | W4 | W5 | W6 | W7 | W8 | W9 | W10
    W11 | W12 | W13 | W14 | W15 | W16 | W17 | W18 | W19 | W20
    W21 | W22 | W23 | W24 | W25 | W26 | W27 | W28 | W29 | W30
    W31 | W32 | W33 | W34 | W35 | W36 | W37 | W38 | W39 | W40
    W41 | W42 | W43 | W44 | W45 | W46 | W47 | W48 | W49 | W50
    W51 | W52 | W53 | W54 | W55 | W56 | W57 | W58 | W59 | W60
    W61 | W62 | W63 | W64 | W65 | W66 | W67 | W68 | W69 | W70
    W71 | W72 | W73 | W74 | W75 | W76 | W77 | W78 | W79 | W80
    W81 | W82 | W83 | W84 | W85 | W86 | W87 | W88 | W89 | W90
    W91 | W92 | W93 | W94 | W95 | W96 | W97 | W98 | W99 | W100
    W101 | W102 | W103 | W104 | W105 | W106 | W107 | W108 | W109 | W110
    W111 | W112 | W113 | W114 | W115 | W116 | W117 | W118 | W119 | W120
    W121 | W122 | W123 | W124 | W125 deriving (Eq,Show)

--We will need to thin these out
worlds :: [World]
worlds = [W1,W2,W3,W4,W5,W6,W7,W8,W9,W10,
         W11,W12,W13,W14,W15,W16,W17,W18,W19,W20,
         W21,W22,W23,W24,W25,W26,W27,W28,W29,W30,
         W31,W32,W33,W34,W35,W36,W37,W38,W39,W40,
         W41,W42,W43,W44,W45,W46,W47,W48,W49,W50,
         W51,W52,W53,W54,W55,W56,W57,W58,W59,W60,
         W61,W62,W63,W64,W65,W66,W67,W68,W69,W70,
         W71,W72,W73,W74,W75,W76,W77,W78,W79,W80,
         W81,W82,W83,W84,W85,W86,W87,W88,W89,W90,
         W91,W92,W93,W94,W95,W96,W97,W98,W99,W100,
         W101,W102,W103,W104,W105,W106,W107,W108,W109,W110,
         W111,W112,W113,W114,W115,W116,W117,W118,W119,W120,
         W121,W122,W123,W124,W125]

--  -- PROFESSORS -- -- 
--Prof Bruce
iBruce :: IEntity
iBruce W1 = killer
iBruce W2 = killer
iBruce W3 = killer
iBruce W4 = killer
iBruce W5 = killer
iBruce W6 = killer
iBruce W7 = killer
iBruce W8 = killer
iBruce W9 = killer
iBruce W10 = killer
iBruce W11 = killer
iBruce W12 = killer
iBruce W13 = killer
iBruce W14 = killer
iBruce W15 = killer
iBruce W16 = killer
iBruce W17 = killer
iBruce W18 = killer
iBruce W19 = killer
iBruce W20 = killer
iBruce W21 = killer
iBruce W22 = killer
iBruce W23 = killer
iBruce W24 = killer
iBruce W25 = killer
iBruce _ = professor

--Prof Chen
iChen :: IEntity
iChen W26 = killer
iChen W27 = killer
iChen W28 = killer
iChen W29 = killer
iChen W30 = killer
iChen W31 = killer
iChen W32 = killer
iChen W33 = killer
iChen W34 = killer
iChen W35 = killer
iChen W36 = killer
iChen W37 = killer
iChen W38 = killer
iChen W39 = killer
iChen W40 = killer
iChen W41 = killer
iChen W42 = killer
iChen W43 = killer
iChen W44 = killer
iChen W45 = killer
iChen W46 = killer
iChen W47 = killer
iChen W48 = killer
iChen W49 = killer
iChen W50 = killer
iChen _ = professor

--Greenberg
iGreenberg :: IEntity
iGreenberg W51 = killer
iGreenberg W52 = killer
iGreenberg W53 = killer
iGreenberg W54 = killer
iGreenberg W55 = killer
iGreenberg W56 = killer
iGreenberg W57 = killer
iGreenberg W58 = killer
iGreenberg W59 = killer
iGreenberg W60 = killer
iGreenberg W61 = killer
iGreenberg W62 = killer
iGreenberg W63 = killer
iGreenberg W64 = killer
iGreenberg W65 = killer
iGreenberg W66 = killer
iGreenberg W67 = killer
iGreenberg W68 = killer
iGreenberg W69 = killer
iGreenberg W70 = killer
iGreenberg W71 = killer
iGreenberg W72 = killer
iGreenberg W73 = killer
iGreenberg W74 = killer
iGreenberg W75 = killer
iGreenberg _ = professor

--Kauchak
iKauchak :: IEntity
iKauchak W76 = killer
iKauchak W77 = killer
iKauchak W78 = killer
iKauchak W79 = killer
iKauchak W80 = killer
iKauchak W81 = killer
iKauchak W82 = killer
iKauchak W83 = killer
iKauchak W84 = killer
iKauchak W85 = killer
iKauchak W86 = killer
iKauchak W87 = killer
iKauchak W88 = killer
iKauchak W89 = killer
iKauchak W90 = killer
iKauchak W91 = killer
iKauchak W92 = killer
iKauchak W93 = killer
iKauchak W94 = killer
iKauchak W95 = killer
iKauchak W96 = killer
iKauchak W97 = killer
iKauchak W98 = killer
iKauchak W99 = killer
iKauchak W100 = killer
iKauchak _ = professor

--Wu
iWu :: IEntity
iWu W101 = killer
iWu W102 = killer
iWu W103 = killer
iWu W104 = killer
iWu W105 = killer
iWu W106 = killer
iWu W107 = killer
iWu W108 = killer
iWu W109 = killer
iWu W110 = killer
iWu W111 = killer
iWu W112 = killer
iWu W113 = killer
iWu W114 = killer
iWu W115 = killer
iWu W116 = killer
iWu W117 = killer
iWu W118 = killer
iWu W119 = killer
iWu W120 = killer
iWu W121 = killer
iWu W122 = killer
iWu W123 = killer
iWu W124 = killer
iWu W125 = killer
iWu _ = professor

-- -- CLASSES -- --
--CS 52
iCS52 :: IEntity
iCS52 W1 = weapon 
iCS52 W2 = weapon
iCS52 W3 = weapon
iCS52 W4 = weapon
iCS52 W5 = weapon
iCS52 W6 = weapon
iCS52 W7 = weapon
iCS52 W8 = weapon
iCS52 W9 = weapon
iCS52 W10 = weapon
iCS52 W11 = weapon
iCS52 W12 = weapon
iCS52 W13 = weapon
iCS52 W14 = weapon
iCS52 W15 = weapon
iCS52 W16 = weapon
iCS52 W17 = weapon
iCS52 W18 = weapon
iCS52 W19 = weapon
iCS52 W20 = weapon
iCS52 W21 = weapon
iCS52 W22 = weapon
iCS52 W23 = weapon
iCS52 W24 = weapon
iCS52 W25 = weapon
iCS52 _ = course

--CS 62
iCS62 :: IEntity
iCS62 W26 = weapon
iCS62 W27 = weapon
iCS62 W28 = weapon
iCS62 W29 = weapon
iCS62 W30 = weapon
iCS62 W31 = weapon
iCS62 W32 = weapon
iCS62 W33 = weapon
iCS62 W34 = weapon
iCS62 W35 = weapon
iCS62 W36 = weapon
iCS62 W37 = weapon
iCS62 W38 = weapon
iCS62 W39 = weapon
iCS62 W40 = weapon
iCS62 W41 = weapon
iCS62 W42 = weapon
iCS62 W43 = weapon
iCS62 W44 = weapon
iCS62 W45 = weapon
iCS62 W46 = weapon
iCS62 W47 = weapon
iCS62 W48 = weapon
iCS62 W49 = weapon
iCS62 W50 = weapon
iCS62 _ = course

--CS 81
iCS81 :: IEntity
iCS81 W51 = weapon
iCS81 W52 = weapon
iCS81 W53 = weapon
iCS81 W54 = weapon
iCS81 W55 = weapon
iCS81 W56 = weapon
iCS81 W57 = weapon
iCS81 W58 = weapon
iCS81 W59 = weapon
iCS81 W60 = weapon
iCS81 W61 = weapon
iCS81 W62 = weapon
iCS81 W63 = weapon
iCS81 W64 = weapon
iCS81 W65 = weapon
iCS81 W66 = weapon
iCS81 W67 = weapon
iCS81 W68 = weapon
iCS81 W69 = weapon
iCS81 W70 = weapon
iCS81 W71 = weapon
iCS81 W72 = weapon
iCS81 W73 = weapon
iCS81 W74 = weapon
iCS81 W75 = weapon
iCS81 _ = course

--Systems
iSystems :: IEntity
iSystems W76 = weapon
iSystems W77 = weapon
iSystems W78 = weapon
iSystems W79 = weapon
iSystems W80 = weapon
iSystems W81 = weapon
iSystems W82 = weapon
iSystems W83 = weapon
iSystems W84 = weapon
iSystems W85 = weapon
iSystems W86 = weapon
iSystems W87 = weapon
iSystems W88 = weapon
iSystems W89 = weapon
iSystems W90 = weapon
iSystems W91 = weapon
iSystems W92 = weapon
iSystems W93 = weapon
iSystems W94 = weapon
iSystems W95 = weapon
iSystems W96 = weapon
iSystems W97 = weapon
iSystems W98 = weapon
iSystems W99 = weapon
iSystems W100 = weapon
iSystems _ = course

--Algs
iAlgs :: IEntity
iAlgs W101 = weapon
iAlgs W102 = weapon
iAlgs W103 = weapon
iAlgs W104 = weapon
iAlgs W105 = weapon
iAlgs W106 = weapon
iAlgs W107 = weapon
iAlgs W108 = weapon
iAlgs W109 = weapon
iAlgs W110 = weapon
iAlgs W111 = weapon
iAlgs W112 = weapon
iAlgs W113 = weapon
iAlgs W114 = weapon
iAlgs W115 = weapon
iAlgs W116 = weapon
iAlgs W117 = weapon
iAlgs W118 = weapon
iAlgs W119 = weapon
iAlgs W120 = weapon
iAlgs W121 = weapon
iAlgs W122 = weapon
iAlgs W123 = weapon
iAlgs W124 = weapon
iAlgs W125 = weapon
iAlgs _ = course

-- -- LOCATIONS -- --

--Beanbag
iBeanbag :: IEntity
iBeanbag W1 = crimeScene 
iBeanbag W2 = crimeScene
iBeanbag W3 = crimeScene
iBeanbag W4 = crimeScene
iBeanbag W5 = crimeScene
iBeanbag W6 = crimeScene
iBeanbag W7 = crimeScene
iBeanbag W8 = crimeScene
iBeanbag W9 = crimeScene
iBeanbag W10 = crimeScene
iBeanbag W11 = crimeScene
iBeanbag W12 = crimeScene
iBeanbag W13 = crimeScene
iBeanbag W14 = crimeScene
iBeanbag W15 = crimeScene
iBeanbag W16 = crimeScene
iBeanbag W17 = crimeScene
iBeanbag W18 = crimeScene
iBeanbag W19 = crimeScene
iBeanbag W20 = crimeScene
iBeanbag W21 = crimeScene
iBeanbag W22 = crimeScene
iBeanbag W23 = crimeScene
iBeanbag W24 = crimeScene
iBeanbag W25 = crimeScene
iBeanbag _ = location

--Hall
iHall :: IEntity
iHall W26 = crimeScene
iHall W27 = crimeScene
iHall W28 = crimeScene
iHall W29 = crimeScene
iHall W30 = crimeScene
iHall W31 = crimeScene
iHall W32 = crimeScene
iHall W33 = crimeScene
iHall W34 = crimeScene
iHall W35 = crimeScene
iHall W36 = crimeScene
iHall W37 = crimeScene
iHall W38 = crimeScene
iHall W39 = crimeScene
iHall W40 = crimeScene
iHall W41 = crimeScene
iHall W42 = crimeScene
iHall W43 = crimeScene
iHall W44 = crimeScene
iHall W45 = crimeScene
iHall W46 = crimeScene
iHall W47 = crimeScene
iHall W48 = crimeScene
iHall W49 = crimeScene
iHall W50 = crimeScene
iHall _ = location

--Office
iOffice :: IEntity
iOffice W51 = crimeScene
iOffice W52 = crimeScene
iOffice W53 = crimeScene
iOffice W54 = crimeScene
iOffice W55 = crimeScene
iOffice W56 = crimeScene
iOffice W57 = crimeScene
iOffice W58 = crimeScene
iOffice W59 = crimeScene
iOffice W60 = crimeScene
iOffice W61 = crimeScene
iOffice W62 = crimeScene
iOffice W63 = crimeScene
iOffice W64 = crimeScene
iOffice W65 = crimeScene
iOffice W66 = crimeScene
iOffice W67 = crimeScene
iOffice W68 = crimeScene
iOffice W69 = crimeScene
iOffice W70 = crimeScene
iOffice W71 = crimeScene
iOffice W72 = crimeScene
iOffice W73 = crimeScene
iOffice W74 = crimeScene
iOffice W75 = crimeScene
iOffice _ = location

--Lab
iLab :: IEntity
iLab W76 = crimeScene
iLab W77 = crimeScene
iLab W78 = crimeScene
iLab W79 = crimeScene
iLab W80 = crimeScene
iLab W81 = crimeScene
iLab W82 = crimeScene
iLab W83 = crimeScene
iLab W84 = crimeScene
iLab W85 = crimeScene
iLab W86 = crimeScene
iLab W87 = crimeScene
iLab W88 = crimeScene
iLab W89 = crimeScene
iLab W90 = crimeScene
iLab W91 = crimeScene
iLab W92 = crimeScene
iLab W93 = crimeScene
iLab W94 = crimeScene
iLab W95 = crimeScene
iLab W96 = crimeScene
iLab W97 = crimeScene
iLab W98 = crimeScene
iLab W99 = crimeScene
iLab W100 = crimeScene
iLab _ = location

--Edmunds 101
iEdmunds101 :: IEntity
iEdmunds101 W101 = crimeScene
iEdmunds101 W102 = crimeScene
iEdmunds101 W103 = crimeScene
iEdmunds101 W104 = crimeScene
iEdmunds101 W105 = crimeScene
iEdmunds101 W106 = crimeScene
iEdmunds101 W107 = crimeScene
iEdmunds101 W108 = crimeScene
iEdmunds101 W109 = crimeScene
iEdmunds101 W110 = crimeScene
iEdmunds101 W111 = crimeScene
iEdmunds101 W112 = crimeScene
iEdmunds101 W113 = crimeScene
iEdmunds101 W114 = crimeScene
iEdmunds101 W115 = crimeScene
iEdmunds101 W116 = crimeScene
iEdmunds101 W117 = crimeScene
iEdmunds101 W118 = crimeScene
iEdmunds101 W119 = crimeScene
iEdmunds101 W120 = crimeScene
iEdmunds101 W121 = crimeScene
iEdmunds101 W122 = crimeScene
iEdmunds101 W123 = crimeScene
iEdmunds101 W124 = crimeScene
iEdmunds101 W125 = crimeScene
iEdmunds101 _ = location

--------------------------- OLD CODE FOR REFERENCE -----------------------------

type IEntity = World -> Entity
type IBool   = World -> Bool
iSnowWhite :: IEntity
iSnowWhite W1 = snowWhite
iSnowWhite W2 = snowWhite'
iSnowWhite W3 = snowWhite'

iGirl, iPrincess, iPerson :: World -> Entity -> Bool
iGirl     W1 = girl
iGirl     W2 = girl'
iGirl     W3 = girl' 
iPrincess W1 = princess
iPrincess W2 = princess'
iPrincess W3 = girl'
iPerson   W1 = person
iPerson   W2 = person'
iPerson   W3 = person'

iLaugh, iShudder :: World -> Entity -> Bool
iLaugh W1 =  laugh 
iLaugh W2 =  laugh'  
iLaugh W3 =  laugh' 
iShudder W1 =  shudder 
iShudder W2 =  shudder' 
iShudder W3 =  shudder' 

iCatch :: World -> Entity -> Entity -> Bool
iCatch W1 = \ x y -> False
iCatch W2 = \ x y -> False
iCatch W3 = \ x y -> elem x [B,R,T] && girl' y

iSent :: Sent -> IBool
iSent (Sent np vp) = iNP np (iVP vp)

iNP :: NP -> (IEntity -> IBool) -> IBool

iNP SnowWhite = \ p -> p iSnowWhite

iNP Everyone  = \ p i -> all (\x -> p (\j -> x) i) 
      (filter (\y -> iPerson i y) entities)
iNP Someone  = \ p i -> any (\x -> p (\j -> x) i) 
      (filter (\y -> iPerson i y) entities)
iNP (NP1 det cn)  = iDET det (iCN cn) 
iNP (NP2 det rcn) = iDET det (iRCN rcn) 

iDET :: DET -> (IEntity -> IBool) 
            -> (IEntity -> IBool) -> IBool
iDET Some p q = \ i -> any (\x -> q (\j -> x) i) 
      (filter (\x -> p (\j -> x) i) entities)
iDET Every p q = \ i -> all (\x -> q (\j -> x) i) 
      (filter (\x -> p (\j -> x) i) entities)
iDET No p q = \ i -> not (any (\x -> q (\j -> x) i) 
      (filter (\x -> p (\j -> x) i) entities))

iVP :: VP -> IEntity -> IBool
iVP Laughed   = \ x i -> iLaugh i (x i) 
iVP Shuddered = \ x i -> iShudder i (x i) 

iVP (VP3 attitude to inf) = iAV attitude (iINF inf) 

iCN :: CN -> IEntity -> IBool 
iCN Girl = \ x i -> iGirl i (x i) 
iCN Princess = \ x i -> iPrincess i (x i) 

iRCN (RCN3 adj cn) = iADJ adj (iCN cn)

eval1 = iSent (Sent SnowWhite Laughed) W1
eval2 = iSent (Sent SnowWhite Laughed) W2
eval3 = iSent (Sent Someone Shuddered) W1
eval4 = iSent (Sent Someone Shuddered) W2
eval5 = iSent (Sent (NP1 Every Girl) Shuddered) W1
eval6 = iSent (Sent (NP1 Every Girl) Shuddered) W2
eval7 = iSent (Sent (NP1 Some Girl) Shuddered) W1
eval8 = iSent (Sent (NP1 Some Girl) Shuddered) W2

iADJ :: ADJ -> (IEntity -> IBool) -> IEntity -> IBool
iADJ Fake = \ p x i -> 
  not (p x i) && any (\ j -> p x j) worlds 

eval9 = iSent 
  (Sent (NP1 Some Princess) Shuddered) W1
eval10 = iSent 
  (Sent (NP2 Some (RCN3 Fake Princess)) Shuddered) W1
eval11 = iSent 
  (Sent (NP2 Some (RCN3 Fake Princess)) Shuddered) W2

iINF :: INF -> IEntity -> IBool
iINF Laugh   = \ x i -> iLaugh i (x i) 
iINF Shudder = \ x i -> iShudder i (x i)
iINF (INF tinf np) = \ s -> iNP np (\ o -> iTINF tinf s o)

iTINF :: TINF -> IEntity -> IEntity -> IBool 
iTINF Catch = \x y w -> iCatch w (x w) (y w)

iAttit :: AV -> IEntity -> IBool 
iAttit Wanted x = \i -> elem i [W2,W3]
iAttit Hoped  x = \i -> i == W3

iAV :: AV -> (IEntity -> IBool) -> (IEntity -> IBool)
iAV Wanted p = \ x i -> 
  and [ p x j | j <- worlds, iAttit Wanted x j ]
iAV Hoped  p = \ x i -> 
  and [ p x j | j <- worlds, iAttit Hoped  x j ]

eval12 = iSent (Sent SnowWhite 
  (VP3 Wanted To (INF Catch (NP1 Some Girl)))) W1
eval13 = iSent (Sent SnowWhite 
  (VP3 Wanted To (INF Catch (NP1 No Girl)))) W2

data Judgement = IsTrue Sent 
               | IsNec  Sent 
               | IsCont Sent deriving Show

iJudgement :: Judgement -> IBool
iJudgement (IsTrue s) = \ i -> iSent s i
iJudgement (IsNec s) = \ i -> 
  all (\j -> iSent s j) worlds
iJudgement (IsCont s) = \ i -> 
  iSent s i && not (all (\j -> iSent s j) worlds)

judgement1,judgement2,judgement3,judgement4 :: Bool
judgement1 = iJudgement 
  (IsTrue (Sent (NP1 Some Girl) Shuddered)) W1
judgement2 = iJudgement 
  (IsTrue (Sent (NP1 Some Girl) Shuddered)) W2
judgement3 = iJudgement 
  (IsNec  (Sent (NP1 Some Girl) Shuddered)) W1
judgement4 = iJudgement 
  (IsCont (Sent (NP1 Some Girl) Shuddered)) W1

iProp :: (World -> Entity -> Bool) -> IEntity -> IBool
iProp x = \ y i -> x i (y i) 

vpINT :: VP -> World -> Entity -> Bool
vpINT Laughed   = iLaugh
vpINT Shuddered = iShudder

intensVP :: VP -> IEntity -> IBool
intensVP = iProp . vpINT

eProp :: (IEntity -> IBool) -> World -> Entity -> Bool
eProp y = \ j x -> y (\k -> x) j

iPropToB :: (World -> ((Entity -> Bool) -> Bool)) 
                           -> (IEntity -> IBool) -> IBool
iPropToB x = \ y i -> x i (eProp y i) 

ePropToB :: ((IEntity -> IBool) -> IBool) -> 
      World -> (Entity -> Bool) -> Bool
ePropToB y = \ j x -> y (iProp (\k -> x)) j

iPropToPropToB :: 
  (World -> (Entity -> Bool) -> (Entity -> Bool) -> Bool)
         -> (IEntity -> IBool) -> (IEntity -> IBool) -> IBool
iPropToPropToB x = \ y1 y2 i -> 
   x i (eProp y1 i) (eProp y2 i) 

ePropToPropToB :: 
     ((IEntity -> IBool) -> (IEntity -> IBool) -> IBool) -> 
      World -> (Entity -> Bool) -> (Entity -> Bool) -> Bool
ePropToPropToB y = \ j x1 x2  -> 
   y (iProp (\k -> x1)) (iProp (\k -> x2)) j

detINT :: DET ->  World -> 
    (Entity -> Bool) -> (Entity -> Bool) -> Bool
detINT det = \ i -> intDET det

intensDET :: DET -> (IEntity -> IBool) 
                 -> (IEntity -> IBool) -> IBool
intensDET = iPropToPropToB . detINT

isSnoww :: IEntity -> Bool
isSnoww x = and [ x i == iSnowWhite i | i <- worlds ]

myY :: IEntity -> IBool
myY x | isSnoww x = \i -> i == W1
      | otherwise = \i -> False 

myY' :: IEntity  -> IBool
myY' = iProp (eProp myY)
