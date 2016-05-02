module EAI where 
import FSynF
import Model
import Model2
import TCOM

data World = W1 | W2 | W3 | W4 | W5 | W6 | W7 | W8 | W9 | W10 |
     W11 | W12 | W13 | W14 | W15 | W16 | W17 | W18 | W19 | W20 |
     W21 | W22 | W23 | W24 | W25 | W26 | W27 | W28 | W29 | W30 |
     W31 | W32 | W33 | W34 | W35 | W36 | W37 | W38 | W39 | W40 |
     W41 | W42 | W43 | W44 | W45 | W46 | W47 | W48 | W49 | W50 |
     W51 | W52 | W53 | W54 | W55 | W56 | W57 | W58 | W59 | W60 |
     W61 | W62 | W63 | W64 | W65 | W66 | W67 | W68 | W69 | W70 |
     W71 | W72 | W73 | W74 | W75 | W76 | W77 | W78 | W79 | W80 | 
     W81 | W82 | W83 | W84 | W85 | W86 | W87 | W88 | W89 | W90 |
     W91 | W92 | W93 | W94 | W95 | W96 | W97 | W98 | W99 | W100 |
     W101 | W102 | W103 | W104 | W105 | W106 | W107 | W108 | W109 | W110 |
     W111 | W112 | W113 | W114 | W115 | W116 | W117 | W118 | W119 | W120 |
     W121 | W122 | W123 | W124 | W125 deriving (Eq,Show,Ord)

--Initial set of worlds
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

--Generate correct world
genCorrectWorld 1 = W1
genCorrectWorld 2 = W2
genCorrectWorld 3 = W3
genCorrectWorld 4 = W4
genCorrectWorld 5 = W5
genCorrectWorld 6 = W6
genCorrectWorld 7 = W7
genCorrectWorld 8 = W8
genCorrectWorld 9 = W9
genCorrectWorld 10 = W10
genCorrectWorld 11 = W11
genCorrectWorld 12 = W12
genCorrectWorld 13 = W13
genCorrectWorld 14 = W14
genCorrectWorld 15 = W15
genCorrectWorld 16 = W16
genCorrectWorld 17 = W17
genCorrectWorld 18 = W18
genCorrectWorld 19 = W19
genCorrectWorld 20 = W20
genCorrectWorld 21 = W21
genCorrectWorld 22 = W22
genCorrectWorld 23 = W23
genCorrectWorld 24 = W24
genCorrectWorld 25 = W25
genCorrectWorld 26 = W26
genCorrectWorld 27 = W27
genCorrectWorld 28 = W28
genCorrectWorld 29 = W29
genCorrectWorld 30 = W30
genCorrectWorld 31 = W31
genCorrectWorld 32 = W32
genCorrectWorld 33 = W33
genCorrectWorld 34 = W34
genCorrectWorld 35 = W35
genCorrectWorld 36 = W36
genCorrectWorld 37 = W37
genCorrectWorld 38 = W38
genCorrectWorld 39 = W39
genCorrectWorld 40 = W40
genCorrectWorld 41 = W41
genCorrectWorld 42 = W42
genCorrectWorld 43 = W43
genCorrectWorld 44 = W44
genCorrectWorld 45 = W45
genCorrectWorld 46 = W46
genCorrectWorld 47 = W47
genCorrectWorld 48 = W48
genCorrectWorld 49 = W49
genCorrectWorld 50 = W50
genCorrectWorld 51 = W51
genCorrectWorld 52 = W52
genCorrectWorld 53 = W53
genCorrectWorld 54 = W54
genCorrectWorld 55 = W55
genCorrectWorld 56 = W56
genCorrectWorld 57 = W57
genCorrectWorld 58 = W58
genCorrectWorld 59 = W59
genCorrectWorld 60 = W60
genCorrectWorld 61 = W61
genCorrectWorld 62 = W62
genCorrectWorld 63 = W63
genCorrectWorld 64 = W64
genCorrectWorld 65 = W65
genCorrectWorld 66 = W66
genCorrectWorld 67 = W67
genCorrectWorld 68 = W68
genCorrectWorld 69 = W69
genCorrectWorld 70 = W70
genCorrectWorld 71 = W71
genCorrectWorld 72 = W72
genCorrectWorld 73 = W73
genCorrectWorld 74 = W74
genCorrectWorld 75 = W75
genCorrectWorld 76 = W76
genCorrectWorld 77 = W77
genCorrectWorld 78 = W78
genCorrectWorld 79 = W79
genCorrectWorld 80 = W80
genCorrectWorld 81 = W81
genCorrectWorld 82 = W82
genCorrectWorld 83 = W83
genCorrectWorld 84 = W84
genCorrectWorld 85 = W85
genCorrectWorld 86 = W86
genCorrectWorld 87 = W87
genCorrectWorld 88 = W88
genCorrectWorld 89 = W89
genCorrectWorld 90 = W90
genCorrectWorld 91 = W91
genCorrectWorld 92 = W92
genCorrectWorld 93 = W93
genCorrectWorld 94 = W94
genCorrectWorld 95 = W95
genCorrectWorld 96 = W96
genCorrectWorld 97 = W97
genCorrectWorld 98 = W98
genCorrectWorld 99 = W99
genCorrectWorld 100 = W100
genCorrectWorld 101 = W101
genCorrectWorld 102 = W102
genCorrectWorld 103 = W103
genCorrectWorld 104 = W104
genCorrectWorld 105 = W105
genCorrectWorld 106 = W106
genCorrectWorld 107 = W107
genCorrectWorld 108 = W108
genCorrectWorld 109 = W109
genCorrectWorld 110 = W110
genCorrectWorld 111 = W111
genCorrectWorld 112 = W112
genCorrectWorld 113 = W113
genCorrectWorld 114 = W114
genCorrectWorld 115 = W115
genCorrectWorld 116 = W116
genCorrectWorld 117 = W117
genCorrectWorld 118 = W118
genCorrectWorld 119 = W119
genCorrectWorld 120 = W120
genCorrectWorld 121 = W121
genCorrectWorld 122 = W122
genCorrectWorld 123 = W123
genCorrectWorld 124 = W124
genCorrectWorld 125 = W125
genCorrectWorld _ = W47

--  -- PROFESSORS -- -- 
--Bruce
iBruce :: IEntity
iBruce W1 = bruce'
iBruce W2 = bruce'
iBruce W3 = bruce'
iBruce W4 = bruce'
iBruce W5 = bruce'
iBruce W6 = bruce'
iBruce W7 = bruce'
iBruce W8 = bruce'
iBruce W9 = bruce'
iBruce W10 = bruce'
iBruce W11 = bruce'
iBruce W12 = bruce'
iBruce W13 = bruce'
iBruce W14 = bruce'
iBruce W15 = bruce'
iBruce W16 = bruce'
iBruce W17 = bruce'
iBruce W18 = bruce'
iBruce W19 = bruce'
iBruce W20 = bruce'
iBruce W21 = bruce'
iBruce W22 = bruce'
iBruce W23 = bruce'
iBruce W24 = bruce'
iBruce W25 = bruce'
iBruce _ = bruce
 
--Chen
iChen :: IEntity
iChen W26 = chen'
iChen W27 = chen'
iChen W28 = chen'
iChen W29 = chen'
iChen W30 = chen'
iChen W31 = chen'
iChen W32 = chen'
iChen W33 = chen'
iChen W34 = chen'
iChen W35 = chen'
iChen W36 = chen'
iChen W37 = chen'
iChen W38 = chen'
iChen W39 = chen'
iChen W40 = chen'
iChen W41 = chen'
iChen W42 = chen'
iChen W43 = chen'
iChen W44 = chen'
iChen W45 = chen'
iChen W46 = chen'
iChen W47 = chen'
iChen W48 = chen'
iChen W49 = chen'
iChen W50 = chen'
iChen _ = chen

--Greenberg
iGreenberg :: IEntity
iGreenberg W51 = greenberg'
iGreenberg W52 = greenberg'
iGreenberg W53 = greenberg'
iGreenberg W54 = greenberg'
iGreenberg W55 = greenberg'
iGreenberg W56 = greenberg'
iGreenberg W57 = greenberg'
iGreenberg W58 = greenberg'
iGreenberg W59 = greenberg'
iGreenberg W60 = greenberg'
iGreenberg W61 = greenberg'
iGreenberg W62 = greenberg'
iGreenberg W63 = greenberg'
iGreenberg W64 = greenberg'
iGreenberg W65 = greenberg'
iGreenberg W66 = greenberg'
iGreenberg W67 = greenberg'
iGreenberg W68 = greenberg'
iGreenberg W69 = greenberg'
iGreenberg W70 = greenberg'
iGreenberg W71 = greenberg'
iGreenberg W72 = greenberg'
iGreenberg W73 = greenberg'
iGreenberg W74 = greenberg'
iGreenberg W75 = greenberg'
iGreenberg _ = greenberg

--Kauchak
iKauchak :: IEntity
iKauchak W76 = kauchak'
iKauchak W77 = kauchak'
iKauchak W78 = kauchak'
iKauchak W79 = kauchak'
iKauchak W80 = kauchak'
iKauchak W81 = kauchak'
iKauchak W82 = kauchak'
iKauchak W83 = kauchak'
iKauchak W84 = kauchak'
iKauchak W85 = kauchak'
iKauchak W86 = kauchak'
iKauchak W87 = kauchak'
iKauchak W88 = kauchak'
iKauchak W89 = kauchak'
iKauchak W90 = kauchak'
iKauchak W91 = kauchak'
iKauchak W92 = kauchak'
iKauchak W93 = kauchak'
iKauchak W94 = kauchak'
iKauchak W95 = kauchak'
iKauchak W96 = kauchak'
iKauchak W97 = kauchak'
iKauchak W98 = kauchak'
iKauchak W99 = kauchak'
iKauchak W100 = kauchak'
iKauchak _ = kauchak

--Wu
iWu :: IEntity
iWu W101 = wu'
iWu W102 = wu'
iWu W103 = wu'
iWu W104 = wu'
iWu W105 = wu'
iWu W106 = wu'
iWu W107 = wu'
iWu W108 = wu'
iWu W109 = wu'
iWu W110 = wu'
iWu W111 = wu'
iWu W112 = wu'
iWu W113 = wu'
iWu W114 = wu'
iWu W115 = wu'
iWu W116 = wu'
iWu W117 = wu'
iWu W118 = wu'
iWu W119 = wu'
iWu W120 = wu'
iWu W121 = wu'
iWu W122 = wu'
iWu W123 = wu'
iWu W124 = wu'
iWu W125 = wu'
iWu _ = wu

-- -- CLASSES -- --
--CS 52
iCS52 :: IEntity
iCS52 W1 = cs52' 
iCS52 W2 = cs52'
iCS52 W3 = cs52'
iCS52 W4 = cs52'
iCS52 W5 = cs52'
iCS52 W26 = cs52'
iCS52 W27 = cs52'
iCS52 W28 = cs52'
iCS52 W29 = cs52'
iCS52 W30 = cs52'
iCS52 W51 = cs52'
iCS52 W52 = cs52'
iCS52 W53 = cs52'
iCS52 W54 = cs52'
iCS52 W55 = cs52'
iCS52 W76 = cs52'
iCS52 W77 = cs52'
iCS52 W78 = cs52'
iCS52 W79 = cs52'
iCS52 W80 = cs52'
iCS52 W101 = cs52'
iCS52 W102 = cs52'
iCS52 W103 = cs52'
iCS52 W104 = cs52'
iCS52 W105 = cs52'
iCS52 _ = cs52

--CS 62
iCS62 :: IEntity
iCS62 W6 = cs62'
iCS62 W7 = cs62'
iCS62 W8 = cs62'
iCS62 W9 = cs62'
iCS62 W10 = cs62'
iCS62 W31 = cs62'
iCS62 W32 = cs62'
iCS62 W33 = cs62'
iCS62 W34 = cs62'
iCS62 W35 = cs62'
iCS62 W56 = cs62'
iCS62 W57 = cs62'
iCS62 W58 = cs62'
iCS62 W59 = cs62'
iCS62 W60 = cs62'
iCS62 W81 = cs62'
iCS62 W82 = cs62'
iCS62 W83 = cs62'
iCS62 W84 = cs62'
iCS62 W85 = cs62'
iCS62 W106 = cs62'
iCS62 W107 = cs62'
iCS62 W108 = cs62'
iCS62 W109 = cs62'
iCS62 W110 = cs62'
iCS62 _ = cs62

iCS81 W15 = cs81'
iCS81 W36 = cs81'
iCS81 W37 = cs81'
iCS81 W38 = cs81'
iCS81 W39 = cs81'
iCS81 W40 = cs81'
iCS81 W61 = cs81'
iCS81 W62 = cs81'
iCS81 W63 = cs81'
iCS81 W64 = cs81'
iCS81 W65 = cs81'
iCS81 W86 = cs81'
iCS81 W87 = cs81'
iCS81 W88 = cs81'
iCS81 W89 = cs81'
iCS81 W90 = cs81'
iCS81 W111 = cs81'
iCS81 W112 = cs81'
iCS81 W113 = cs81'
iCS81 W114 = cs81'
iCS81 W115 = cs81'
iCS81 _ = cs81

--Systems
iSystems :: IEntity
iSystems W16 = systems'
iSystems W17 = systems'
iSystems W18 = systems'
iSystems W19 = systems'
iSystems W20 = systems'
iSystems W41 = systems'
iSystems W42 = systems'
iSystems W43 = systems'
iSystems W44 = systems'
iSystems W45 = systems'
iSystems W66 = systems'
iSystems W67 = systems'
iSystems W68 = systems'
iSystems W69 = systems'
iSystems W70 = systems'
iSystems W91 = systems'
iSystems W92 = systems'
iSystems W93 = systems'
iSystems W94 = systems'
iSystems W95 = systems'
iSystems W116 = systems'
iSystems W117 = systems'
iSystems W118 = systems'
iSystems W119 = systems'
iSystems W120 = systems'
iSystems _ = systems

--Algs
iAlgs :: IEntity
iAlgs W21 = algs'
iAlgs W22 = algs'
iAlgs W23 = algs'
iAlgs W24 = algs'
iAlgs W25 = algs'
iAlgs W46 = algs'
iAlgs W47 = algs'
iAlgs W48 = algs'
iAlgs W49 = algs'
iAlgs W50 = algs'
iAlgs W71 = algs'
iAlgs W72 = algs'
iAlgs W73 = algs'
iAlgs W74 = algs'
iAlgs W75 = algs'
iAlgs W96 = algs'
iAlgs W97 = algs'
iAlgs W98 = algs'
iAlgs W99 = algs'
iAlgs W100 = algs'
iAlgs W121 = algs'
iAlgs W122 = algs'
iAlgs W123 = algs'
iAlgs W124 = algs'
iAlgs W125 = algs'
iAlgs _ = algs


-- -- LOCATIONS -- --

--Edmunds
--iEdmunds ::  World -> Entity -> Bool
iEdmunds W1 = edmunds' 
iEdmunds W6 = edmunds'
iEdmunds W11 = edmunds'
iEdmunds W16 = edmunds'
iEdmunds W21 = edmunds'
iEdmunds W26 = edmunds'
iEdmunds W31 = edmunds'
iEdmunds W36 = edmunds'
iEdmunds W41 = edmunds'
iEdmunds W46 = edmunds'
iEdmunds W51 = edmunds'
iEdmunds W56 = edmunds'
iEdmunds W61 = edmunds'
iEdmunds W66 = edmunds'
iEdmunds W71 = edmunds'
iEdmunds W76 = edmunds'
iEdmunds W81 = edmunds'
iEdmunds W86 = edmunds'
iEdmunds W91 = edmunds'
iEdmunds W96 = edmunds'
iEdmunds W101 = edmunds'
iEdmunds W106 = edmunds'
iEdmunds W111 = edmunds'
iEdmunds W116 = edmunds'
iEdmunds W121 = edmunds'
iEdmunds _ = edmunds


--Lincoln
--iLincoln :: IEntity
iLincoln W2 = lincoln'
iLincoln W7 = lincoln'
iLincoln W12 = lincoln'
iLincoln W17 = lincoln'
iLincoln W22 = lincoln'
iLincoln W27 = lincoln'
iLincoln W32 = lincoln'
iLincoln W37 = lincoln'
iLincoln W42 = lincoln'
iLincoln W47 = lincoln'
iLincoln W52 = lincoln'
iLincoln W57 = lincoln'
iLincoln W62 = lincoln'
iLincoln W67 = lincoln'
iLincoln W72 = lincoln'
iLincoln W77 = lincoln'
iLincoln W82 = lincoln'
iLincoln W87 = lincoln'
iLincoln W92 = lincoln'
iLincoln W97 = lincoln'
iLincoln W102 = lincoln'
iLincoln W107 = lincoln'
iLincoln W112 = lincoln'
iLincoln W117 = lincoln'
iLincoln W122 = lincoln'
iLincoln _ = lincoln


--Skyspace
--iSkyspace :: IEntity
iSkyspace W3 = skyspace'
iSkyspace W8 = skyspace'
iSkyspace W13 = skyspace'
iSkyspace W18 = skyspace'
iSkyspace W23 = skyspace'
iSkyspace W28 = skyspace'
iSkyspace W33 = skyspace'
iSkyspace W38 = skyspace'
iSkyspace W43 = skyspace'
iSkyspace W48 = skyspace'
iSkyspace W53 = skyspace'
iSkyspace W58 = skyspace'
iSkyspace W63 = skyspace'
iSkyspace W68 = skyspace'
iSkyspace W73 = skyspace'
iSkyspace W78 = skyspace'
iSkyspace W83 = skyspace'
iSkyspace W88 = skyspace'
iSkyspace W93 = skyspace'
iSkyspace W98 = skyspace'
iSkyspace W103 = skyspace'
iSkyspace W108 = skyspace'
iSkyspace W113 = skyspace'
iSkyspace W118 = skyspace'
iSkyspace W123 = skyspace'
iSkyspace _ = skyspace


--Frary
--iFrary :: IEntity
iFrary W4 = frary'
iFrary W9 = frary'
iFrary W14 = frary'
iFrary W19 = frary'
iFrary W24 = frary'
iFrary W29 = frary'
iFrary W34 = frary'
iFrary W39 = frary'
iFrary W44 = frary'
iFrary W49 = frary'
iFrary W54 = frary'
iFrary W59 = frary'
iFrary W64 = frary'
iFrary W69 = frary'
iFrary W74 = frary'
iFrary W79 = frary'
iFrary W84 = frary'
iFrary W89 = frary'
iFrary W94 = frary'
iFrary W99 = frary'
iFrary W104 = frary'
iFrary W109 = frary'
iFrary W114 = frary'
iFrary W119 = frary'
iFrary W124 = frary'
iFrary _ = frary


--Frank
--iFrank :: IEntity
iFrank W5 = frank'
iFrank W10 = frank'
iFrank W15 = frank'
iFrank W20 = frank'
iFrank W25 = frank'
iFrank W30 = frank'
iFrank W35 = frank'
iFrank W40 = frank'
iFrank W45 = frank'
iFrank W50 = frank'
iFrank W55 = frank'
iFrank W60 = frank'
iFrank W65 = frank'
iFrank W70 = frank'
iFrank W75 = frank'
iFrank W80 = frank'
iFrank W85 = frank'
iFrank W90 = frank'
iFrank W95 = frank'
iFrank W100 = frank'
iFrank W105 = frank'
iFrank W110 = frank'
iFrank W115 = frank'
iFrank W120 = frank'
iFrank W125 = frank'
iFrank _ = frank

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

--ADDED KILLER
iKiller _ = killer
iWeapon _ = weapon
iCrimescene _ = crimescene

iIs _ = is

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
----------------------------- MORE CODE FOR CLUE --------------------------------------
--Profs
iNP Bruce = \ p -> p iBruce
iNP Chen = \ p -> p iChen
iNP Greenberg = \ p -> p iGreenberg
iNP Kauchak = \ p -> p iKauchak
iNP Wu = \ p -> p iWu

--Classes
iNP CS52 = \ p -> p iCS52
iNP CS62 = \ p -> p iCS62
iNP CS81 = \ p -> p iCS81
iNP Systems = \ p -> p iSystems
iNP Algs = \ p -> p iAlgs

--Locations
iNP Edmunds =  \ p -> p iEdmunds
iNP Lincoln = \ p -> p iLincoln
iNP Skyspace = \ p -> p iSkyspace
iNP Frary = \ p -> p iFrary
iNP Frank = \ p -> p iFrank
-------------------------------------------------------------------------------------
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

--ADDED THE
iDET The p q = \ i -> singleton (filter (\x -> p (\j -> x) i) entities) && (\x -> q (\j ->x) i) (head (filter (\x -> p (\j -> x) i) entities))
       where
          --plist = (filter (\x -> p (\j -> x) i) entities)
          singleton [x] = True
          singleton _ = False

--intTV Defeated = \ x y -> defeat x y
iTV Is = \ x y i -> iIs i (x i) (y i)  --  \ subj obj i -> iIs i (subj obj i)

iVP :: VP -> IEntity -> IBool
iVP Laughed   = \ x i -> iLaugh i (x i) 
iVP Shuddered = \ x i -> iShudder i (x i)

--intVP :: VP -> Entity -> Bool 
--intVP Laughed   = \ x -> laugh x
--intVP Cheered   = \ x -> cheer x 
--intVP Shuddered = \ x -> shudder x 

--val = (\ obj subj i -> iTV tv subj obj i) 
-- interpret verb phrases that are transitive verbs with object
iVP (VP1 tv np) = \ subj -> (iNP np) (\ obj i -> iTV tv subj obj i)

iVP (VP3 attitude to inf) = iAV attitude (iINF inf)

iCN :: CN -> IEntity -> IBool 
iCN Girl = \ x i -> iGirl i (x i) 
iCN Princess = \ x i -> iPrincess i (x i)

iCN Killer = \ x i -> iKiller i (x i)
iCN Weapon = \ x i -> iWeapon i (x i)
iCN Crimescene = \ x i -> iCrimescene i (x i)

iRCN (RCN3 adj cn) = iADJ adj (iCN cn)

eval1 = iSent (Sent SnowWhite Laughed) W1
eval2 = iSent (Sent SnowWhite Laughed) W2
eval3 = iSent (Sent Someone Shuddered) W1
eval4 = iSent (Sent Someone Shuddered) W2
eval5 = iSent (Sent (NP1 Every Killer) Shuddered) W1
eval6 = iSent (Sent (NP1 Every Girl) Shuddered) W2
eval7 = iSent (Sent (NP1 The Weapon) Shuddered) W1
eval8 = iSent (Sent (NP1 Some Girl) Shuddered) W2

--eval12 = iSent (Sent SnowWhite 
  --(VP3 Wanted To (INF Catch (NP1 Some Girl)))) W1
eval47 = iSent (Sent Bruce (VP1 Is (NP1 The Killer))) W1

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
eval13 = iSent (Sent Bruce
  (VP3 Wanted To (INF Catch (NP1 No Killer)))) W2

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
