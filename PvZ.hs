module PvZ where
import Data.List
import Data.Maybe

type Coordinate = (Int, Int)
type Sun = Int

data Plant = Peashooter Int | Sunflower Int | Walnut Int | CherryBomb Int deriving (Show, Eq)    -- életpont
data Zombie = Basic Int Int | Conehead Int Int | Buckethead Int Int | Vaulting Int Int deriving (Show, Eq)   --1.életpont, 2.sebesség
data GameModel = GameModel Sun [(Coordinate, Plant)] [(Coordinate, Zombie)] deriving (Show, Eq)

--növények
defaultPeashooter :: Plant
defaultPeashooter = Peashooter 3

defaultSunflower :: Plant
defaultSunflower = Sunflower 2

defaultWalnut :: Plant
defaultWalnut = Walnut 15

defaultCherryBomb :: Plant
defaultCherryBomb = CherryBomb 2

--zombik
basic :: Zombie
basic = Basic 5 1

coneHead :: Zombie
coneHead = Conehead 10 1

bucketHead :: Zombie
bucketHead = Buckethead 20 1

vaulting :: Zombie
vaulting = Vaulting 7 2

--Növények vásárlása
tryPurchase :: GameModel -> Coordinate -> Plant -> Maybe GameModel
tryPurchase (GameModel x ps zs) c@(a,b) p
    | lookup c ps /= Nothing = Nothing                              --Ha már van az adott koordinátán növény akkor Nothing
    | x < value p = Nothing                                         --Ha nincs elég nap a vásáráshoz akkor Nothing
    | a < 0 || a > 4 || b < 0 || b > 11 = Nothing                   --Ha nem megfelelő a koordináta akkor Nothing
    | otherwise = Just (GameModel (x - value p) ((c,p):ps) zs)      --Különben az adott koordinátára helyezi a növenyt és levonja az árát
        where
            value :: Plant -> Int
            value (CherryBomb _) = 150
            value (Peashooter _) = 100
            value _ = 50

--Zombik lerakása
placeZombieInLane :: GameModel -> Zombie -> Int -> Maybe GameModel
placeZombieInLane (GameModel x ps zs) z r
    | lookup (r,11) zs /= Nothing = Nothing                         --Ha a sáv végén már van zombi akkor Nothing
    | r < 0 || r > 4 = Nothing                                      --Ha nem megfelelő sor szám akkor Nothing
    | otherwise = Just (GameModel x ps (((r,11),z):zs))             --Különben a sor végére helyez egy zombit

--Zombik mozgása és támadása
lowerHP :: (Coordinate,Plant) -> (Coordinate,Plant)          --Egy adott koordinátájú növény életét csökkenti egyel
lowerHP (c,(Peashooter x)) = (c,Peashooter (x-1))
lowerHP (c,(CherryBomb x)) = (c,CherryBomb (x-1))
lowerHP (c,(Walnut x)) = (c,Walnut (x-1))
lowerHP (c,(Sunflower x)) = (c,Sunflower (x-1))

lowerPos :: (Coordinate,Zombie) -> (Coordinate,Zombie)           --Egy adott koordinátájú zombi előre haladása
lowerPos ((a,b), (Basic h s)) = ((a,b-s), (Basic h s))
lowerPos ((a,b), (Conehead h s)) = ((a,b-s), (Conehead h s))
lowerPos ((a,b), (Buckethead h s)) = ((a,b-s), (Buckethead h s))
lowerPos ((a,b), (Vaulting h s)) = ((a,b-s), (Vaulting h s))

lowerHpIfPos :: [(Coordinate,Zombie)] -> (Coordinate,Plant) -> (Coordinate,Plant)  --Sebezze meg a Zombi a virágot ha ugyan ott van
lowerHpIfPos [] p = p                                                              --Ha nincs zombi akkor nem sebződnek a virágok
lowerHpIfPos (z@((a,b),(Vaulting h s)):zs) p                                       --Ha vaulting és
    | fst z == fst p && s == 1 = lowerHpIfPos zs (lowerHP p)                       --1 a sebessége és ugyan azon a helyen vannak, sebezze a virágot
    | otherwise = lowerHpIfPos zs p                                                --különben ne sebezze
lowerHpIfPos (z:zs) p                                                              --Ha nem vaulting akkor
    | fst p == fst z = lowerHpIfPos zs (lowerHP p)                                 --ha ugyan azon a pozíción vannak sebezze a virágot
    | otherwise = lowerHpIfPos zs p                                                --különben ne

vaultingOnPlant :: [(Coordinate,Plant)] -> (Coordinate,Zombie) -> Maybe (Coordinate,Zombie)  --Rajta van-e a vaulting egy virágon
vaultingOnPlant [] _ = Nothing                                                               --Ha nincs virág akkor nincs rajta zombi
vaultingOnPlant (p:ps) z@((a,b),(Vaulting h s))
    | s == 2 && (fst z == fst p) = Just ((a,b - 1),(Vaulting h 1))                           --Ha rajta van és a sebessége 2 akkor lépje át, sebesség 1 lesz
    | otherwise = vaultingOnPlant ps z                                                       --Ha nincs rajta vagy 1 a sebessége akkor másik fügvényben cselekszik

goForward :: [(Coordinate,Plant)] -> (Coordinate,Zombie) -> (Coordinate,Zombie)                             --Egy adott zombi lépése
goForward [] z = lowerPos z                                                                                 --Ha nincsenek virágok menjen előre
goForward l@(p:ps) z@((a,b),(Vaulting h s))                                                                 --Ha vaulting és nincs virágon akkor ha átlépné a virágot akkor tegye meg különben csak lépjen előre
    | vaultingOnPlant l z == Nothing = if (a == (fst $ fst p) && (snd $ fst $ lowerPos z) == ((snd $ fst p) - 1)) then ((a,b - s),(Vaulting h 1)) else goForward ps z
    | otherwise = fromJust (vaultingOnPlant l z)                                                            --Ha rajta van a virágon akkor lépje át ha sebessége 2
goForward (p:ps) z                                                                                          --Ha nem vaulting
    | fst p == fst z = z                                                                                    --és ugyan az a koordináta ne lepjen elore
    | otherwise = goForward ps z                                                                            --különben nézzük meg a következő virágra is

performZombieActions :: GameModel -> Maybe GameModel
performZombieActions g@(GameModel s ps []) = Just g                                      --Ha nincsenek zombik nem történik semmi      
performZombieActions (GameModel s [] zs)                                                 --Ha nincsenek virágok
    | not $ null $ filter (\x -> (snd $ fst x) <= 0) zs = Nothing                        --és van olyan zombi amelyik eljut a játéktér végére akkor Nothing
    | otherwise = Just (GameModel s [] (map lowerPos zs))                                --Különben lépjenek előre a zombik
performZombieActions (GameModel s ps zs)                                                    --Ha van mindegyik
    | not $ null $ filter (\x -> (snd $ fst x) <= 0) zs = Nothing                           --és beért egy zombi akkor Nothing
    | otherwise = Just (GameModel s (map (lowerHpIfPos zs $) ps) (map (goForward ps $) zs)) --különben virágok sebződnek, aki tud előre lép

--Pályatisztítás
getZHP :: ((Coordinate, Zombie)) -> Int                    --egy zombi hpjat adja vissza
getZHP (_, (Basic h s)) = h
getZHP (_, (Conehead h s)) = h
getZHP (_, (Buckethead h s)) = h
getZHP (_, (Vaulting h s)) = h

cleanBoard :: GameModel -> GameModel                        --Csak a 0-nál több hp-val rendelkező dolog maradhat
cleanBoard (GameModel s ps zs) = (GameModel s (filter (\p -> getPHP p > 0) ps) (filter (\z -> getZHP z > 0) zs))
    where
        getPHP (c,(Peashooter x)) = x                       --Egy növény hp-ját adja vissza
        getPHP (c,(CherryBomb x)) = x
        getPHP (c,(Walnut x)) = x
        getPHP (c,(Sunflower x)) = x

--Növények műveletei
sunInc :: GameModel -> GameModel                            --Minden Sunflower 25 napot ad
sunInc (GameModel s ps zs) = GameModel (((helper ps 0) * 25) + s) ps zs
    where
        helper [] acc = acc
        helper (((a,b),(Sunflower _)):ps) acc = helper ps (acc+1)
        helper (p:ps) acc = helper ps acc

setZHP :: Int -> (Coordinate, Zombie) -> (Coordinate, Zombie)       --Átálítja egy zombi hpját adott számra
setZHP x ((a,b), (Basic h s)) = ((a,b), (Basic x s))
setZHP x ((a,b), (Conehead h s)) = ((a,b), (Conehead x s))
setZHP x ((a,b), (Buckethead h s)) = ((a,b), (Buckethead x s))
setZHP x ((a,b), (Vaulting h s)) = ((a,b), (Vaulting x s))

minCol :: [(Coordinate, Zombie)] -> Int -> (Coordinate)                    --kiválasztja egy oszlop előtt lévő első zombit
minCol [] _ = (-3,-3)                                                      --Ha nincs zombi nem létező koordináta
minCol [z] col                                                             --Ha egy zombi van akkor
    | (snd $ fst z) >= col = fst z                                         --ha az oszlop elott van akkor o van legkozelebb
    | otherwise = (-3, -3)                                                 --kulonben senki
minCol (x:y:ys) col                                                        
    | (snd $ fst x) <= (snd $ fst y) && (snd $ fst x) >= col = minCol (x:ys) col    --Ha az elso kozelebb van es az oszlop utan akkor maradjon a listaban
    | (snd $ fst x) >= (snd $ fst y) && (snd $ fst y) >= col = minCol (y:ys) col    --Ha a masodik akkor ugyan ez foditva
    | ((snd $ fst x) < col && (snd $ fst y) >= col) = minCol (y:ys) col             --Ha az elso nincs az oszlop elott hagyja el
    | ((snd $ fst y) < col && (snd $ fst x) >= col) = minCol (x:ys) col             --Ugyan ez a masikra
    | otherwise = minCol ys col                                                     --különben mindegyiket hagyja el

isAround :: Coordinate -> Coordinate -> Bool                        --3*3mas negyzeten belül van-e a koordináta
isAround (a,b) x = x `elem` [(c,d) | c <- [a-1..a+1], d <- [b-1..b+1]]

flowerDMG :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate,Zombie)]       --Sebző virágok lépése
flowerDMG ps [] = []                                                                        --Ha nincs zombi nem történik semmi
flowerDMG [] zs = zs                                                                        --Ha nincs virág nem sebződnek
flowerDMG ps@(((a,b), (Peashooter _)):ls) zs = flowerDMG ls (map (\z -> if min >= b && (fst $ fst z) == a && min == (snd $ fst z) then setZHP ((getZHP z) - 1) z else z) zs)
    where                                                                           --Ha a zombi az adott sorban van és ő a legközelebbi zombi akkor -1 hp különben marad
        min = snd $ minCol (filter (\x -> (fst $ fst x) == a) zs) b
flowerDMG ps@(((a,b), (CherryBomb _)):ls) zs = flowerDMG ls (map (\z -> if (isAround (a,b) (fst z)) then setZHP 0 z else z) zs) --Ha a zombi a virág körül van (3*3) 0 hpja legyen
flowerDMG (p:ps) zs = flowerDMG ps zs                                                       --Többi virág nem sebez


bombDeath :: [(Coordinate, Plant)] -> [(Coordinate, Plant)]                     --a robbanó virág megöli magát
bombDeath ps = map f ps
    where
        f ((a,b),(CherryBomb h)) = ((a,b),(CherryBomb 0))
        f p = p

getSun :: GameModel -> Int         --Vissza adja a Sun mennyiségét
getSun (GameModel s _ _) = s

performPlantActions :: GameModel -> GameModel                           --növények lépépse
performPlantActions g@(GameModel s [] zs) = g                                                  --ha nincs növény ugyan az marad
performPlantActions g@(GameModel s ps []) = GameModel (getSun (sunInc g)) (bombDeath ps) []    --cherrybomb felrobban és sun gyűlik(Ha nincs zombi nincs kit sebezni)
performPlantActions g@(GameModel s ps zs) = GameModel (getSun (sunInc g)) (bombDeath ps) (flowerDMG ps zs) --különben nepot kapunk, meghal a robbanó noveny, a zombik sebzodnek


--Játék menete
--Szimuláció
placeARound :: GameModel -> [(Int, Zombie)] -> GameModel                --elhelyez egy listányi zombit
placeARound g ls = helper g ls
    where
        helper g@(GameModel s ps xs) [] = g                             --Ha nem kell senkit lerakni adja vissza a játék helyzetét
        helper g@(GameModel s ps xs) ((n,z):zs)                         --Ha kell letenni akkor
            | placeZombieInLane g z n == Nothing = helper g zs          --ha már van ott zombi vagy nem megfelelő a sor akkor a következőt probalja
            | otherwise = helper (fromJust(placeZombieInLane g z n)) zs --különben tegye le

sunAtTheEnd :: GameModel -> GameModel
sunAtTheEnd (GameModel s ps zs) = GameModel (s+25) ps zs                --növeli 25-tel a nap értékét

getZombies :: GameModel -> [(Coordinate,Zombie)]           --Az adott játék állásból adja meg a zombik helyzetét
getZombies (GameModel _ _ []) = []
getZombies (GameModel _ _ zs) = zs

defendsAgainst :: GameModel -> [[(Int, Zombie)]] -> Bool
defendsAgainst g@(GameModel s ps []) [] = True                                          --Ha nincsenek zombik a pályán és nem is fognak jönni akkor a virágok nyertek
defendsAgainst g@(GameModel s ps zs) []                                                 --Ha vannak zombik de már nem jönnek akkor,
    | (performZombieActions $ cleanBoard $ performPlantActions g) == Nothing = False    --ha bejut egy zombi akkor ők nyertek
    | otherwise = defendsAgainst (sunAtTheEnd $ cleanBoard $ fromJust $ performZombieActions $ cleanBoard $ performPlantActions g) [] --különben folytatódik a játék
defendsAgainst g@(GameModel s ps []) (zs:xs) = defendsAgainst (sunAtTheEnd $ cleanBoard $ placeARound (cleanBoard $ performPlantActions g) zs) xs --Ha nincs a pályán zombi de fog jönni akkor új zombik lerakása
defendsAgainst g@(GameModel s ps zs) (xs:ys)                                            --Ha van is lent és jönni is fog akkor,
    | (performZombieActions $ cleanBoard $ performPlantActions g) == Nothing = False    --ha beér egy zombi akkor ők nyertek
    | otherwise = defendsAgainst (sunAtTheEnd $ cleanBoard $ placeARound (fromJust $ performZombieActions $ cleanBoard $ performPlantActions g) xs) ys --különben folytatódik a játék


--Interaktív szimuláció
defendsAgainstI :: (GameModel -> GameModel) -> GameModel -> [[(Int, Zombie)]] -> Bool
defendsAgainstI f g@(GameModel 0 [] zs) []                                               --Ha nincsenek virágok
    | null (getZombies (f g)) = True                                                     --és nem lesznek zombik -> virágok nyertek
    | otherwise = False                                                                  --de vannak zombik (vagy lesznek a felhasználó miatt) -> zombik nyertek
defendsAgainstI f g@(GameModel 0 [] zs) (xs:ys) = defendsAgainstI f (placeARound (f g) xs) ys   --Ha nincsenek virágok de jönnek
defendsAgainstI f g@(GameModel s ps []) [] = True                                        --Ha nincsenek zombik a pályán és nem is fognak jönni akkor a virágok nyertek
defendsAgainstI f g@(GameModel s ps zs) []                                               --Ha vannak de már nem jönnek akkor,
    | (performZombieActions $ cleanBoard $ performPlantActions (f g)) == Nothing = False --A játékos módosításával folytatódik a játék és ha beér egy zombi azok nyernek
    | otherwise = defendsAgainstI f (sunAtTheEnd $ cleanBoard $ placeARound (fromJust $ performZombieActions $ cleanBoard $ performPlantActions $ f g) []) [] --különben mehet tovább
defendsAgainstI f g@(GameModel s ps []) (zs:xs) = defendsAgainstI f (sunAtTheEnd $ cleanBoard $ placeARound (fromJust $ performZombieActions $ cleanBoard $ performPlantActions $ f g) zs) xs    --Ha nincs a pályán zombi de fog jönni akkor új zombik lerakása
defendsAgainstI f g@(GameModel s ps zs) (xs:ys)                                           --Ha van is lent és jönni is fog akkor,
    | (performZombieActions $ cleanBoard $ performPlantActions (f g)) == Nothing = False  --ha beér egy zombi akkor ők nyertek
    | otherwise = defendsAgainstI f (sunAtTheEnd $ cleanBoard $ placeARound (fromJust $ performZombieActions $ cleanBoard $ performPlantActions $ f g) xs) ys --különben folytatódik a játék