import System.IO
import System.Random
import Data.Maybe
import qualified Data.Map as Map

type RoomID = String
type Item = String
type WeaponPower = Int

data Room = Room { 
    roomId   :: RoomID, 
    desc     :: String,
    options  :: [(String, RoomID)],
    effect   :: PlayerState -> IO PlayerState, 
    isEnd    :: Bool
}

type World = Map.Map RoomID Room

data PlayerState = PlayerState { 
    location :: RoomID, 
    inventory :: [Item],
    alive :: Bool,
    visitedRooms :: [RoomID],  -- Lista pokojów gdzie już podniesiono przedmioty
    defeatedEnemies :: [String] -- Lista pokonanych przeciwników
}

-- Dane o przeciwnikach
data Enemy = Enemy {
    enemyName :: String,
    enemyStrength :: Int,
    attackMessages :: [String],
    defeatMessage :: String,
    victoryMessage :: String
}

-- Mapa broni i ich siły
weaponPower :: Map.Map Item WeaponPower
weaponPower = Map.fromList [
    ("Stary miecz", 1),
    ("Magiczny sztylet", 2),
    ("Ognisty młot", 3),
    ("Łuk elfów", 2),
    ("Runiczna siekiera", 3)
    ]

-- Definicje przeciwników
trollEnemy :: Enemy
trollEnemy = Enemy {
    enemyName = "potężny troll",
    enemyStrength = 7,
    attackMessages = [
        "Troll macha ogromną maczugą, próbując cię zmiażdżyć!",
        "Troll ryczy i uderza pięścią w ziemię, sprawiając że cała jaskinia się trzęsie!",
        "Troll rzuca w ciebie kamieniem wielkości głowy!",
        "Troll próbuje złapać cię swoimi ogromnymi łapami!",
        "Troll atakuje z dziką furią, jego oczy płoną czerwienią!"
    ],
    defeatMessage = "Troll pada z hukiem na ziemię, pokonany!",
    victoryMessage = "Troll łapie cię swoją ogromną łapą i wyrzuca z jaskini! Ląduje twardy, ale żyjesz..."
}

witchEnemy :: Enemy
witchEnemy = Enemy {
    enemyName = "mroczna wiedźma",
    enemyStrength = 5,
    attackMessages = [
        "Wiedźma rzuca w ciebie płonącą kulę ognia!",
        "Wiedźma mamrocze zaklęcie i wypuszcza chmurę trującego dymu!",
        "Wiedźma wyciąga kostistą różdżkę i strzela w ciebie błyskawicą!",
        "Wiedźma wzywa kruki, które atakują cię dziobami i pazurami!",
        "Wiedźma próbuje rzucić na ciebie klątwę przemiany!"
    ],
    defeatMessage = "Wiedźma krzyczy przeraźliwie i rozpływa się w dymie!",
    victoryMessage = "Wiedźma rzuca na ciebie czar snu... Budzisz się w lesie, oszołomiony i bez ekwipunku..."
}

-- Funkcja pomocnicza: dodaj przedmiot do inwentarza
addItem :: Item -> PlayerState -> PlayerState
addItem item ps = ps { inventory = item : inventory ps }

-- Funkcja pomocnicza: sprawdź czy pokój został już odwiedzony
hasVisited :: RoomID -> PlayerState -> Bool
hasVisited roomId ps = roomId `elem` visitedRooms ps

-- Funkcja pomocnicza: oznacz pokój jako odwiedzony
markVisited :: RoomID -> PlayerState -> PlayerState
markVisited roomId ps = ps { visitedRooms = roomId : visitedRooms ps }

-- Funkcja pomocnicza: sprawdź czy przeciwnik został pokonany
isEnemyDefeated :: String -> PlayerState -> Bool
isEnemyDefeated enemy ps = enemy `elem` defeatedEnemies ps

-- Funkcja pomocnicza: oznacz przeciwnika jako pokonanego
markEnemyDefeated :: String -> PlayerState -> PlayerState
markEnemyDefeated enemy ps = ps { defeatedEnemies = enemy : defeatedEnemies ps }

-- Funkcja obliczająca siłę gracza
getPlayerPower :: PlayerState -> Int
getPlayerPower ps = sum $ mapMaybe (`Map.lookup` weaponPower) (inventory ps)

-- Funkcja czyszczenia terminala
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- Funkcja pomocnicza: pokaz inwentarz z informacjami o broniach
showInventory :: PlayerState -> IO ()
showInventory ps = do
    clearScreen
    putStrLn "\n=== EKWIPUNEK ==="
    if null (inventory ps)
        then putStrLn "- (pusty)"
        else do
            mapM_ showItem (inventory ps)
            putStrLn $ "\nTwoja łączna siła: " ++ show (getPlayerPower ps)
    putStrLn "\nNaciśnij Enter, aby kontynuować..."
    _ <- getLine
    return ()
  where
    showItem item = 
        case Map.lookup item weaponPower of
            Just power -> putStrLn $ "- " ++ item ++ " (siła: +" ++ show power ++ ")"
            Nothing -> putStrLn $ "- " ++ item

-- Ulepszona funkcja walki z reakcjami przeciwników
fightSequence :: Enemy -> PlayerState -> IO PlayerState
fightSequence enemy ps = do
    let playerPower = getPlayerPower ps
    putStrLn $ "\n=== ROZPOCZYNA SIĘ WALKA! ==="
    putStrLn $ "Stoisz twarzą w twarz z " ++ enemyName enemy ++ "!"
    putStrLn $ "Twoja siła: " ++ show playerPower
    putStrLn $ "Siła wroga: " ++ show (enemyStrength enemy)
    putStrLn "\nPodnosisz broń i przygotowujesz się do ataku..."
    putStrLn "Naciśnij Enter, aby zaatakować..."
    _ <- getLine
    
    -- Rzut gracza
    playerRoll <- randomRIO (1, 10) :: IO Int
    let totalPlayerPower = playerRoll + playerPower
    putStrLn $ "\nTwój atak: rzucasz kostką " ++ show playerRoll ++ " + twoja siła " ++ show playerPower ++ " = " ++ show totalPlayerPower
    
    -- Losowy atak przeciwnika
    attackIndex <- randomRIO (0, length (attackMessages enemy) - 1)
    let enemyAttack = (attackMessages enemy) !! attackIndex
    putStrLn $ "\n" ++ enemyAttack
    
    -- Rzut przeciwnika
    enemyRoll <- randomRIO (1, 6) :: IO Int
    let totalEnemyPower = enemyRoll + enemyStrength enemy
    putStrLn $ "Atak wroga: " ++ show enemyRoll ++ " + siła " ++ show (enemyStrength enemy) ++ " = " ++ show totalEnemyPower
    
    putStrLn "\nRozstrzygnięcie walki..."
    putStrLn "Naciśnij Enter..."
    _ <- getLine
    
    if totalPlayerPower > totalEnemyPower
    then do
        putStrLn $ "\nZWYCIĘSTWO!"
        putStrLn $ defeatMessage enemy
        return $ markEnemyDefeated (enemyName enemy) ps
    else do
        putStrLn $ "\nPORAŻKA!"
        putStrLn $ victoryMessage enemy
        putStrLn "\nTracisz cały ekwipunek i budzisz się w bezpiecznym miejscu..."
        putStrLn "Na szczęście żyjesz i możesz kontynuować przygodę!"
        return ps { inventory = [], location = getDefeatLocation (enemyName enemy) }

-- Funkcja określająca gdzie gracz się budzi po porażce
getDefeatLocation :: String -> RoomID
getDefeatLocation enemy
    | "troll" `elem` words enemy = "mountains"
    | "wiedźma" `elem` words enemy = "forest"
    | otherwise = "start"

-- Świat gry
world :: World
world = Map.fromList $ map (\r -> (roomId r, r)) [ 
    Room "start" "Stoisz na rozdrożu w magicznej krainie. Wiatr szumi przez trawy, a przed tobą rozciągają się trzy ścieżki." [ 
        ("Idź na północ do gór", "mountains"), 
        ("Idź na wschód do lasu", "forest"), 
        ("Idź na południe do wioski", "village"),
        ("Przeszukaj okolicę", "search_crossroads")
    ] return False,

    Room "search_crossroads" "Przeszukujesz okolice rozdrożu..." [
        ("Wróć na rozdroże", "start")
    ] (\ps -> do
        if hasVisited "search_crossroads" ps
        then do
            putStrLn "Przeszukujesz ponownie okolicę, ale nic więcej tu nie ma."
            return ps
        else do
            putStrLn "W wysokiej trawie znajdujesz porzucony miecz!"
            putStrLn "To stary, ale wciąż ostry miecz. Może się przydać w walce."
            return $ markVisited "search_crossroads" $ addItem "Stary miecz" ps
    ) False,
    
    Room "mountains" "Wchodzisz w skaliste góry. Zimny wiatr świszczy między szczytami. W oddali słychać głębokie warczenie trolla." [ 
        ("Wejdź do jaskini trolla", "troll"), 
        ("Przeszukaj skalną półkę", "mountain_search"),
        ("Wróć na rozdroże", "start")
    ] return False,

    Room "mountain_search" "Wspinasz się na skalną półkę..." [
        ("Wróć do gór", "mountains")
    ] (\ps -> do
        if hasVisited "mountain_search" ps
        then do
            putStrLn "Przeszukujesz ponownie skalną półkę, ale nic więcej tu nie ma."
            return ps
        else do
            roll <- randomRIO (1, 10) :: IO Int
            if roll <= 6
            then do
                putStrLn "Na półce leży błyszczący sztylet z magicznymi runami!"
                putStrLn "Czujesz moc płynącą z ostrza."
                return $ markVisited "mountain_search" $ addItem "Magiczny sztylet" ps
            else do
                putStrLn "Kamień się osypuje! Ledwo unikasz upadku."
                putStrLn "Nic tu nie ma, tylko luźne skały."
                return $ markVisited "mountain_search" ps
    ) False,
    
    Room "troll" "Wchodzisz do ciemnej jaskini. Nagle słyszysz ciężkie kroki i widzisz ogromną sylwetkę trolla o czerwonych oczach!" [ 
        ("Walcz z trollem!", "fight_troll"), 
        ("Uciekaj do gór", "mountains")
    ] (\ps -> do
        if isEnemyDefeated "potężny troll" ps
        then do
            putStrLn "Jaskinia trolla jest teraz cicha i spokojna."
            putStrLn "Na ziemi leży pokonany troll, a jego skarby błyszczą w półmroku."
            return ps
        else do
            putStrLn "Troll zauważa cię i ryczy gniewnie!"
            putStrLn "Jego wielkie pięści uderzają o ściany jaskini, sprawiając, że kamienie spadają z sufitu."
            return ps
    ) False,

    Room "fight_troll" "Arena walki z trollem!" [ 
        ("Zakończ przygodę", "end"), 
        ("Wróć do gór", "mountains")
    ] (\ps -> do
        if isEnemyDefeated "potężny troll" ps
        then do
            putStrLn "Przeszukujesz skarbiec trolla..."
            if "Talizman" `elem` inventory ps
            then do
                putStrLn "Już masz Talizman. Nie ma tu nic więcej wartościowego."
                return ps
            else do
                putStrLn "Znajdujesz legendarny Talizman wśród skarbów trolla!"
                putStrLn "Talizman pulsuje magiczną energią w twoich dłoniach."
                return $ addItem "Talizman" ps
        else do
            result <- fightSequence trollEnemy ps
            if isEnemyDefeated "potężny troll" result
            then do
                putStrLn "\nTroll pada z hukiem, a z jego skarbca wydobywasz legendarny Talizman!"
                putStrLn "Talizman pulsuje magiczną energią w twoich dłoniach."
                return $ addItem "Talizman" result
            else return result
    ) False,

    Room "forest" "Wchodzisz w mglisty las. Drzewa szumią tajemniczo, a między gałęziami migocze dziwne światło." [ 
        ("Idź za światłem", "witch"), 
        ("Przeszukaj stary pień", "forest_search"),
        ("Wróć na rozdroże", "start")
    ] return False,

    Room "forest_search" "Przeszukujesz wydrążony pień starego dębu..." [
        ("Wróć do lasu", "forest")
    ] (\ps -> do
        if hasVisited "forest_search" ps
        then do
            putStrLn "Przeszukujesz ponownie stary pień, ale jest już pusty."
            return ps
        else do
            putStrLn "W środku pnia znajujesz piękny łuk z napisami w języku elfów!"
            putStrLn "Łuk jest lekki jak piórko, ale czujesz w nim wielką moc."
            return $ markVisited "forest_search" $ addItem "Łuk elfów" ps
    ) False,

    Room "witch" "Napotykasz starą wiedźmę przy kociołku. Bulgocząca mikstura wydaje dziwne opary." [ 
        ("Przyjmij dar", "gift"), 
        ("Walcz z wiedźmą!", "fight_witch"),
        ("Uciekaj!", "forest")
    ] (\ps -> do
        if isEnemyDefeated "mroczna wiedźma" ps
        then do
            putStrLn "Miejsce gdzie była wiedźma jest teraz puste."
            putStrLn "Pozostał tylko kociołek z bulgoczącą miksturą."
            return ps
        else do
            putStrLn "Wiedźma patrzy na ciebie przenikliwym wzrokiem..."
            putStrLn "'Witaj, wędrowcze. Mogę ci ofiarować dar... ale wszystko ma swoją cenę.'"
            return ps
    ) False,

    Room "fight_witch" "Walka z wiedźmą!" [ 
        ("Wróć do lasu", "forest")
    ] (\ps -> do
        if isEnemyDefeated "mroczna wiedźma" ps
        then do
            putStrLn "Przeszukujesz pozostałości po wiedźmie..."
            if "Ognisty młot" `elem` inventory ps
            then do
                putStrLn "Już masz Ognisty młot. Nic więcej tu nie ma."
                return ps
            else do
                putStrLn "Z resztek kociołka wydobywasz płonący młot!"
                return $ addItem "Ognisty młot" ps
        else do
            putStrLn "Wiedźma krzyczy i zaczyna rzucać czary!"
            putStrLn "Jej oczy płoną niebieskim ogniem!"
            result <- fightSequence witchEnemy ps
            if isEnemyDefeated "mroczna wiedźma" result
            then do
                putStrLn "\nWiedźma zostaje pokonana i znika w dymie!"
                putStrLn "Z jej kociołka wydobywasz płonący młot!"
                return $ addItem "Ognisty młot" result
            else return result
    ) False,

    Room "gift" "Wiedźma rzuca czar..." [ 
        ("Wróć do lasu", "forest")
    ] (\ps -> do
        if isEnemyDefeated "mroczna wiedźma" ps
        then do
            putStrLn "Wiedźma została już pokonana. Nie ma tu nikogo."
            return ps
        else do
            roll <- randomRIO (1, 10) :: IO Int
            if roll <= 4
            then do
                putStrLn "Otrzymujesz magiczny amulet!"
                putStrLn "Amulet świeci ciepłym światłem i dodaje ci pewności siebie."
                return $ addItem "Amulet" ps
            else do
                putStrLn "Wiedźma śmieje się złowrogo!"
                putStrLn "'Głupiec! To była pułapka!' - wykrzykuje i rzuca śmiertelny czar!"
                putStrLn "Budzisz się w lesie bez ekwipunku, ale żyjesz..."
                return ps { inventory = [] }
    ) False,

    Room "village" "Wchodzisz do małej wioski. Wieśniacy patrzą na ciebie podejrzliwie, niektórzy chowają się w domach." [ 
        ("Odpocznij w karczmie", "inn"), 
        ("Zapytaj starca o Talizman", "elder"), 
        ("Przeszukaj kuźnię", "blacksmith"),
        ("Wróć na rozdroże", "start")
    ] return False,

    Room "blacksmith" "Wchodzisz do opuszczonej kuźni..." [
        ("Wróć do wioski", "village")
    ] (\ps -> do
        if hasVisited "blacksmith" ps
        then do
            putStrLn "Przeszukujesz ponownie kuźnię, ale już nic tu nie ma."
            return ps
        else do
            putStrLn "Na stole kowala leży potężna siekiera z runicznymi napisami!"
            putStrLn "Siekiera jest ciężka, ale doskonale wyważona."
            return $ markVisited "blacksmith" $ addItem "Runiczna siekiera" ps
    ) False,

    Room "inn" "Karczmarz, brodaty mężczyzna o podejrzliwym spojrzeniu, oferuje ci eliksir za darmo." [ 
        ("Wypij eliksir", "drink"), 
        ("Zapytaj o eliksir", "ask_innkeeper"),
        ("Wyjdź", "village")
    ] (\ps -> do
        putStrLn "W karczmie panuje półmrok. Kilku miejscowych siedzi w kącie i szepce."
        putStrLn "Karczmarz nalewa zielonkawy eliksir do kubka..."
        return ps
    ) False,

    Room "ask_innkeeper" "Rozmawiasz z karczmarzem..." [ 
        ("Wypij eliksir", "drink"), 
        ("Wyjdź", "village")
    ] (\ps -> do
        putStrLn "Karczmarz mruczy: 'To specjalny napój... da ci siły na drogę.'"
        putStrLn "Jego oczy błyszczą podejrzliwie w półmroku."
        return ps
    ) False,

    Room "drink" "Wypiłeś eliksir..." [ 
        ("Wróć do wioski", "village")
    ] (\ps -> do
        roll <- randomRIO (1, 10) :: IO Int
        if roll <= 3
        then do
            putStrLn "Nagle czujesz piekący ból w żołądku!"
            putStrLn "Eliksir był zatruty! Tracisz przytomność..."
            putStrLn "Budzisz się na zewnątrz karczmy bez ekwipunku, ale żyjesz."
            return ps { inventory = [] }
        else do
            putStrLn "Czujesz, jak ciepło rozlewa się po twoim ciele."
            putStrLn "Odzyskałeś siły! Karczmarz kiwa głową z zadowoleniem."
            return ps
    ) False,

    Room "elder" "Starzec z długą brodą patrzy na ciebie mądrymi oczami." [ 
        ("Idź do gór", "mountains"), 
        ("Zostań w wiosce", "village")
    ] (\ps -> do
        putStrLn "Starzec mówi powoli: 'Talizman... tak, znam tę legendę.'"
        putStrLn "'Znajduje się w jaskini trolla w górach. Ale uważaj, młody wojowniku...'"
        putStrLn "'Troll jest potężny. Będziesz potrzebował dobrej broni, by go pokonać.'"
        return ps
    ) False,

    Room "end" "Zwycięstwo! Z Talizmanem w ręku kończy się twoja przygoda. Jego moc pozwoli ci powrócić do domu jako bohater!" [] return True
    ]

-- Główna pętla gry
play :: PlayerState -> World -> IO ()
play ps worldMap = do
    clearScreen
    let currentRoom = fromJust (Map.lookup (location ps) worldMap)
    putStrLn $ "\n== " ++ roomId currentRoom ++ " =="
    putStrLn $ desc currentRoom

    -- Efekt pokoju
    ps' <- effect currentRoom ps
    if not (alive ps')
    then do
        putStrLn "\nZginąłeś. KONIEC GRY."
        putStrLn "Naciśnij Enter, aby zakończyć..."
        _ <- getLine
        return ()
    else if isEnd currentRoom
    then do
        putStrLn "\nKONIEC GRY - ZWYCIĘSTWO!"
        putStrLn "Naciśnij Enter, aby zakończyć..."
        _ <- getLine
        return ()
    else do
        let allOptions = options currentRoom ++ [("Pokaż ekwipunek", "inventory")]
        putStrLn ""
        mapM_ (\(i, (desc, _)) -> putStrLn $ show i ++ ". " ++ desc) (zip [1..] allOptions)
        putStr "\nTwój wybór: "
        hFlush stdout
        choice <- getLine
        let maybeIndex = reads choice :: [(Int, String)]
        case maybeIndex of
            [(n, _)] | n > 0 && n <= length allOptions -> do
                let (_, nextAction) = allOptions !! (n - 1)
                if nextAction == "inventory"
                then do
                    showInventory ps'
                    play ps' worldMap
                else
                    play ps' { location = nextAction } worldMap
            _ -> do
                putStrLn "Nieprawidłowy wybór. Spróbuj jeszcze raz."
                putStrLn "Naciśnij Enter, aby kontynuować..."
                _ <- getLine
                play ps' worldMap

-- Start gry
main :: IO ()
main = do
    clearScreen
    putStrLn "=== Przygoda: Talizman Losu ==="
    putStrLn "\nW tej grze możesz znaleźć różne bronie, które pomogą ci w walce."
    putStrLn "Każda broń ma swoją siłę, która dodaje się do twoich ataków!"
    putStrLn "Jeśli przegrasz walkę, nie zginiesz - budzisz się bez ekwipunku, ale możesz kontynuować!"
    putStrLn "\nNaciśnij Enter, aby rozpocząć przygodę..."
    _ <- getLine
    let initialState = PlayerState { 
        location = "start", 
        inventory = [], 
        alive = True, 
        visitedRooms = [],
        defeatedEnemies = [],
        solvedPuzzles = [],
        undergroundKeys = 0,
        currentLevel = 0,
        hasTalisman = False,
        hasCrown = False
    }
    play initialState world