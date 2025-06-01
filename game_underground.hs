import System.IO
import System.Random
import Data.Maybe
import qualified Data.Map as Map
import Data.List (nub, sort)

type RoomID = String
type Item = String
type WeaponPower = Int

-- Rekurencyjne struktury danych dla podziemi
data UndergroundLevel = UndergroundLevel {
    levelId :: Int,
    levelName :: String,
    levelRooms :: [RoomID],  -- pokoje na tym poziomie
    nextLevel :: Maybe UndergroundLevel,  -- następny poziom w głąb
    bossRoom :: Maybe RoomID,  -- pokój z bossem
    requiredKeys :: Int  -- ile kluczy potrzeba do przejścia dalej
} deriving (Show)

-- Struktura zagadki
data Puzzle = Puzzle {
    puzzleId :: String,
    puzzleDescription :: String,
    puzzleQuestion :: String,
    puzzleAnswers :: [String],  -- możliwe odpowiedzi
    correctAnswer :: String,
    puzzleReward :: Maybe Item,
    solvedMessage :: String
} deriving (Show)

data Room = Room { 
    roomId   :: RoomID, 
    desc     :: String,
    options  :: [(String, RoomID)],
    effect   :: PlayerState -> IO PlayerState, 
    isEnd    :: Bool,
    roomPuzzle :: Maybe Puzzle,  -- zagadka w pokoju
    isUnderground :: Bool  -- czy pokój jest w podziemiach
}

type World = Map.Map RoomID Room

data PlayerState = PlayerState { 
    location :: RoomID, 
    inventory :: [Item],
    alive :: Bool,
    visitedRooms :: [RoomID],
    defeatedEnemies :: [String],
    solvedPuzzles :: [String],  -- rozwiązane zagadki
    undergroundKeys :: Int,  -- klucze do podziemi
    currentLevel :: Int,  -- aktualny poziom podziemi
    hasTalisman :: Bool,  -- czy ma talizman
    hasCrown :: Bool  -- czy zdobył koronę
}

-- Dane o przeciwnikach
data Enemy = Enemy {
    enemyName :: String,
    enemyStrength :: Int,
    attackMessages :: [String],
    defeatMessage :: String,
    victoryMessage :: String,
    enemyReward :: Maybe Item
}

-- Mapa broni i ich siły
weaponPower :: Map.Map Item WeaponPower
weaponPower = Map.fromList [
    ("Stary miecz", 1),
    ("Magiczny sztylet", 2),
    ("Ognisty młot", 3),
    ("Łuk elfów", 2),
    ("Runiczna siekiera", 3),
    ("Klucz Podziemi", 0),  -- specjalny przedmiot
    ("Korona Władzy", 0),   -- cel gry
    ("Kryształowy Miecz", 4),  -- broń z podziemi
    ("Płaszcz Cieni", 0),      -- magiczny przedmiot
    ("Pierścień Mocy", 1)      -- dodatkowa moc
    ]

-- Definicja rekurencyjnej struktury podziemi
undergroundLevels :: UndergroundLevel
undergroundLevels = UndergroundLevel {
    levelId = 1,
    levelName = "Górne Podziemia",
    levelRooms = ["underground_entrance", "crystal_chamber", "skeleton_hall"],
    nextLevel = Just UndergroundLevel {
        levelId = 2,
        levelName = "Średnie Podziemia", 
        levelRooms = ["deeper_tunnel", "shadow_maze", "ancient_library"],
        nextLevel = Just UndergroundLevel {
            levelId = 3,
            levelName = "Głębokie Podziemia",
            levelRooms = ["throne_room", "final_chamber"],
            nextLevel = Nothing,
            bossRoom = Just "crown_guardian",
            requiredKeys = 3
        },
        bossRoom = Just "shadow_lord",
        requiredKeys = 2
    },
    bossRoom = Just "crystal_guardian", 
    requiredKeys = 1
}

-- Funkcja rekurencyjna do znajdowania poziomu
findLevel :: Int -> UndergroundLevel -> Maybe UndergroundLevel
findLevel targetLevel level
    | levelId level == targetLevel = Just level
    | otherwise = case nextLevel level of
        Nothing -> Nothing
        Just nextLvl -> findLevel targetLevel nextLvl

-- Funkcja rekurencyjna do zliczania wszystkich pokojów w podziemiach
countUndergroundRooms :: UndergroundLevel -> Int
countUndergroundRooms level = 
    length (levelRooms level) + 
    (case bossRoom level of Just _ -> 1; Nothing -> 0) +
    (case nextLevel level of 
        Nothing -> 0
        Just next -> countUndergroundRooms next)

-- Definicje przeciwników
trollEnemy :: Enemy
trollEnemy = Enemy {
    enemyName = "potężny troll",
    enemyStrength = 0,
    attackMessages = [
        "Troll macha ogromną maczugą, próbując cię zmiażdżyć!",
        "Troll ryczy i uderza pięścią w ziemię, sprawiając że cała jaskinia się trzęsie!",
        "Troll rzuca w ciebie kamieniem wielkości głowy!",
        "Troll próbuje złapać cię swoimi ogromnymi łapami!",
        "Troll atakuje z dziką furią, jego oczy płoną czerwienią!"
    ],
    defeatMessage = "Troll pada z hukiem na ziemię, pokonany!",
    victoryMessage = "Troll łapie cię swoją ogromną łapą i wyrzuca z jaskini! Ląduje twardy, ale żyjesz...",
    enemyReward = Nothing
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
    victoryMessage = "Wiedźma rzuca na ciebie czar snu... Budzisz się w lesie, oszołomiony i bez ekwipunku...",
    enemyReward = Nothing
}

-- Nowi przeciwnicy z podziemi
crystalGuardianEnemy :: Enemy
crystalGuardianEnemy = Enemy {
    enemyName = "Kryształowy Strażnik",
    enemyStrength = 0,
    attackMessages = [
        "Strażnik wystrzeliwuje ostre kryształy w twoją stronę!",
        "Jego kryształowe ramię przekształca się w ostrą broń!",
        "Strażnik świeci oślepiającym światłem kryształów!",
        "Ziemia trzęsie się gdy Strażnik uderza kryształową pięścią!"
    ],
    defeatMessage = "Kryształowy Strażnik rozpada się na tysiące świecących fragmentów!",
    victoryMessage = "Strażnik porąża cię swoją magią i wyrzuca z komory...",
    enemyReward = Just "Klucz Podziemi"
}

shadowLordEnemy :: Enemy
shadowLordEnemy = Enemy {
    enemyName = "Władca Cieni",
    enemyStrength = 0,
    attackMessages = [
        "Władca Cieni otacza cię ciemnością i atakuje z każdej strony!",
        "Cienie materializują się w ostre szpony!",
        "Władca znika w ciemności i atakuje gdy się tego nie spodziewasz!",
        "Mroczna energia otacza cię i wysysa twoją siłę!"
    ],
    defeatMessage = "Władca Cieni krzyczy i rozpływa się w nicości!",
    victoryMessage = "Cienie wciągają cię w głąb podziemi, tracisz przytomność...",
    enemyReward = Just "Płaszcz Cieni"
}

crownGuardianEnemy :: Enemy
crownGuardianEnemy = Enemy {
    enemyName = "Strażnik Korony",
    enemyStrength = 0,
    attackMessages = [
        "Strażnik wyciąga płonący miecz i atakuje z furią!",
        "Jego złota zbroja odbija światło, oślepiając cię!",
        "Strażnik wzywa duchy dawnych królów do walki!",
        "Magiczna aura Strażnika paraliżuje twoją broń!"
    ],
    defeatMessage = "Strażnik Korony pada na kolana i znika w złotym blasku!",
    victoryMessage = "Strażnik pokonuje cię swoją potęgą...",
    enemyReward = Just "Korona Władzy"
}

-- Zagadki dla podziemi
riddlePuzzles :: [Puzzle]
riddlePuzzles = [
    Puzzle {
        puzzleId = "riddle1",
        puzzleDescription = "Na ścianie widnieją starożytne runy i zagadka.",
        puzzleQuestion = "Mam korony, ale nie jestem królem. Mam korzenie, ale nie jestem drzewem. Kim jestem?",
        puzzleAnswers = ["ząb", "zab", "teeth", "tooth"],
        correctAnswer = "ząb",
        puzzleReward = Just "Klucz Podziemi",
        solvedMessage = "Runy świecą i otwierają sekretną niszę z kluczem!"
    },
    Puzzle {
        puzzleId = "riddle2", 
        puzzleDescription = "Magiczny portal wymaga hasła.",
        puzzleQuestion = "Jestem początkiem końca i końcem czasu i przestrzeni. Co jestem?",
        puzzleAnswers = ["litera e", "e", "letter e"],
        correctAnswer = "e",
        puzzleReward = Just "Pierścień Mocy",
        solvedMessage = "Portal otwiera się i ukazuje pierścień!"
    },
    Puzzle {
        puzzleId = "riddle3",
        puzzleDescription = "Starożytna biblioteka kryje sekret.",
        puzzleQuestion = "Im więcej ze mnie zabierasz, tym większy się staję. Co jestem?",
        puzzleAnswers = ["dziura", "dół", "jama", "hole"],
        correctAnswer = "dziura", 
        puzzleReward = Just "Kryształowy Miecz",
        solvedMessage = "Księgi układają się w kryształowy miecz!"
    }
    ]

-- Funkcje pomocnicze
addItem :: Item -> PlayerState -> PlayerState
addItem item ps = ps { inventory = item : inventory ps }

hasVisited :: RoomID -> PlayerState -> Bool
hasVisited roomId ps = roomId `elem` visitedRooms ps

markVisited :: RoomID -> PlayerState -> PlayerState
markVisited roomId ps = ps { visitedRooms = roomId : visitedRooms ps }

isEnemyDefeated :: String -> PlayerState -> Bool
isEnemyDefeated enemy ps = enemy `elem` defeatedEnemies ps

markEnemyDefeated :: String -> PlayerState -> PlayerState
markEnemyDefeated enemy ps = ps { defeatedEnemies = enemy : defeatedEnemies ps }

isPuzzleSolved :: String -> PlayerState -> Bool
isPuzzleSolved puzzleId ps = puzzleId `elem` solvedPuzzles ps

markPuzzleSolved :: String -> PlayerState -> PlayerState
markPuzzleSolved puzzleId ps = ps { solvedPuzzles = puzzleId : solvedPuzzles ps }

getPlayerPower :: PlayerState -> Int
getPlayerPower ps = sum $ mapMaybe (`Map.lookup` weaponPower) (inventory ps)

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- Funkcja rozwiązywania zagadek
solvePuzzle :: Puzzle -> PlayerState -> IO PlayerState
solvePuzzle puzzle ps = do
    if isPuzzleSolved (puzzleId puzzle) ps
    then do
        putStrLn "Ta zagadka została już rozwiązana."
        return ps
    else do
        putStrLn $ puzzleDescription puzzle
        putStrLn $ "\n" ++ puzzleQuestion puzzle
        putStr "Twoja odpowiedź: "
        hFlush stdout
        answer <- getLine
        let lowerAnswer = map toLower answer
            possibleAnswers = map (map toLower) (puzzleAnswers puzzle)
        if lowerAnswer `elem` possibleAnswers
        then do
            putStrLn $ "\nPoprawna odpowiedź!"
            putStrLn $ solvedMessage puzzle
            let newState = markPuzzleSolved (puzzleId puzzle) ps
            case puzzleReward puzzle of
                Just reward -> do
                    putStrLn $ "Otrzymujesz: " ++ reward
                    return $ addItem reward newState
                Nothing -> return newState
        else do
            putStrLn "Niepoprawna odpowiedź. Spróbuj ponownie później."
            return ps
  where
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

showInventory :: PlayerState -> IO ()
showInventory ps = do
    clearScreen
    putStrLn "\n=== EKWIPUNEK ==="
    if null (inventory ps)
        then putStrLn "- (pusty)"
        else do
            mapM_ showItem (inventory ps)
            putStrLn $ "\nTwoja łączna siła: " ++ show (getPlayerPower ps)
            putStrLn $ "Klucze Podziemi: " ++ show (undergroundKeys ps)
            putStrLn $ "Aktualny poziom podziemi: " ++ show (currentLevel ps)
    putStrLn "\nNaciśnij Enter, aby kontynuować..."
    _ <- getLine
    return ()
  where
    showItem item = 
        case Map.lookup item weaponPower of
            Just power -> putStrLn $ "- " ++ item ++ " (siła: +" ++ show power ++ ")"
            Nothing -> putStrLn $ "- " ++ item

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
    
    playerRoll <- randomRIO (1, 10) :: IO Int
    let totalPlayerPower = playerRoll + playerPower
    putStrLn $ "\nTwój atak: rzucasz kostką " ++ show playerRoll ++ " + twoja siła " ++ show playerPower ++ " = " ++ show totalPlayerPower
    
    attackIndex <- randomRIO (0, length (attackMessages enemy) - 1)
    let enemyAttack = (attackMessages enemy) !! attackIndex
    putStrLn $ "\n" ++ enemyAttack
    
    enemyRoll <- randomRIO (1, 6) :: IO Int
    let totalEnemyPower = enemyRoll + enemyStrength enemy
    putStrLn $ "Atak wroga: " ++ show enemyRoll ++ " + siła " ++ show (enemyStrength enemy) ++ " = " ++ show totalEnemyPower
    
    putStrLn "\nRozstrzygnięcie walki..."
    putStrLn "Naciśnij Enter..."
    _ <- getLine

    clearScreen
    
    if totalPlayerPower > totalEnemyPower
    then do
        putStrLn $ "\nZWYCIĘSTWO!"
        putStrLn $ defeatMessage enemy
        let newState = markEnemyDefeated (enemyName enemy) ps
        case enemyReward enemy of
            Just reward -> do
                putStrLn $ "Otrzymujesz nagrodę: " ++ reward ++ "!"
                if reward == "Klucz Podziemi"
                then return $ newState { undergroundKeys = undergroundKeys newState + 1 }
                else if reward == "Korona Władzy"
                then return $ addItem reward $ newState { hasCrown = True }
                else return $ addItem reward newState
            Nothing -> return newState
    else do
        putStrLn $ "\nPORAŻKA!"
        putStrLn $ victoryMessage enemy
        putStrLn "\nTracisz cały ekwipunek i budzisz się w bezpiecznym miejscu..."
        putStrLn "Na szczęście żyjesz i możesz kontynuować przygodę!"
        return ps { inventory = [], location = getDefeatLocation (enemyName enemy) }

getDefeatLocation :: String -> RoomID
getDefeatLocation enemy
    | "troll" `elem` words enemy = "mountains"
    | "wiedźma" `elem` words enemy = "forest"
    | "Strażnik" `elem` words enemy || "Władca" `elem` words enemy = "underground_entrance"
    | otherwise = "start"

-- Funkcja sprawdzająca czy gracz może przejść na następny poziom podziemi
canDescendLevel :: PlayerState -> UndergroundLevel -> Bool
canDescendLevel ps level = undergroundKeys ps >= requiredKeys level

-- Świat gry z podziemiami
world :: World
world = Map.fromList $ map (\r -> (roomId r, r)) [ 
    -- POWIERZCHNIA
    Room "start" "Stoisz na rozdrożu w magicznej krainie. Wiatr szumi przez trawy, a przed tobą rozciągają się trzy ścieżki." [ 
        ("Idź na północ do gór", "mountains"), 
        ("Idź na wschód do lasu", "forest"), 
        ("Idź na południe do wioski", "village"),
        ("Przeszukaj okolicę", "search_crossroads")
    ] return False Nothing False,

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
    ) False Nothing False,
    
    Room "mountains" "Wchodzisz w skaliste góry. Zimny wiatr świszczy między szczytami. W oddali słychać głębokie warczenie trolla." [ 
        ("Wejdź do jaskini trolla", "troll"), 
        ("Przeszukaj skalną półkę", "mountain_search"),
        ("Wróć na rozdroże", "start")
    ] return False Nothing False,

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
    ) False Nothing False,
    
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
    ) False Nothing False,

    Room "fight_troll" "Arena walki z trollem!" [ 
        ("Użyj Talizmanu (przejdź do podziemi)", "use_talisman"), 
        ("Wróć do gór", "mountains")
    ] (\ps -> do
        if isEnemyDefeated "potężny troll" ps
        then do
            putStrLn "Przeszukujesz skarbiec trolla..."
            if "Talizman" `elem` inventory ps
            then do
                putStrLn "Już masz Talizman. Pulsuje mocniej niż wcześniej..."
                putStrLn "Czujesz, że może przenieść cię w nowe miejsce!"
                return ps
            else do
                putStrLn "Znajdujesz legendarny Talizman wśród skarbów trolla!"
                putStrLn "Talizman pulsuje magiczną energią w twoich dłoniach."
                return $ addItem "Talizman" $ ps { hasTalisman = True }
        else do
            result <- fightSequence trollEnemy ps
            if isEnemyDefeated "potężny troll" result
            then do
                putStrLn "\nTroll pada z hukiem, a z jego skarbca wydobywasz legendarny Talizman!"
                putStrLn "Talizman pulsuje magiczną energią w twoich dłoniach."
                return $ addItem "Talizman" $ result { hasTalisman = True }
            else return result
    ) False Nothing False,

    -- Nowy pokój - użycie Talizmanu
    Room "use_talisman" "Używasz Talizmanu..." [
        ("Wejdź do podziemi", "underground_entrance")
    ] (\ps -> do
        if hasTalisman ps
        then do
            putStrLn "Talizman świeci oślepiającym światłem!"
            putStrLn "Ziemia pod twoimi stopami zaczyna się rozstępować..."
            putStrLn "Spadasz w głąb, ale Talizman chroni cię przed upadkiem!"
            putStrLn "\n=== WKRACZASZ DO PODZIEMI ==="
            putStrLn "Przed tobą rozciąga się labirynt korytarzy i komnat..."
            return ps { currentLevel = 1 }
        else do
            putStrLn "Nie masz Talizmanu! Musisz go najpierw zdobyć."
            return ps { location = "fight_troll" }
    ) False Nothing False,

    Room "forest" "Wchodzisz w mglisty las. Drzewa szumią tajemniczo, a między gałęziami migocze dziwne światło." [ 
        ("Idź za światłem", "witch"), 
        ("Przeszukaj stary pień", "forest_search"),
        ("Wróć na rozdroże", "start")
    ] return False Nothing False,

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
    ) False Nothing False,

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
    ) False Nothing False,

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
    ) False Nothing False,

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
    ) False Nothing False,

    Room "village" "Wchodzisz do małej wioski. Wieśniacy patrzą na ciebie podejrzliwie, niektórzy chowają się w domach." [ 
        ("Odpocznij w karczmie", "inn"), 
        ("Zapytaj starca o Talizman", "elder"), 
        ("Przeszukaj kuźnię", "blacksmith"),
        ("Wróć na rozdroże", "start")
    ] return False Nothing False,

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
    ) False Nothing False,

    Room "inn" "Karczmarz, brodaty mężczyzna o podejrzliwym spojrzeniu, oferuje ci eliksir za darmo." [ 
        ("Wypij eliksir", "drink"), 
        ("Zapytaj o eliksir", "ask_innkeeper"),
        ("Wyjdź", "village")
    ] (\ps -> do
        putStrLn "W karczmie panuje półmrok. Kilku miejscowych siedzi w kącie i szepce."
        putStrLn "Karczmarz nalewa zielonkawy eliksir do kubka..."
        return ps
    ) False Nothing False,

    Room "ask_innkeeper" "Rozmawiasz z karczmarzem..." [ 
        ("Wypij eliksir", "drink"), 
        ("Wyjdź", "village")
    ] (\ps -> do
        putStrLn "Karczmarz mruczy: 'To specjalny napój... da ci siły na drogę.'"
        putStrLn "Jego oczy błyszczą podejrzliwie w półmroku."
        return ps
    ) False Nothing False,

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
    ) False Nothing False,

    Room "elder" "Starzec z długą brodą patrzy na ciebie mądrymi oczami." [ 
        ("Idź do gór", "mountains"), 
        ("Zostań w wiosce", "village")
    ] (\ps -> do
        putStrLn "Starzec mówi powoli: 'Talizman... tak, znam tę legendę.'"
        putStrLn "'Znajduje się w jaskini trolla w górach. Ale uważaj, młody wojowniku...'"
        putStrLn "'Troll jest potężny. Będziesz potrzebował dobrej broni, by go pokonać.'"
        return ps
    ) False Nothing False,

    -- PODZIEMIA - POZIOM 1
    Room "underground_entrance" "Stoisz w wielkiej komorze oświetlonej kryształami. To wejście do podziemi." [
        ("Idź do Kryształowej Komory", "crystal_chamber"),
        ("Idź do Sali Szkieletów", "skeleton_hall"),
        ("Wróć na powierzchnię (użyj Talizmanu)", "fight_troll")
    ] (\ps -> do
        putStrLn $ "Jesteś na poziomie " ++ show (currentLevel ps) ++ " podziemi."
        putStrLn $ "Masz " ++ show (undergroundKeys ps) ++ " kluczy podziemi."
        let currentLvl = findLevel (currentLevel ps) undergroundLevels
        case currentLvl of
            Just lvl -> do
                putStrLn $ "Na tym poziomie potrzebujesz " ++ show (requiredKeys lvl) ++ " kluczy do przejścia dalej."
                if canDescendLevel ps lvl && isJust (nextLevel lvl)
                then putStrLn "Możesz zejść na następny poziom!"
                else return ()
            Nothing -> return ()
        return ps
    ) False Nothing True,

    Room "crystal_chamber" "Komora pełna świecących kryształów. W centrum stoi potężny Kryształowy Strażnik." [
        ("Walcz ze Strażnikiem!", "fight_crystal_guardian"),
        ("Rozwiąż zagadkę kryształów", "crystal_riddle"),
        ("Wróć do wejścia", "underground_entrance")
    ] return False (Just (riddlePuzzles !! 0)) True,

    Room "crystal_riddle" "Zagadka Kryształów" [
        ("Wróć do komory", "crystal_chamber")
    ] (\ps -> solvePuzzle (riddlePuzzles !! 0) ps) False Nothing True,

    Room "fight_crystal_guardian" "Walka z Kryształowym Strażnikiem!" [
        ("Wróć do komory", "crystal_chamber")
    ] (\ps -> do
        if isEnemyDefeated "Kryształowy Strażnik" ps
        then do
            putStrLn "Strażnik został już pokonany."
            return ps
        else
            fightSequence crystalGuardianEnemy ps
    ) False Nothing True,

    Room "skeleton_hall" "Długi korytarz wypełniony kośćmi starożytnych wojowników." [
        ("Przeszukaj szczątki", "search_bones"),
        ("Idź dalej głębiej", "descend_level2"),
        ("Wróć do wejścia", "underground_entrance")
    ] return False Nothing True,

    Room "search_bones" "Przeszukujesz starożytne szczątki..." [
        ("Wróć do sali", "skeleton_hall")
    ] (\ps -> do
        if hasVisited "search_bones" ps
        then do
            putStrLn "Nie ma tu już nic więcej."
            return ps
        else do
            roll <- randomRIO (1, 10) :: IO Int
            if roll <= 7
            then do
                putStrLn "Znajdujesz Klucz Podziemi wśród kości!"
                return $ markVisited "search_bones" $ ps { undergroundKeys = undergroundKeys ps + 1 }
            else do
                putStrLn "Nic wartościowego tutaj nie ma."
                return $ markVisited "search_bones" ps
    ) False Nothing True,

    Room "descend_level2" "Próbujesz zejść głębiej..." [
        ("Wróć do sali", "skeleton_hall"),
        ("Zejdź na poziom 2", "deeper_tunnel")
    ] (\ps -> do
        let currentLvl = findLevel 1 undergroundLevels
        case currentLvl of
            Just lvl -> do
                if canDescendLevel ps lvl
                then do
                    putStrLn "Używasz kluczy i otwierasz przejście na następny poziom!"
                    return ps { currentLevel = 2, undergroundKeys = undergroundKeys ps - requiredKeys lvl }
                else do
                    putStrLn $ "Potrzebujesz " ++ show (requiredKeys lvl) ++ " kluczy, aby przejść dalej!"
                    putStrLn $ "Masz tylko " ++ show (undergroundKeys ps) ++ " kluczy."
                    return ps
            Nothing -> return ps
    ) False Nothing True,

    -- PODZIEMIA - POZIOM 2
    Room "deeper_tunnel" "Głębsze tunele. Powietrze jest gęste od magii." [
        ("Idź do Labiryntu Cieni", "shadow_maze"),
        ("Idź do Starożytnej Biblioteki", "ancient_library"),
        ("Wróć na poziom 1", "underground_entrance")
    ] (\ps -> do
        putStrLn $ "Jesteś na poziomie " ++ show (currentLevel ps) ++ " podziemi."
        putStrLn $ "Masz " ++ show (undergroundKeys ps) ++ " kluczy podziemi."
        let currentLvl = findLevel (currentLevel ps) undergroundLevels
        case currentLvl of
            Just lvl -> do
                putStrLn $ "Na tym poziomie potrzebujesz " ++ show (requiredKeys lvl) ++ " kluczy do przejścia dalej."
                if canDescendLevel ps lvl && isJust (nextLevel lvl)
                then putStrLn "Możesz zejść na następny poziom!"
                else putStrLn "Musisz zdobyć więcej kluczy, aby zejść niżej."
            Nothing -> return ()
        return ps
    ) False Nothing True,

    Room "shadow_maze" "Jesteś w Labiryncie Cieni. Ciemność zdaje się żyć własnym życiem..." [
        ("Spróbuj znaleźć wyjście", "shadow_riddle"),
        ("Wróć do tunelu", "deeper_tunnel")
    ] return False Nothing True,

    Room "shadow_riddle" "Zagadka Cienia" [
        ("Wróć do labiryntu", "shadow_maze")
    ] (\ps -> solvePuzzle (riddlePuzzles !! 1) ps) False (Just (riddlePuzzles !! 1)) True,

    Room "ancient_library" "Starożytna Biblioteka pełna zapomnianych ksiąg i magicznych artefaktów." [
        ("Przeszukaj półki", "library_search"),
        ("Wróć do tunelu", "deeper_tunnel")
    ] return False Nothing True,

    Room "library_search" "Szukasz wśród zakurzonych tomów..." [
        ("Wróć do biblioteki", "ancient_library")
    ] (\ps -> do
        if hasVisited "library_search" ps
        then do
            putStrLn "Już wcześniej przeszukałeś to miejsce."
            return ps
        else do
            putStrLn "Znajdujesz Pierścień Mocy! Czujesz, jak twoja siła rośnie."
            return $ markVisited "library_search" $ addItem "Pierścień Mocy" ps
    ) False Nothing True,

    Room "fight_shadow_lord" "Walka z Władcą Cienia!" [
        ("Wróć do tunelu", "deeper_tunnel")
    ] (\ps -> do
        if isEnemyDefeated "Władca Cienia" ps
        then do
            putStrLn "Pokonałeś już Władcę Cienia. Ciemność słabnie..."
            return ps
        else
            fightSequence shadowLordEnemy ps
    ) False Nothing True,


    Room "final_chamber" "Ostatnia komnata. Przed tobą stoi Strażnik Korony Władzy." [
        ("Walcz ze Strażnikiem!", "fight_crown_guardian"),
        ("Wróć na poziom 2", "deeper_tunnel")
    ] return False Nothing True,

    Room "fight_crown_guardian" "Walka z Finałowym Strażnikiem!" [
        ("Wróć do komnaty", "final_chamber"),
        ("Idź do sali tronowej", "throne_room")
    ] (\ps -> do
        if isEnemyDefeated "Strażnik Korony" ps
        then do
            putStrLn "Strażnik został już pokonany. Droga do tronu jest wolna..."
            return ps
        else do
            result <- fightSequence crownGuardianEnemy ps
            if isEnemyDefeated "Strażnik Korony" result
            then do
                putStrLn "Pokonałeś Strażnika Korony! Drzwi do Sali Tronowej otwierają się z hukiem..."
                return result
            else return result
    ) False Nothing True,

    Room "throne_room" "Wkroczyłeś do Sali Tronowej. Na piedestale lśni Korona Władzy." [
        ("Zasiądź na tronie i załóż koronę", "game_end")
    ] (\ps -> do
        if hasCrown ps
        then do
            putStrLn "Już zdobyłeś Koronę. Tron czeka na twe decyzje..."
            return ps
        else do
            putStrLn "Zbliżasz się do Korony... Jej moc przenika twoją duszę."
            putStrLn "Zakładasz ją na głowę. Czujesz przypływ siły i przeznaczenia."
            return ps { hasCrown = True }
    )
    True Nothing True,

    Room "game_end" "Zasiadasz na tronie. Korona Władzy błyszczy na twej głowie. Przeznaczenie się wypełniło." 
    [] return False Nothing True
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