import System.IO
import System.Random
import Data.Maybe
import qualified Data.Map as Map

-- Typy danych
type RoomID = String
type Item = String

data Room = Room
    { roomId   :: RoomID
    , desc     :: String
    , options  :: [(String, RoomID)] -- opis opcji + ID pokoju docelowego
    , effect   :: PlayerState -> IO PlayerState -- efekt wejścia do pokoju
    , isEnd    :: Bool
    }

type World = Map.Map RoomID Room

data PlayerState = PlayerState
    { location :: RoomID
    , inventory :: [Item]
    , alive :: Bool
    }

-- Funkcja pomocnicza: dodaj przedmiot do inwentarza
addItem :: Item -> PlayerState -> PlayerState
addItem item ps = ps { inventory = item : inventory ps }

-- Funkcja pomocnicza: pokaz inwentarz
showInventory :: PlayerState -> IO ()
showInventory ps = do
    putStrLn "\nTwój ekwipunek:"
    if null (inventory ps)
        then putStrLn "- (pusty)"
        else mapM_ (\i -> putStrLn ("- " ++ i)) (inventory ps)

-- Świat gry
world :: World
world = Map.fromList $ map (\r -> (roomId r, r))
    [ Room "start" "Stoisz na rozdrożu w magicznej krainie."
        [ ("Idź na północ do gór", "mountains")
        , ("Idź na wschód do lasu", "forest")
        , ("Idź na południe do wioski", "village")
        ] return False

    , Room "mountains" "Wchodzisz w góry. W oddali słychać trolla."
        [ ("Wejdź do jaskini trolla", "troll")
        , ("Wróć na rozdroże", "start")
        ] return False

    , Room "troll" "Troll cię zauważył!"
        [ ("Walcz!", "fight")
        , ("Uciekaj do gór", "mountains")
        ] return False

    , Room "fight" "Zmierzasz się z trollem!"
        [ ("Zakończ przygodę", "end")
        , ("Wróć do gór", "mountains")
        ] (\ps -> do
              roll <- randomRIO (1, 10) :: IO Int
              if roll <= 6
              then do
                  putStrLn "Pokonałeś trolla i zdobywasz Talizman!"
                  return $ addItem "Talizman" ps
              else do
                  putStrLn "Troll cię pokonał..."
                  return ps { alive = False }
           )
        False

    , Room "forest" "Mglisty las otacza cię tajemnicą."
        [ ("Idź za światłem", "witch")
        , ("Wróć na rozdroże", "start")
        ] return False

    , Room "witch" "Wiedźma proponuje ci losowy dar."
        [ ("Przyjmij dar", "gift")
        , ("Uciekaj!", "forest")
        ] return False

    , Room "gift" "Wiedźma rzuca czar..."
        [ ("Wróć do lasu", "forest")
        ] (\ps -> do
              roll <- randomRIO (1, 10) :: IO Int
              if roll <= 4
              then do
                  putStrLn "Otrzymujesz magiczny amulet!"
                  return $ addItem "Amulet" ps
              else do
                  putStrLn "Wiedźma okazała się demonem! Tracisz życie."
                  return ps { alive = False }
           ) False

    , Room "village" "Wieśniacy patrzą podejrzliwie."
        [ ("Odpocznij w karczmie", "inn")
        , ("Zapytaj starca o Talizman", "elder")
        , ("Wróć na rozdroże", "start")
        ] return False

    , Room "inn" "Karczmarz oferuje eliksir za darmo. Przyjmujesz?"
        [ ("Wypij", "drink")
        , ("Wyjdź", "village")
        ] return False

    , Room "drink" "Czujesz się dziwnie..."
        [ ("Wróć do wioski", "village")
        ] (\ps -> do
              roll <- randomRIO (1, 10) :: IO Int
              if roll <= 3
              then do
                  putStrLn "Eliksir był zatruty!"
                  return ps { alive = False }
              else do
                  putStrLn "Odzyskałeś siły!"
                  return ps
           ) False

    , Room "elder" "Starzec mówi, że Talizman jest w jaskini trolla."
        [ ("Idź do gór", "mountains")
        , ("Zostań w wiosce", "village")
        ] return False

    , Room "end" "Zwycięstwo! Z Talizmanem w ręku kończysz przygodę." [] return True
    ]

-- Główna pętla gry
play :: PlayerState -> World -> IO ()
play ps worldMap = do
    let currentRoom = fromJust (Map.lookup (location ps) worldMap)
    putStrLn $ "\n== " ++ roomId currentRoom ++ " =="
    putStrLn $ desc currentRoom

    -- Efekt pokoju
    ps' <- effect currentRoom ps
    if not (alive ps')
    then putStrLn "\nZginąłeś. KONIEC GRY."
    else if isEnd currentRoom
    then putStrLn "\nKONIEC GRY."
    else do
        showInventory ps'
        mapM_ (\(i, (desc, _)) -> putStrLn $ show i ++ ". " ++ desc) (zip [1..] (options currentRoom))
        putStr "Twój wybór: "
        hFlush stdout
        choice <- getLine
        let maybeIndex = reads choice :: [(Int, String)]
        case maybeIndex of
            [(n, _)] | n > 0 && n <= length (options currentRoom) -> do
                let (_, nextRoomId) = options currentRoom !! (n - 1)
                play ps' { location = nextRoomId } worldMap
            _ -> do
                putStrLn "Nieprawidłowy wybór. Spróbuj jeszcze raz."
                play ps' worldMap

-- Start gry
main :: IO ()
main = do
    putStrLn "=== Przygoda: Talizman Losu ==="
    let initialState = PlayerState { location = "start", inventory = [], alive = True }
    play initialState world
