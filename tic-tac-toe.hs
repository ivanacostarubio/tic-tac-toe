import Control.Applicative

data Board = Board [String]
           | Error
    deriving (Show)

data Player = X
            | O

data Position = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | PError
    deriving (Show)

data GameStatus = Won | Undecided
    deriving (Show)

checkWinningCondition :: Board -> GameStatus
checkWinningCondition (Board xs)
    | allTheSame (firstElement xs ) = Won
    | allTheSame (secondElement xs) = Won
    | allTheSame (thirdElement xs) = Won
    | allTheSame (firstElements xs) = Won
    | allTheSame (secondElements xs) = Won
    | allTheSame (thirdElements xs) = Won
    | allTheSame (lDiagonal xs) = Won
    | allTheSame (rDiagonal xs) = Won
    | otherwise = Undecided

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)

positionFromString :: String -> Position
positionFromString "1" = One
positionFromString "2" = Two
positionFromString "3" = Three
positionFromString "4" = Four
positionFromString "5" = Five
positionFromString "6" = Six
positionFromString "7" = Seven
positionFromString "8" = Eight
positionFromString "9" = Nine
positionFromString _ = PError

buildEmptyBoard :: Board
buildEmptyBoard = Board ["123", "456", "789"]

stringFromBoard :: Board -> [String]
stringFromBoard (Board xs ) = xs

replacePosition :: ([String], Position, String) -> [String]
-- first row
replacePosition (x,One, z) = [ appendToFirst(z, head x), secondElement(x), thirdElement(x) ]
replacePosition (x,Two, z) = [ appendToSecond(z, head x), secondElement(x), thirdElement(x) ]
replacePosition (x,Three, z) = [ appendToThird(z, head x), secondElement(x), thirdElement(x) ]
-- second row
replacePosition (x, Four, z) = [ firstElement(x), appendToFirst(z, secondElement(x)), thirdElement(x) ]
replacePosition (x, Five, z) = [ firstElement(x), appendToSecond(z, secondElement(x)), thirdElement(x) ]
replacePosition (x, Six, z) = [ firstElement(x), appendToThird(z, secondElement(x)), thirdElement(x) ]
-- third row
replacePosition (x, Seven, z) = [ firstElement(x), secondElement(x), appendToFirst(z, thirdElement(x)) ]
replacePosition (x, Eight, z) = [ firstElement(x), secondElement(x), appendToSecond(z, thirdElement(x)) ]
replacePosition (x, Nine, z) = [ firstElement(x), secondElement(x), appendToThird(z, thirdElement(x))]

appendToFirst :: (String, String) -> String
appendToFirst (x,y) = x ++ (drop 1 y)
appendToSecond :: (String, String) -> String
appendToSecond (x,y) = (take 1 y) ++ x ++ (drop 2 y)
appendToThird :: (String, String) -> String
appendToThird (x,y) = (take 2 y)++ x

firstElement :: [String] -> String
firstElement x = head x
thirdElement :: [String] -> String
thirdElement x = unwords (drop 2 x)
secondElement :: [String] -> String
secondElement x = head (drop 1 x)

firstElements :: [String] -> String
firstElements (x:xs) = head x : firstElements xs
firstElements x = []

secondElements :: [String] -> String
secondElements (x:xs) = head (drop 1 x) : secondElements xs
secondElements x = []

thirdElements :: [String] -> String
thirdElements (x:xs) =  head (reverse(x)) : thirdElements xs
thirdElements x = []

lDiagonal :: [String] -> String
lDiagonal (x:y:z:[]) =  (head x) : head (drop 1 y) : head( reverse z) : []

rDiagonal :: [String] -> String
rDiagonal (x:y:z:[]) =  head(reverse(x)) : head (drop 1 y) : head z : []



buildBoard :: String -> Board
buildBoard (x) = Board [(take 3 x ), (take 3 (drop 3 x)),  reverse(take 3 (reverse x))]
-- TODO: Cover case when board is bigger than expected
-- This has a board represented as: "........." or "x..o..x.."

bbuildBoard :: [String] -> Board
bbuildBoard(x) = Board x

move :: (Board, Player, Position) -> Board
move (x,X,p) = bbuildBoard(replacePosition(stringFromBoard(x), p, "x"))
move (x,O,p) = bbuildBoard(replacePosition(stringFromBoard(x), p, "o"))

otherPlayer :: Player -> Player
otherPlayer X = O
otherPlayer O = X

--
-- TODO: Check that a player can't move to a taken position
--
--
--
askUserForInput :: (Board, Player) -> IO()
askUserForInput (y, z) = do
    m <- getLine
    case positionFromString(m) of
      PError -> gameLoop(y,z)
      pp -> gameLoop(move(y,z,pp), otherPlayer(z))


gameLoop :: (Board, Player) -> IO()
gameLoop (board,z) = do
    print board
    putStrLn "waiting for your move"
    putStrLn "....................."
    case checkWinningCondition board of
        Won -> putStrLn "You won"
        Undecided -> askUserForInput(board,z)

main = do
    putStrLn "Welcome to Tic Tac Toe"
    putStrLn "......................"
    gameLoop(buildEmptyBoard, X)
