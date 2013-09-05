import Control.Applicative

data Board = Board [String]
           | Error
    deriving (Show)

data Player = X
            | O

data Position = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
    deriving (Show)

data GameStatus = Won | Undecided
    deriving (Show)

checkStatus :: Board -> GameStatus
checkStatus (Board xs) = do
    -- TODO
    Won

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


buildBoard :: String -> Board
buildBoard (x) = Board [(take 3 x ), (take 3 (drop 3 x)),  (take 3 (reverse x))]
-- Cover case when board is bigger than expected
-- This has a board represented as: "........." or "x..o..x.."
--
bbuildBoard :: [String] -> Board
bbuildBoard(x) = Board x
--
--
-- A move : takes a board, a player and a position. Returns a new Board with the updated position
-- TODO: validate the move is valid. AKA, it is not taken by another player
move :: (Board, Player, Position) -> Board
move (x,X,p) = bbuildBoard(replacePosition(stringFromBoard(x), p, "x"))
move (x,O,p) = bbuildBoard(replacePosition(stringFromBoard(x), p, "o"))


gameLoop :: (String, Board, Player) -> IO()
gameLoop (x,y,z) = do
    print y
    putStrLn "waiting for your move"
    putStrLn "....................."
    -- TODO:  Make sure is a valid move?
    m <- getLine
    case z of
      X -> gameLoop(x, move(y, z, positionFromString(m)), O)
      O -> gameLoop(x, move(y, z, positionFromString(m)), X)

main = do
    putStrLn "Welcome to Tic Tac Toe"
    putStrLn "......................"
    gameLoop("none", buildEmptyBoard, X)
