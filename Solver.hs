import Data.List 
import System.Environment
import System.IO
import System.Console.GetOpt
import Debug.Trace


type Game  = (Color,Board)
type Board = [[Color]]
data Color = Red | Yellow deriving (Show, Eq)
type Move = Int
data Winner = Won Color | Tie deriving (Show, Eq)
type Rating = Int

-- Main, Flags
data Flag = Help | FindWinner | Depth Int | DoMove String deriving (Show, Eq)

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit.",
            Option ['w'] ["winner"] (NoArg FindWinner) "Finds the definitive best move.",
            Option ['m'] ["move"] (ReqArg DoMove "<move>") "Do move <move> on the board.",
            Option ['d'] ["depth"] (ReqArg (Depth . read) "<num>") "Specify cutoff depth <num>"            
          ]

main :: IO ()
main = 
    do args <- getArgs  
       let (flags, inputs, errors) = getOpt Permute options args
       if Help `elem` flags
       then putStrLn $ usageInfo "Solver [options] [filename]\nConnect Four Solver." options
       else
         do filepath <- getFileName inputs
            loadResult <- loadGame filepath
            case loadResult of 
              Just game -> dispatch flags game
              Nothing -> putStrLn "Failed to load game"

dispatch :: [Flag] -> Game -> IO ()
dispatch flags game
  | FindWinner `elem` flags   = putBestMove game
  | any isDoMove flags        = putDoMove game (getMove flags)  
  | otherwise                 = putGoodMove game (getDepth flags)
  
-- 
-- Story 2
-- 

-- Function to check who has won the game state.
checkWinner :: Game -> Maybe Winner
checkWinner game@(_,brd) = 
  let winnerLst = checkVertical brd ++ (checkHorizontal brd) ++ (checkDiagonal brd)
  in if winnerLst == [] then if validMoves game == [] then Just Tie
                             else Nothing
     else if Red `elem` winnerLst
          then if Yellow `elem` winnerLst then Just Tie
               else Just (Won Red)
          else Just (Won Yellow)

--Checks for horizontal wins

checkFour (Red:_) (Red:_) (Red:_) (Red:_) = [Red]
checkFour (Yellow:_) (Yellow:_) (Yellow:_) (Yellow:_) = [Yellow]
checkFour (_:xs) (_:ys) (_:zs) (_:ws) = checkFour xs ys zs ws
checkFour _ _ _ _ = []

checkHorizontal (xs:ys:zs:ws:rest) = checkFour xs ys zs ws ++ checkHorizontal (ys:zs:ws:rest)
checkHorizontal _ = []

-- Checks one column for a win, returning a list of winning colors
checkColumn col =
  let aux 4 clr _ = [clr]
      aux _ _ [] = []
      aux i clr (x:xs) =
        if x == clr then aux (i+1) x xs
        else aux 1 x xs
  in if col == [] then []
     else aux 1 (head col) (tail col)

-- Checks all columns for a win, returning a list of winning colors
checkVertical brd = concat (map checkColumn brd)

-- Checks board for diagonals using checkFour, returning list of winning colors
checkDiagonal :: Board -> [Color]
checkDiagonal (xs:ys:zs:ws:rest) = 
    checkFour xs (drop 1 ys) (drop 2 zs) (drop 3 ws)
    ++ checkFour (drop 3 xs) (drop 2 ys) (drop 1 zs) ws
    ++ checkDiagonal (ys:zs:ws:rest)
checkDiagonal _ = []
-- 
-- End of Story 2
-- 

-- 
-- Story 3
-- 

-- Simple function to make a move by dropping a piece into the specified column
makeMove :: Game -> Move -> Game
makeMove (currentColor, board) move =
  -- It's the step we split the array to insert the color for the move.
  case splitAt move board of
    (leftCols, (column:rightCols)) -> let updatedColumn = dropPiece currentColor column
      in (nextColor currentColor, leftCols ++ (updatedColumn : rightCols)) -- Return new col
    _ -> error "Invalid move"
    
-- Drops a piece into the first available position in a column
dropPiece :: Color -> [Color] -> [Color]
dropPiece color column = 
  if length column < 6
  then column ++ [color]
  else error "Invalid move"

-- Function to switch to the next player's color. I made it since we need to deicide who's next.
nextColor :: Color -> Color
nextColor Red = Yellow
nextColor Yellow = Red
-- 
-- End of Story 3
-- 

-- 
-- Story 4
-- 

-- Helper function for validMoves
position :: Board -> Int -> [Move] -> [Move]
position [] index acc = acc
position (x:xs) index acc  
    |length x == 6 = position xs (index+1) acc
    |otherwise     = position xs (index+1) (index:acc)
    
validMoves :: Game -> [Move]
validMoves (_,board) = reverse (position board 0 [])
-- 
-- End of Story 4
-- 

-- 
-- Story 5 
-- 

-- Takes a board of Colors and turns it into a board of Maybe Colors.
-- If it encounters a column that isn't yet filled to a length of 6, then Nothings are used to fill it up.
maybeinator :: Board -> [[Maybe Color]]
maybeinator [] = [] 
maybeinator (x:xs) = 
 let aux _ 6 = []
     aux [] num = Nothing : aux [] (num+1)
     aux (y:ys) num  
        | y == Yellow = Just Yellow : aux ys (num+1)
        | otherwise   = Just Red : aux ys (num+1) 
 in aux x 0 : maybeinator xs

-- Converts the board into a single String. 
-- Without a proper IO function, the string of the game state is printed on a single line. 
showRows :: [[Maybe Color]] -> String
showRows [] = []
showRows (x:xs) = showRow x ++ "\n" ++ showRows xs
 where showRow [] = ""
       showRow (y:ys)  
            | y == Just Red     = " o" ++ showRow ys
            | y == Just Yellow  = " x" ++ showRow ys
            | otherwise         = " ." ++ showRow ys

-- Combines the code of maybeinator and showRows to print out the current state of the board and whose turn it is.
prettyPrint :: Game -> String
prettyPrint (curr,brd) = "Current players turn: " ++ show curr ++ "\n" ++ showRows rows
    -- The transpose function turns the board from a list of columns to a list of rows, its a handy imported function.
    where rows = reverse $ transpose $ maybeinator $ brd
-- 
-- End of Story 5
-- 

-- 
-- Story 7 has already been done, since Connect 4 is already has bounded depth.
-- 

-- 
-- Story 8 has already been finished.
-- 

-- 
-- Story 9 
-- 

-- Chooses the move with the best outcome for the current player by using validMoves to consider all the 
-- valid moves, and each move's resulting game state.
whoWillWin :: Game -> Winner
whoWillWin game@(color,_) =  
    case checkWinner game of
       Just w -> w 
       Nothing -> 
            let nextGames = map (makeMove game) (validMoves game) 
                options = map whoWillWin nextGames
            in if (Won color) `elem` options 
               then (Won color) 
               else if Tie `elem` options 
                    then Tie 
                    else (Won (nextColor color))
-- 
-- End of Story 9
--           
                
-- 
-- Story 10
--               
bestMove :: Game -> Maybe Move
bestMove game@(color,_) = 
    let moves = validMoves game
        moveResultPairs = map (\x -> (x,whoWillWin (makeMove game x))) moves
        winningMove = keyByVal (Won color) moveResultPairs
    in case winningMove of
       Just x  -> Just x
       Nothing -> 
         let tyingMove = keyByVal Tie moveResultPairs
         in case tyingMove of
            Just x -> Just x
            Nothing -> keyByVal (Won (nextColor color)) moveResultPairs
          
keyByVal x [] = Nothing
keyByVal x ((key,val):pairs) =
  if val == x
  then Just key
  else keyByVal x pairs
-- 
-- End of Story 10
--           

-- 
-- Story 11:    String format of game: "o\nxox\nox\nxoxx\nxxxo\nooxxo\noxxoo\nx\n"
--                  where the first 'o' or 'x' is the current player and each column is 
--                  separated by a \n (newline)
--

-- 
-- Story 12
--
readGame :: String -> Maybe Game       
readGame file =
  case length strs of
  8 -> 
    let colorStr = head strs
        boardStrs = tail strs
        color = colorFromStr colorStr
        boardMaybeColors = map (map colorFromChar) boardStrs
        boardMaybeColumns = map catNoNothings boardMaybeColors
        board = validBoard (catNoNothings boardMaybeColumns)
    in case (color,board) of
       (Nothing, _) -> Nothing
       (_, Nothing) -> Nothing
       (Just x, Just y) -> Just (x,y)
  otherwise ->  Nothing
  where strs = lines file
        colorFromStr str =
          case str of
          "o" -> Just Red
          "x" -> Just Yellow
          otherwise -> Nothing
        colorFromChar c = 
          case c of
          'o' -> Just Red
          'x' -> Just Yellow
          otherwise -> Nothing

catNoNothings :: [Maybe a] -> Maybe [a]
catNoNothings [] = Just []
catNoNothings (x:xs) =
  case (catNoNothings xs,x) of
  (Nothing, _) -> Nothing
  (_, Nothing) -> Nothing
  (Just lst, Just x) -> Just (x:lst)

validBoard board = 
  case board of
  Nothing -> Nothing
  Just xs -> if length (filter (\x -> length x < 7) xs) == 7
             then Just xs
             else Nothing
-- 
-- End of Story 12
--      

-- 
-- Story 13
--

-- Turns a game state into a string in order to load a game into a file.
-- The resulting text format: "o\nxox\nox\nxoxx\nxxxo\nooxxo\noxxoo\nx\n"
-- The first character ('o' or 'x') is the current player, while eveything else belongs to a column.
showGame :: Game -> String
showGame (player,board) = unlines $ [showColor player]:[showColumn col | col <- board]
  where showColumn col = [showColor color | color <- col]

-- Helper function for showGame. Turns a color into a character
showColor :: Color -> Char
showColor Red = 'o'
showColor Yellow = 'x'
-- 
-- End of Story 13
--  

-- 
-- Story 14
-- 
writegame :: Game -> FilePath -> IO ()
writegame game filePath = writeFile filePath (showGame game)

loadGame :: FilePath -> IO (Maybe Game)
loadGame filePath = do
        cont <- readFile filePath 
        case readGame cont of
                Just game -> return (Just game)
                Nothing -> do
                        putStrLn "Invalid game format"
                        return Nothing                 

putBestMove :: Game -> IO ()
putBestMove game = do
        case bestMove game of
                Just move -> do
                        let winner = whoWillWin game
                        putStrLn $ "The best move is " ++ show (move + 1)
                        putStrLn $ "Winner " ++ show winner
                Nothing -> putStrLn "Game is complete"

getFileName :: [String] -> IO String
getFileName (x:xs) = return x
getFileName [] = do putStr "Enter the file path:"
                    hFlush stdout
                    answer <- getLine
                    return answer
-- 
-- End of Story 14
--  

-- 
-- Story 15:      Already created files with test cases to satisfy story 15
--

-- 
-- Story 16
-- 
eqLists :: (Eq a) => [a] -> [a] -> Bool
eqLists xs ys = null (xs \\ ys) && null (ys \\ xs)

-- Test Cases for `validMoves`
-- testValidMoves :: Bool
-- testValidMoves =
--   let board1 = replicate 7 []  -- Empty board
--       board2 = [[], [Red], [Red, Yellow], [], [], [Red, Red, Yellow], [Red, Red, Yellow, Yellow]]
--       board3 = replicate 7 [Red, Yellow, Red, Yellow, Red, Yellow]  -- Full board
--   in eqLists (validMoves (Red, board1)) [0..6] &&
--      eqLists (validMoves (Yellow, board2)) [0, 1, 3, 4] &&
--      null (validMoves (Red, board3))

-- -- Test Cases for `checkWinner`
-- testCheckWinner :: Bool
-- testCheckWinner =
--   let boardHorizontalWin = [[Red, Red, Red, Red], [], [], [], [], [], []]
--       boardVerticalWin = [[Red], [Red], [Red], [Red], [], [], []]
--       boardDiagonalWin = [[Red], [Yellow, Red], [Yellow, Yellow, Red], [Yellow, Yellow, Yellow, Red], [], [], []]
--       boardTie = replicate 7 [Red, Yellow, Red, Yellow, Red, Yellow]
--       boardOngoing = [[Red, Yellow], [Yellow, Red], [], [], [], [], []]
--   in checkWinner (Red, boardHorizontalWin) == Just (Won Red) &&
--      checkWinner (Yellow, boardVerticalWin) == Just (Won Red) &&
--      checkWinner (Red, boardDiagonalWin) == Just (Won Red) &&
--      checkWinner (Red, boardTie) == Just Tie &&
--      checkWinner (Yellow, boardOngoing) == Nothing

-- Test Cases for `makeMove`
--testMakeMove :: Bool
--testMakeMove =
--  let game1 = (Red, replicate 7 [])  -- Empty board
--      game2 = (Yellow, [[Red], [Red, Yellow], [Yellow, Yellow], [], [], [], []])
--      move1 = 0
--      move2 = 3
--      moveInvalid = 7  -- Invalid move (out of range)
--      game1Result = makeMove game1 move1
--      game2Result = makeMove game2 move2
--  in snd game1Result !! move1 == [Red] &&
--     snd game2Result !! move2 == [Yellow] &&
--     (makeMove game2 moveInvalid `seq` False) `catch` (\_ -> True)  -- Expect an error

-- Test Cases for `whoWillWin`
-- testWhoWillWin :: Bool
-- testWhoWillWin =
--   let gameWin = (Red, [[Red], [Red], [Red], []])  -- Immediate win
--       gameLose = (Yellow, [[Yellow], [Yellow], [Yellow], []])  -- Opponent will win
--       gameTie = (Red, replicate 7 [Red, Yellow, Red, Yellow, Red, Yellow])  -- Tie
--   in whoWillWin gameWin == Won Red &&
--      whoWillWin gameLose == Won Yellow &&
--      whoWillWin gameTie == Tie

-- -- Test Cases for `bestMove`
-- testBestMove :: Bool
-- testBestMove =
--   let gameImmediateWin = (Red, [[Red], [Red], [Red], [], [], [], []])
--       gameBlock = (Yellow, [[Yellow], [Yellow], [Yellow], [], [], [], []])
--       gameTie = (Red, replicate 7 [Red, Yellow, Red, Yellow, Red, Yellow])
--   in bestMove gameImmediateWin == Just 3 &&
--      bestMove gameBlock == Just 3 &&
--      bestMove gameTie == Nothing

-- -- Test Cases for `showGame` and `readGame`
-- testShowReadGame :: Bool
-- testShowReadGame =
--   let game = (Red, [[Red], [Yellow, Red], [Yellow, Yellow, Red], [], [], [], []])
--       gameString = showGame game
--   in readGame gameString == Just game

-- -- Combined Test Runner
-- runTests :: IO ()
-- runTests = do
--   putStrLn "Testing validMoves..."
--   print testValidMoves
--   putStrLn "Testing checkWinner..."
--   print testCheckWinner
--   putStrLn "Testing makeMove..."
--   print testMakeMove
--   putStrLn "Testing whoWillWin..."
--   print testWhoWillWin
--   putStrLn "Testing bestMove..."
--   print testBestMove
--   putStrLn "Testing showGame and readGame..."
--   print testShowReadGame
-- 
-- End of Story 16
--  

-- 
-- Story 17
--

-- Rates all possible connect 4. If it is made of 2 different pieces, then the score is 0. 
-- If it has only 1 color, then add a point for every piece that is of that color.
-- Red is a positive score and Yellow is a negative score.
rateGame :: Game -> Rating
rateGame game@(_,brd) 
  | checkWinner game == Just (Won Red)      = 10000000
  | checkWinner game == Just (Won Yellow)   = -10000000
  | otherwise                               = rateVertical board + rateHorizontal board + rateDiags board
    where board = maybeinator brd

-- Gives a rating for a list of four Colors, adding a point for each Color of the same type.
count ::  Color -> [Maybe Color] -> Rating
count color [] = 0
count color (x:xs) = 
  if x == Just color 
    then 1 + count color xs
    else count color xs

-- Takes a list of four Colors and computes their rating. Red is a positive score and 
-- Yellow is a negative score. If the list has Red and Yellow in it than it doesn't score.
rateFour :: [Maybe Color] -> Rating
rateFour list 
  | Just Red `elem` list && Just Yellow `elem` list = 0
  | otherwise                                       = count Red list - count Yellow list

-- Used to determine a rating amongst multiple columns. 
-- Used by the row rating function and diagonal rating function.
rateFourColumns :: [Maybe Color] -> [Maybe Color] -> [Maybe Color] -> [Maybe Color] -> Rating
rateFourColumns (x:xs) (y:ys) (z:zs) (w:ws) = rateFour [x,y,z,w] + rateFourColumns xs ys zs ws 
rateFourColumns _ _ _ _ = 0

-- Rates all of the rows
rateHorizontal :: [[Maybe Color]] -> Rating 
rateHorizontal (xs:ys:zs:ws:rest) = rateFourColumns xs ys zs ws + rateHorizontal (ys:zs:ws:rest)
rateHorizontal _ = 0

-- Rates a single column
rateColumn :: [Maybe Color] -> Rating
rateColumn (x:y:z:w:rest) = rateFour [x,y,z,w] + rateColumn (y:z:w:rest)
rateColumn _ = 0

-- Rates all of the columns
rateVertical :: [[Maybe Color]] -> Rating
rateVertical board = sum [rateColumn col | col <- board]

-- Rates all of the diagonals
rateDiags :: [[Maybe Color]] -> Rating
rateDiags (xs:ys:zs:ws:rest) =
  rateFourColumns xs (drop 1 ys) (drop 2 zs) (drop 3 ws)
  + rateFourColumns (drop 3 xs) (drop 2 ys) (drop 1 zs) ws
  + rateDiags (ys:zs:ws:rest)
rateDiags _ = 0
-- 
-- End of Story 17
--  

-- 
-- Story 18
whoMightWin :: Game -> Int -> (Rating, Maybe Move)
whoMightWin game@(color,_) depth 
    | depth == 0 || isTerminal game = (rateGame game, Nothing)
    | otherwise = 
        let moves = validMoves game
            res = [(move,fst $ whoMightWin (makeMove game move) (depth -1)) | move <- moves]
            bestRes = selectBestEarly color res
            --ratedMoves = [(adjustR color rating,Just move) | (move, (rating, _ )) <- res]
        in bestRes
adjustR :: Color -> Rating -> Rating 
adjustR color rating 
    |color == Red = rating 
    |color == Yellow = -rating
selectBest :: Color -> [(Rating , Maybe Move)] -> (Rating, Maybe Move) 
selectBest color ratedMoves
    |color == Red = maximumBy compareFst ratedMoves
    |color == Yellow = minimumBy compareFst ratedMoves
    where 
        compareFst (r1,_) (r2,_) = compare r1 r2
maxRate=10000000
minRate= -maxRate
selectBestEarly :: Color -> [(Move,Rating)] -> (Rating, Maybe Move) 
selectBestEarly color res = foldr betterResult ( initialR, Nothing) res 
    where initialR = if color == Red then minRate else maxRate 
          betterResult (move, rating) (bestRating, bestMove)
              | isWinning rating = (rating, Just move) 
              | otherwise        = maxOrMin (bestRating, bestMove) (rating , Just move) 
          maxOrMin = if color == Red then max else min 
          isWinning r = (color == Red && r==maxRate) || (color == Yellow && r == minRate)
isTerminal :: Game -> Bool 
isTerminal game = case checkWinner game of 
    Just _ -> True 
    Nothing -> False 

whoMightWinTest :: IO Bool
whoMightWinTest = 
  let depth = 4
      gameWin = (Red, [[Red], [Red], [Red], []]) 
      gameLose = (Yellow, [[Yellow], [Yellow], [Yellow], []]) 
      gameTie = (Red, replicate 7 [Red, Yellow, Red, Yellow, Red, Yellow])
      moveWin = whoMightWin gameWin depth 
      moveLost = whoMightWin gameLose depth 
      moveTie = whoMightWin gameTie depth
  in do
    print $ "Win test result: " ++ show moveWin
    print $ "Lose test result: " ++ show moveLost
    print $ "Tie test result:" ++ show moveTie
    return (fst moveWin == 10000000 && fst moveLost == -10000000 && fst moveTie == 0)
 
-- Story 19
--

-- 
-- Story 20
--

-- 
-- Story 21
--
putGoodMove :: Game -> Int -> IO ()
putGoodMove game n =
  let (_,goodMove) = whoMightWin game n
  in case goodMove of
     Nothing -> putStrLn "Game is already over"
     Just x -> putStrLn ("A good move is " ++ (show (x + 1)))
-- 
-- Story 22
--

-- 
-- Story 23
--
isDepthFlag :: Flag -> Bool
isDepthFlag (Depth _) = True
isDepthFlag _ = False

getDepth :: [Flag] -> Int
getDepth [] = 5 -- Default Depth if none specified
getDepth ((Depth d):_) =  d
getDepth (_:flags) = getDepth flags

-- 
-- Story 24
--

-- 
-- Story 25
--

isDoMove :: Flag -> Bool
isDoMove (DoMove _) = True
isDoMove _ = False

getMove :: [Flag] -> Int
getMove [] = 0
getMove ((DoMove x):_) = read x
getMove (_:flags) = getMove flags

putDoMove :: Game -> Move -> IO ()
putDoMove game move = 
  do putStrLn "New board:"
     putStrLn (showGame (makeMove game (move - 1)))
-- 
-- Story 26
--

-- 
-- Story 27
--
