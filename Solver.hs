-- implementation option 1
import Data.List 

type Game  = (Color,Board)
type Board = [[Color]]
data Color = Red | Yellow deriving (Show, Eq)
type Move = Int
data Winner = Won Color | Tie deriving (Show, Eq)

--Story 3

-- Simple function to make a move by dropping a piece into the specified column
makeMove :: Game -> Move -> Game
makeMove (currentColor, board) move =
  -- It's the step we split the array to insert the color for the move.
  let (leftCols, (column:rightCols)) = splitAt move board 
      -- I wrote the dropPiece which helps to insert the Color into the column
      updatedColumn = dropPiece currentColor column
  in (nextColor currentColor, leftCols ++ (updatedColumn : rightCols)) -- Return new col

-- Drops a piece into the first available position in a column
dropPiece :: Color -> [Color] -> [Color]
dropPiece color column = column ++ [color]

-- Function to switch to the next player's color. I made it since we need to deicide who's next.
nextColor :: Color -> Color
nextColor Red = Yellow
nextColor Yellow = Red


position :: Board -> Int ->[Move] -> [Move]
position [] index acc = acc
position (x:xs) index acc  
    |length x == 6 =  position xs (index+1) acc
    |otherwise     = position xs (index +1) (index:acc)
    

validMoves :: Game -> [Move]
validMoves (_,board) =reverse( position board 0 [])


checkWinner :: Game -> Maybe Winner
checkWinner game@(_,brd) = 
  let winnerLst = checkVertical brd ++ (checkHorizontal brd) ++ (checkDiagonal brd)
  in if winnerLst == [] then if validMoves game == [] then Just Tie
                             else Nothing
     else if Red `elem` winnerLst
          then if Yellow `elem` winnerLst then Just Tie
               else Just(Won Red)
          else Just (Won Yellow)

--checks the bottom row of a board for a win, returning a list of winning colors
checkRow brd =
  let aux 4 clr xs = [clr]
      aux 0 _ (x:xs) = if x == [] then aux 0 Red xs
                       else aux 1 (head x) xs
      aux i clr [] = []
      aux i clr (x:xs) =
        if x == [] then aux 0 Red xs
        else if head x == clr then aux (i+1) (head x) xs
             else aux 1 (head x) xs
  in if head brd == [] then aux 0 Red (tail brd) else aux 1 (head (head brd)) (tail brd)

--checks all rows for win, returning a list of winning colors
checkHorizontal brd = 
  if length (filter (\x -> length x > 0) brd) < 4 then []
  else (checkRow brd) ++ (checkHorizontal (map (\x -> if x == [] then [] else tail x) brd))

--checks one column for a win, returning list of winning colors
checkColumn col =
  let aux 4 clr _ = [clr]
      aux _ _ [] = []
      aux i clr (x:xs) =
        if x == clr then aux (i+1) x xs
        else aux 1 x xs
  in if col == [] then []
     else aux 1 (head col) (tail col)

--checks all columns for a win, returning a list of winning colors
checkVertical brd = concat (map checkColumn brd)


--checks if the bottom left 4x4 has a diagonal win this way /, returning list of winning colors
checkSquareOne :: Board -> [Color]
checkSquareOne brd =
  if length brd < 4 then []
  else let c1 = head brd
           c2 = head (tail brd)
           c3 = head (tail (tail brd))
           c4 = head (tail (tail (tail brd)))
       in if c1 == [] || length c2 < 2 || length c3 < 3 || length c4 < 4 then []
          else let clr = head c1
               in if head (tail c2) == clr && head (tail (tail c3)) == clr && head (tail (tail (tail c4))) == clr then [clr]
                  else []

--same as above but for diagonal this way \
checkSquareTwo :: Board -> [Color]
checkSquareTwo brd =
  if length brd < 4 then []
  else let c1 = head brd
           c2 = head (tail brd)
           c3 = head (tail (tail brd))
           c4 = head (tail (tail (tail brd)))
       in if c4 == [] || length c3 < 2 || length c2 < 3 || length c1 < 4 then []
          else let clr = head c4
               in if head (tail c3) == clr && head (tail (tail c2)) == clr && head (tail (tail (tail c1))) == clr then [clr]
                  else []

--checks board for diagonals in a bottom-up, left to right manner, returning list of winning colors
checkDiagonal :: Board -> [Color]
checkDiagonal brd = 
  if filter (\x -> length x > 0) brd == [] then []
  else let aux [] = []
           aux chunk = checkSquareOne chunk ++ (checkSquareTwo chunk) ++ (aux (tail chunk))
       in (aux brd) ++ (checkDiagonal (map (\x -> if x == [] then [] else tail x) brd))


-- Story 5 

-- takes a board and turns it into Maybe Colors, if a column isn't filled to its maximum length, 
-- then Nothings are used to fill it up
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
-- Without a proper IO function, the board is printed as a single string on a single line. 
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
-- Story 11
--

-- Turns a game state into a string in order to load a game into a file.
-- The resulting text format: "Red\nxox\nox\nxoxx\nxxxo\nooxxo\noxxoo\nx"
writeGame :: Game -> String
writeGame (player,columns) = show player ++ "\n" ++ writeColumns columns
  where writeColumns columns = unlines $ [ writeColumn col | col <- columns]

-- Helper function for writeGame. Turns a column into a string
writeColumn :: [Color] -> String
writeColumn [] = "" 
writeColumn (x:xs) 
  | x == Red  = "o" ++ writeColumn xs
  | otherwise = "x" ++ writeColumn xs
-- 
-- End of Story 11
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
  otherwise -> Nothing
  where strs = lines file
        colorFromStr str =
          case str of
          "Red" -> Just Red
          "Yellow" -> Just Yellow
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
-- Story 13
--

-- Turns a game state into a string in order to load a game into a file.
-- The resulting text format: "o\nxox\nox\nxoxx\nxxxo\nooxxo\noxxoo\nx\n"
-- The first character is the current player, while eveything else belongs to a column.
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
writegame :: Game -> FilePath -> IO ()
writegame game filePath = writeFile filePath (showGame game)

loadGame :: FilePath -> IO Game
loadGame filePath = do
	cont <- readFile filePath 
	case readGame cont of
		Just game -> return game
		Nothing -> do
			putStrLn "Invalid game format"
			error "Failed to load file"			

putBestMove :: Game -> IO ()
putBestMove game = do
	case bestMove game of
		Just move -> do
			let winner = whoWillWin game
			putStrLn $ "The best move is " ++ show move
			putStrLn $ "Winner " ++ show winner
		Nothing -> putStrLn "Game is complete"

getFilename :: [String] -> IO String
getFileName (x:xs) = return x
getFileName [] = do putStr "Enter the file path:"

main :: IO()
main = 
    do args <- getArgs
    	filepath <- if null args then getFileName else return head args
	filePath <- getLine
	game <- loadGame filePath
	putBestMove game

-- 
-- Story 15
--

-- 
-- Story 16
--





