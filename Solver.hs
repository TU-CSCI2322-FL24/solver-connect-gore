--implementation option 1
type Game  = (Color,Board)
type Board = [[Color]]
data Color = Red | Yellow deriving (Show, Eq)
type Move = Int
data Winner = Won Color | Tie | Ongoing deriving (Show, Eq)

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
dropPiece color column = reverse (color : reverse column)

-- Function to switch to the next player's color. I made it since we need to deicide who's next.
nextColor :: Color -> Color
nextColor Red = Yellow
nextColor Yellow = Red
