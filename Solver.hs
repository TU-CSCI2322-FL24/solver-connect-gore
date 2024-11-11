--implementation option 1
type Game  = (Color,Board)
type Board = [[Color]]
data Color = Red | Yellow deriving (Show)
type Move = Int 
data Winner = Won Color | Tie | Ongoing deriving (Show,Eq)

position :: Board -> Int ->[Move] -> [Move]
position [] index acc = acc
position (x:xs) index acc  
    |length x == 6 =  position xs (index+1) acc
    |otherwise     = position xs (index +1) (index:acc)
    


validMoves :: Game -> [Move]
validMoves (_,board) =reverse( position board 0 [])

