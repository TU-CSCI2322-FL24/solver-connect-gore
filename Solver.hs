-- implementation option 1
import Data.List 

type Game  = (Color,Board)
type Board = [[Color]]
data Color = Red | Yellow deriving (Show, Eq) 
type Move = Int
data Winner = Won Color | Tie | Ongoing deriving (Show, Eq)
-- make a function that turns the board from a [[Color]] columns into [[Maybe Color]] rows
-- built in transpose funtion for haskell which can rotate the board
-- for printing it makes more sense to have the horizontal rows than vertical columns
-- built-in splitAt haskell function

-- board is 7 lists of columns where each column is a list of maximum length 6
-- board is 6 x 7 	(6 is column height and 7 is row length)
maybeinator :: Board -> [[Maybe Color]]
maybeinator [] = [] 
maybeinator (x:xs) = 
 let aux _ 6 = []
     aux [] num = Nothing : aux [] (num+1)
     aux (y:ys) num  
        | y == Yellow = Just Yellow : aux ys (num+1)
        | otherwise   = Just Red : aux ys (num+1) -- | otherwise   = Nothing : aux ys (num+1)
 in aux x 0 : maybeinator xs

showRows :: [[Maybe Color]] -> String
showRows [] = []
showRows (x:xs) = showRow x ++ "\n" ++ showRows xs
 where showRow [] = ""
       showRow (y:ys)  
            | y == Just Red     = " o" ++ showRow ys
            | y == Just Yellow  = " x" ++ showRow ys
            | otherwise         = " ." ++ showRow ys
 
boardPrt :: Game -> String
boardPrt (curr,board) = "Current players turn: " ++ show curr ++ "\n" ++ showRows rows
    where rows = reverse . transpose $ maybeinator $ board