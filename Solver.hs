--implementation option 1
type Game  = (Color,Board)
type Board = [[Color]]
data Color = Red | Yellow
type Move = Int
data Winner = Won Color | Tie | Ongoing
-- make a function that turns the board from a [[Color]] columns into [[Maybe Color]] rows
-- built in transpose funtion for haskell which can rotate the board
-- for printing it makes more sense to have the horizontal rows than vertical columns



