--implementation option 1
type Game  = (Color,Board)
type Board = [[Color]]
data Color = Red | Yellow
type Move = Int
data Winner = Won Color | Tie | Ongoing
