# solver-connect-gore
Project 5
* Team Name: Connect Gore
* Team Members: Rudra Vashi, Aidan McLoughlin, Kevin Han, Owen Flynn


```Haskell
--implementation option 1
Game  :: (Color,[Columns])
          -- list of lists (list of columns)

Column :: [Color]

data Color = Red | Yellow
          -- 2 different data types (red or yellow)
          
Move :: Int
          -- int = column number
          -- add new move to an end of a list
          
Winner :: OneWinner Color | Tie | None
          -- 3 different types: winning player, tie, no winner
          
-- implementation option 2

Game :: [Column]

Move :: (Color, Int)
-- the color of the piece being played and the number of the column it's being played in

```
