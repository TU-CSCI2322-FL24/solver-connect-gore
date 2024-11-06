# solver-connect-gore
Project 5
* Team Name: Connect Gore
* Team Members: Rudra Vashi, Aidan McLoughlin, Kevin Han, Owen Flynn


```Haskell

(Game board)Game  :: (Color,[Columns])
          - list of lists (list of columns)

Winnerboard :: Column

Column :: [Color]

(Player) Color :: Enum
          - 2 different data types (red or yellow)
          
Move :: Int
          - int = column number
          - add new move to an end of a list
          
Winner :: OneWinner Color | Tie | None
          - 3 different types: winning player, tie, no winner
          - checking diagonal wins will be difficult
```
