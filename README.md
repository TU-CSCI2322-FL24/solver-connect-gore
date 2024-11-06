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
          
Move :: (Color, Int)
          - add new move to an end of a list
          
Winner :: Color 
          - checking diagonal wins will be difficult
```
