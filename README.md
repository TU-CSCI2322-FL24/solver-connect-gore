# solver-connect-gore
Team Name: Connect Gore
Team Members: Rudra Vashi, Aidan McLoughlin, Kevin Han, Owen Flynn


\\\ Haskell

Game board: Game  :: [[Color]]
          - list of lists (list of columns)
          - list of rows
Player: Color :: Enum
      - 2 different data types (red or yellow)
Move:  Move :: (Color, Int) / (Color, Column) 
    - add new move to an end of a list
Winner:  Winner :: Color 
      - checking diagonal wins will be difficult
\\\
