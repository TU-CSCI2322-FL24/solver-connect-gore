--implementation option 1
type Game  = (Color,Board)
type Board = [[Color]]
data Color = Red | Yellow deriving (Show, Eq)
type Move = Int
data Winner = Won Color | Tie | Ongoing deriving (Show,Eq)

checkWinner :: Game -> Winner
checkWinner (_,brd) = 
  let winnerLst = checkVertical brd ++ (checkHorizontal brd) ++ (checkDiagonal brd)
  in if winnerLst == [] then Ongoing
     else if Red `elem` winnerLst
          then if Yellow `elem` winnerLst then Tie
               else Won Red
          else Won Yellow

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
  if filter (\x -> length x > 0) brd == [] then []
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

--checks bottom row of board for diagonals
--checkBottomDiag :: Board -> [Color]
--checkBottomDiag [] = []
--checkBottomDiag chunk = checkSquareOne chunk ++ (checkSquareTwo chunk) ++ (checkBottomDiag
--checks board for diagonals in a bottom-up, left to right manner, returning list of winning colors
checkDiagonal :: Board -> [Color]
checkDiagonal brd = 
  if filter (\x -> length x > 0) brd == [] then []
  else let aux [] = []
           aux chunk = checkSquareOne chunk ++ (checkSquareTwo chunk) ++ (aux (tail chunk))
       in (aux brd) ++ (checkDiagonal (map (\x -> if x == [] then [] else tail x) brd))
