--implementation option 1
type Game  = (Color,Board)
type Board = [[Color]]
data Color = Red | Yellow deriving (Show) 
type Move = Int
data Winner = Won Color | Tie | Ongoing
-- make a function that turns the board from a [[Color]] columns into [[Maybe Color]] rows
-- built in transpose funtion for haskell which can rotate the board
-- for printing it makes more sense to have the horizontal rows than vertical columns
-- built-in splitAt haskell function

-- instance Show Color where
-- 	show (Red) = (show x)++" f/s"
--  show (Yellow) = (show x)++" m/s"

-- board is 7 lists of columns where each column is a list of maximum length 6
-- board is 6 x 7 	(6 is column height and 7 is row length)
maybeinator :: Board -> [[Maybe Color]]
maybeinator [] = [] 
maybeinator (x:xs) = 
	-- aux [] num = []
	let aux _ 6 = []
		aux (y:ys) num = 
			| y == Yellow	= Just Yellow : aux ys (num++)
			| y == Red		= Just Red : aux ys (num++)
			| otherwise		= Nothing : aux ys (num++)
	in aux x 0 : maybeinator xs
-- bucket :: Eq b => (a -> b) -> [b] -> [a] -> [(b,[a])]
-- bucket bucketFn buckets items = 
--     [(b, filter (\x -> bucketFn x == b) items) | b <- buckets]

rowtate :: [[Maybe Color]] -> [[Maybe Color]]
rowtate = transpose 
-- rotate mayboard = transpose mayboard

class Show a => Rhow a where
	rhow :: a -> String
	rhow x = show x

showBoard :: [[Maybe Color]] -> String
showBoard [] = []
showBoard board = line ++ showRows board  ++ line 
	where line = Show $ " - - - - - - - "
			
showRows :: [[Maybe Color]] -> [[Maybe Color]]
showRows [] = []
showRows (x:xs) = "| " : showRow x
	let showRow [] = " |"
		showRow (x:xs) = 
			| x == Just Red  			= " R" : showRow xs
			| x == Just Yellow  		= " Y" : showRow xs
			| otherwise 				= " 0" : showRow xs
			
-- showRow :: [Maybe Color] -> [[Maybe Color]]
-- showRow [] = []







-- instance Show Color where
-- 	show (Red) = (show x)++" f/s"
--   	show (Yellow) = (show x)++" m/s"

-- class Show a => Phow a where
--   	phow :: a -> String
--   	phow x = show x


