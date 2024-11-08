--implementation option 1
 Game  :: (Color,[Columns])
 Column :: [Color]
 data Color = Red | Yellow
 Move :: Int
 Winner :: OneWinner Color | Tie | None
