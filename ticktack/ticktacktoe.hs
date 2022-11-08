type Result = [String]

-- Pretty print
pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))

-- Generate points
points :: (Int, Int) -> [(Int, Int)]
points size = let 
    w = fst size
    h = snd size
  in [(i, j) | i <- [0..w+1], j <- [0..h+1]]

-- Get X coordinate
get_x :: (Int, Int) -> Int
get_x element = fst element

-- Get Y coordinate
get_y :: (Int, Int) -> Int
get_y element = snd element

-- Get element position
get_pos :: (Int, (Int, Int)) -> Int
get_pos element = fst (element)

-- Get value (reversed)
get_val :: (Int, (Int, Int)) -> (Int, Int)
get_val element = (snd(snd(element)), fst(snd(element)))

compute_single :: (Int, (Int, Int)) -> (Char, (Int, Int))
compute_single coordinate 
    | odd (get_pos(coordinate)) = ('X', get_val(coordinate))
    | even (get_pos(coordinate)) = ('O', get_val(coordinate))

-- Split into chunks
group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero n"

-- Compute character
-- ( w x h ) -> [char, (coord_x, coord_y)] -> (x, y) -> result_char
compute :: (Int, Int) -> [(Char, (Int, Int))] -> (Int, Int) -> Char
compute size coordinates point = 
  if get_x(point) == 0                      then '-' else 
  if get_x(point) == get_x(size) +1         then '-' else 
  if get_y(point) == 0                      then '|' else  
  if get_y(point) == get_y(size) +1         then '|' else 
  if ('X', point) `elem` coordinates        then 'X' else
  if ('O', point) `elem` coordinates        then 'O'
  else ' '

-- Tick tack
ticktack :: (Int, Int) -> [(Int, Int)] -> Result
ticktack size coordinates = let
    points' = points(size)
    coordinates' = map (compute_single) (zip [1..] coordinates)
    result' = (map (compute size coordinates') points')
    
    -- split by line size (width + 2) and reverse symetrically to get proper indices positions
    in map (reverse) (group (fst size+2) (reverse result'))
     
  -- pp(ticktack (3,3) [(1,1), (3,3), (2,3), (1,3)] )
