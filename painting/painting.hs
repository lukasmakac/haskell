data Point = Point Int Int
data Shape = Ellipse Point Point Int 
           | Square {topLeft :: Point, size :: Int}
    
type Result = [String]

-- Pretty print
pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))

-- function return list of ellipse points for given formula
ellipse_fun :: Shape -> [Point]
ellipse_fun (Ellipse (Point x1 y1) (Point x2 y2) a)
    = [(Point 0 0)]

-- function return list of square poitns for given formula 
square_fun :: Shape -> [Point]
square_fun (Square { topLeft = (Point p1 p2), size = s })
    = [(Point 0 0)]

-- function merges 2..n lists (2D - arrays) into single one 
merge :: [[Point]] -> [Point]
merge screens 
    = [(Point 0 0)]

-- view 
view :: (Int, Int) -> [Shape] -> Result
view size shapes = 
    ["Hello"]


-- pp(view (40,15) [Ellipse (Point 8 4) (Point 16 4) 6, Square {topLeft = Point 15 5, size = 6 }, Ellipse (Point 25 7) (Point 35 12) 7] )