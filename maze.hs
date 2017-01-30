import System.Random
import System.IO.Unsafe

data Maze = Maze { cells :: [(Bool, Bool)]  -- [(rightWall, downWall)]
                 , width :: Int
                 , height :: Int
                 } deriving (Show)

rand :: Int -> Int
-- Returns a random integer from 0 to max-1
rand max = unsafePerformIO $ randomRIO (0, max-1)

shuffle :: [a] -> [a]
-- Randomly shuffles a list
shuffle = unsafePerformIO . shuffleM

shuffleM :: [a] -> IO [a]
-- DON'T BOTHER! Helper for shuffle
shuffleM [] = return []
shuffleM n = do {
                r <- fmap (flip mod $ length n) randomIO;
                n1 <- return $ n !! r;
                fmap ((:) n1) $ shuffleM $ (take r n) ++ (drop (r+1) n)
             }

--vazoume kai signatures
makeMaze :: Int -> Int -> Maze
makeMaze width height = Maze (fill_cell_list width height)  width height

fill_cell_list :: Int -> Int -> [(Bool, Bool)]
fill_cell_list width height = if (height == 1) then (fill_row width) else (fill_row width) ++
    (fill_cell_list width (height-1))

fill_row :: Int -> [(Bool, Bool)]
fill_row width = if (width == 1) then [(True, True)] else [(True,True)] ++ (fill_row (width-1) )


--prepei na diavasoume ton pseudokodika
--kruskal :: Maze -> Maze
--ksukal maze =


--solvePerfect :: Maze -> (Int,Int) -> (Int, Int) -> [(Int, Int)]
--solvePerfect maze (xs,ys) (xe,ye) =


--showMaze :: Maze -> [(Int,Int)] -> String
--showMaze (Maze cells width height) list = (first_line width) ++ (fillboard height width cells)

--fillboard :: Int -> Int -> [(Int, Int)] -> String
--fillboard y x cells = if (height == 1) then "|" ++ (fst fill_line) ++ "\n" ++ "+" ++ (snd fill_line)

--fill_line :: Int -> (String, String)

--mipos thelei putStr allios to unlines
--test x = putStr (first_line x) -> auto doueuei me \n
first_line :: Int -> String
first_line x = if (x== 0) then "+\n" else "+---" ++ (first_line (x-1))
