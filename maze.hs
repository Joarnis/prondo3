import System.Random
import System.IO.Unsafe
import Data.Set (Set)
import qualified Data.Set as Set

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

makeMaze :: Int -> Int -> Maze
makeMaze width height = Maze (fill_cell_list width height)  width height

fill_cell_list :: Int -> Int -> [(Bool, Bool)]
fill_cell_list width height = if (height == 1) then (fill_row width) else (fill_row width) ++
    (fill_cell_list width (height-1))

fill_row :: Int -> [(Bool, Bool)]
fill_row width = if (width == 1) then [(True, True)] else [(True, True)] ++ (fill_row (width-1) )


-- Every cell is represented by an integer equal to its position in the list 
--POLLA THELOUN 0 ORISMA THIMISOY

--kruskal :: Maze -> Maze
--ksukal maze =

-- Function that returns a list containing a set for each cell of the maze, with a representation of it 
init_sets :: [(Bool, Bool)] -> Int -> [Set Int]
init_sets [] _ = []
init_sets (c:cells) curr = (Set.singleton curr : init_sets cells (curr + 1))  

-- Function that returns a list with all the possible wall positions between two neighboring cells
init_walls :: Int -> Int -> Int -> [(Int, Int)]
init_walls width height curr 
	| curr `div` width < height - 1 && curr `mod` width < width - 1 = ((curr, curr + 1) : (curr, curr + width) :
		init_walls width height (curr + 1))
	| curr `div` width < height - 1 = ((curr, curr + width) : init_walls width height (curr + 1))
	| curr `mod` width < width - 1 = ((curr, curr + 1) : init_walls width height (curr + 1))
	| otherwise = []

-- Function that replaces a set with a joined set where needed
join_sets :: [Set Int] -> Int -> Set Int -> [Set Int]
join_sets [] _ _ = []
join_sets (x : xs) k joined_set = if Set.member k joined_set then (joined_set : join_sets xs (k + 1) joined_set)
	else (x : join_sets xs (k + 1) joined_set)

-- Function that executes the core kruskal algorithm part (outputs corridor positions)
make_path :: [Set Int] -> [(Int, Int)] -> [(Int, Int)]
make_path sets [] = []
make_path sets ((ci, cj) : walls) = if Set.notMember ci (sets !! cj) && Set.notMember cj (sets !! ci)
	then ((ci, cj) : make_path (join_sets sets 0 (Set.union (sets !! ci) (sets !! cj))) walls)
	else make_path sets walls

-- Function that applies paths to maze
maze_from_path :: Maze -> [(Int, Int)] -> Maze
maze_from_path maze ((ci, cj) : corr) = if cj == ci + 1 then 

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
