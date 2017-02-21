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
makeMaze width height = Maze (fill_cell_list width height) width height

fill_cell_list :: Int -> Int -> [(Bool, Bool)]
fill_cell_list width height = if (height == 1) then (fill_row width) else (fill_row width) ++
    (fill_cell_list width (height-1))

fill_row :: Int -> [(Bool, Bool)]
fill_row width = if (width == 1) then [(True, True)] else [(True, True)] ++ (fill_row (width-1) )


-- Every cell is represented by an integer equal to its position in the list

--NEED TO CHECK FOR BUGS WITH SHOWMAZE
kruskal :: Maze -> Maze
-- Function that executes the custom kruskal algorithm
kruskal maze = maze_from_path maze (make_path (init_sets (cells maze) 0) (shuffle (init_walls (width maze) (height maze) 0)))

init_sets :: [(Bool, Bool)] -> Int -> [Set Int]
-- Function that returns a list containing a set for each cell of the maze, with a representation of it
init_sets [] _ = []
init_sets (c : cells) curr = (Set.singleton curr : init_sets cells (curr + 1))

init_walls :: Int -> Int -> Int -> [(Int, Int)]
-- Function that returns a list with all the possible wall positions between two neighboring cells
init_walls width height curr
	| curr `div` width < height - 1 && curr `mod` width < width - 1 =
		((curr, curr + 1) : (curr, curr + width) : init_walls width height (curr + 1))
	| curr `div` width < height - 1 = ((curr, curr + width) : init_walls width height (curr + 1))
	| curr `mod` width < width - 1 = ((curr, curr + 1) : init_walls width height (curr + 1))
	| otherwise = []

join_sets :: [Set Int] -> Int -> Set Int -> [Set Int]
-- Function that replaces a set with a joined set where needed
join_sets [] _ _ = []
join_sets (x : xs) k joined_set = if Set.member k joined_set
	then (joined_set : join_sets xs (k + 1) joined_set)
	else (x : join_sets xs (k + 1) joined_set)

make_path :: [Set Int] -> [(Int, Int)] -> [(Int, Int)]
-- Function that executes the core kruskal algorithm part (outputs corridor positions)
make_path sets [] = []
make_path sets ((ci, cj) : walls) = if Set.notMember ci (sets !! cj) && Set.notMember cj (sets !! ci)
	then ((ci, cj) : make_path (join_sets sets 0 (Set.union (sets !! ci) (sets !! cj))) walls)
	else make_path sets walls

alter_maze_cell :: Maze -> Int -> Int -> Bool -> Maze
-- Function that alters given maze's cell value (0 is rw, 1 is dw)
alter_maze_cell maze pos rw_or_dw new_value = Maze (alter_cell_list (cells maze) pos rw_or_dw new_value) (width maze) (height maze)

alter_cell_list :: [(Bool, Bool)] -> Int -> Int -> Bool -> [(Bool, Bool)]
-- Function that outputs an altered maze cell list (alter_maze_cell helper)
alter_cell_list [] _ _ _ = []
alter_cell_list ((rw, dw) : cells) pos 0 new_value = if pos == 0
	then ((new_value, dw) : alter_cell_list cells (pos - 1) 0 new_value)
	else ((rw, dw) : alter_cell_list cells (pos - 1) 0 new_value)
alter_cell_list ((rw, dw) : cells) pos 1 new_value = if pos == 0
	then ((rw, new_value) : alter_cell_list cells (pos - 1) 0 new_value)
	else ((rw, dw) : alter_cell_list cells (pos - 1) 1 new_value)

maze_from_path :: Maze -> [(Int, Int)] -> Maze
-- Function that applies paths to maze
maze_from_path maze [] = maze
maze_from_path maze ((ci, cj) : corr) = if cj == ci + 1
	then maze_from_path (alter_maze_cell maze ci 0 False) corr
	else maze_from_path (alter_maze_cell maze ci 1 False) corr

--solvePerfect :: Maze -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
--solvePerfect maze (xs,ys) (xe,ye) =

--get_actions :: Maze -> Int -> [Int]
-- Function that outputs
--get_actions maze pos =

--perfect_dfs

showMaze :: Maze -> [(Int,Int)] -> String
showMaze (Maze cells width height) solution = (first_line width) ++ (fillboard height width cells solution)

--the recursive function
fillboard :: Int -> Int -> [(Int, Int)] -> String
fillboard y x cells solution =
  if (x == 1) then (fill_line y x cells solution)
  else (unlines (fill_line y y x cells solution) ) ++ (fillboard y (x-1) cells solution)

fill_line :: Int -> Int -> Int -> [(Bool,Bool)] -> [(Int, Int)] -> [String]
fill_line sy y x cells solution =
  | y == sy = [ "|" ++ (decide_star y x solution) ++ (decide_right) ++ (fill_line )] ++
  | y == 0 = "|"
  | otherwise =

decide_star :: Int -> Int -> [(Int, Int)] -> String
decide_star y x solution
    | solution == [] = "   " --3 spaces because roof is ---
    | fst (head solution) == x && snd (head solution) == y = " * "
    | otherwise = decide_star y x (tail solution)

--mipos thelei putStr allios to unlines
--test x = putStr (first_line x) -> auto doueuei me \n
first_line :: Int -> String
first_line x = if (x== 0) then "+\n" else "+---" ++ (first_line (x-1))
