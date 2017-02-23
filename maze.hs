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


-- Every cell is represented by an integer equal to its position in the list (for the kruskal implementation)

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
-- Function that executes the core kruskal algorithm part (returns corridor positions)
make_path sets [] = []
make_path sets ((ci, cj) : walls) = if Set.notMember ci (sets !! cj) && Set.notMember cj (sets !! ci)
	then ((ci, cj) : make_path (join_sets sets 0 (Set.union (sets !! ci) (sets !! cj))) walls)
	else make_path sets walls

alter_maze_cell :: Maze -> Int -> Int -> Bool -> Maze
-- Function that alters given maze's cell value (0 is rw, 1 is dw)
alter_maze_cell maze pos rw_or_dw new_value = Maze (alter_cell_list (cells maze) pos rw_or_dw new_value) (width maze) (height maze)

alter_cell_list :: [(Bool, Bool)] -> Int -> Int -> Bool -> [(Bool, Bool)]
-- Function that returns an altered maze cell list (alter_maze_cell helper)
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

--NEED TO CHECK FOR BUGS WITH SHOWMAZE
solvePerfect :: Maze -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
-- Function that solves a perfect maze
solvePerfect maze (sx, sy) (gx, gy) = perfect_dfs maze (get_actions maze (sx, sy)) (-1, -1) (sx, sy) (gx, gy)

--NEED TO CHECK FOR BUGS WITH SHOWMAZE
get_actions :: Maze -> (Int, Int) -> [(Int, Int)]
-- Function that returns possible actions from a maze cell
get_actions maze pos = up_action maze pos

up_action :: Maze -> (Int, Int) -> [(Int, Int)]
-- Function that returns the position of the cell up if there is no wall separating them (and calls left_action)
up_action maze (x, y) = if x == 0 then (left_action maze (x, y)) else
	if snd ((cells maze) !! (width maze * (x - 1) + y)) == False
	then ((x - 1, y) : left_action maze (x, y))
	else (left_action maze (x, y))

left_action :: Maze -> (Int, Int) -> [(Int, Int)]
-- Function that returns the position of the cell left if there is no wall separating them (and calls right_action)
left_action maze (x, y) = if y `mod` (width maze) == 0 then (right_action maze (x, y)) else
	if fst ((cells maze) !! (width maze * x + y - 1)) == False
	then ((x, y - 1) : right_action maze (x, y))
	else (right_action maze (x, y))

right_action :: Maze -> (Int, Int) -> [(Int, Int)]
-- Function that returns the position of the cell right if there is no wall separating them (and calls down_action)
right_action maze (x, y) = if fst ((cells maze) !! (width maze * x + y)) == False
	then ((x, y + 1) : down_action maze (x, y))
	else (down_action maze (x, y))

down_action :: Maze -> (Int, Int) -> [(Int, Int)]
-- Function that returns the position of the cell down if there is no wall separating them
down_action maze (x, y) = if snd ((cells maze) !! (width maze * x + y)) == False
	then ((x + 1, y) : [])
	else []

--NEED TO CHECK FOR BUGS WITH SHOWMAZE
perfect_dfs :: Maze -> [(Int, Int)] -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
-- Function that performs the core dfs algorithm (the second argument list are the actions remaining for the current cell)
perfect_dfs _ [] _ _ _ = []
perfect_dfs maze (curr_action : rest_actions) prev_pos curr_pos goal_pos
	| curr_pos == goal_pos = (curr_pos : [])
	| curr_action == prev_pos = perfect_dfs maze rest_actions prev_pos curr_pos goal_pos
	| perfect_dfs maze (get_actions maze curr_action) curr_pos curr_action goal_pos == [] =
		perfect_dfs maze rest_actions prev_pos curr_pos goal_pos
	| otherwise = (curr_pos : perfect_dfs maze (get_actions maze curr_action) curr_pos curr_action goal_pos)

--NEED TO CHECK FOR BUGS WITH SHOWMAZE
solveBraid :: Maze -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
-- Function that solves a braid maze (instead of a previous node (Int, Int) there is an explored set (Set (Int, Int)))
solveBraid maze (sx, sy) (gx, gy) = braid_dfs maze (get_actions maze (sx, sy)) (Set.empty) (sx, sy) (gx, gy)

--NEED TO CHECK FOR BUGS WITH SHOWMAZE
braid_dfs :: Maze -> [(Int, Int)] -> Set (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
-- Function that performs the core dfs algorithm for a graph (the second argument list are the actions remaining for the current cell)
braid_dfs _ [] _ _ _ = []
braid_dfs maze (curr_action : rest_actions) explored_set curr_pos goal_pos
	| curr_pos == goal_pos = (curr_pos : [])
	| Set.member curr_action explored_set = braid_dfs maze rest_actions explored_set curr_pos goal_pos
	| braid_dfs maze (get_actions maze curr_action) (Set.insert curr_pos explored_set) curr_action goal_pos == [] =
		braid_dfs maze rest_actions explored_set curr_pos goal_pos
	| otherwise = (curr_pos : braid_dfs maze (get_actions maze curr_action) (Set.insert curr_pos explored_set) curr_action goal_pos)

showMaze :: Maze -> [(Int,Int)] -> String
showMaze (Maze cells width height) solution = (first_line width)
  ++ (fillboard width height 0 cells solution) ++ "\n"

--the recursive function
--should also be alright
fillboard :: Int -> Int -> Int -> [(Bool, Bool)] -> [(Int, Int)] -> String
fillboard width height y cells solution =
  if (y == height-1) then (fst (fill_line width height 0 y cells solution)) ++ "\n"
  ++ (snd (fill_line width height 0 y cells solution))
  else (fst (fill_line width height 0 y cells solution)) ++ "\n"
  ++ (snd (fill_line width height 0 y cells solution)) ++ "\n" ++
  (fillboard width height (y+1) cells solution)

fill_line :: Int -> Int -> Int -> Int -> [(Bool, Bool)] -> [(Int, Int)] -> (String, String)
fill_line width height x y cells solution
  | x == 0 =  ("|" ++ (decide_star x y solution) ++
    (decide_right width height 0 y 0 0 cells)
    ++ (fst (fill_line width height 1 y cells solution)),
    "+" ++ (decide_down width height 0 y 0 0 cells) ++ "+"
    ++ (snd (fill_line width height 1 y cells solution)))
  | x == width = ("","")
  | otherwise = ((decide_star x y solution) ++ (decide_right width height x y 0 0 cells)
    ++ (fst (fill_line width height (x+1) y cells solution)),
    (decide_down width height x y 0 0 cells) ++ "+"
    ++ (snd (fill_line width height (x+1) y cells solution)))

--is there a star?
decide_star :: Int -> Int -> [(Int, Int)] -> String
decide_star x y solution
  | solution == [] = "   " --3 spaces because roof is ---
  | fst (head solution) == x && snd (head solution) == y = " * "
  | otherwise = decide_star x y (tail solution)

--is there a right wall?
decide_right :: Int -> Int -> Int -> Int -> Int -> Int -> [(Bool, Bool)] -> String
decide_right width height x y curx cury cells =
  if (curx == x && cury == y) then fst (getwalls (head cells))
  else decide_right width height x y (fst (getnext width height curx cury))
  (snd (getnext width height curx cury)) (tail cells)

--is there a down wall?
decide_down :: Int -> Int -> Int -> Int -> Int -> Int -> [(Bool, Bool)] -> String
decide_down width height x y curx cury cells =
  if (curx == x && cury == y) then snd (getwalls (head cells))
  else decide_down width height x y (fst (getnext width height curx cury))
  (snd (getnext width height curx cury)) (tail cells)

--simple permutations of walls' existence
getwalls :: (Bool, Bool) -> (String, String)
getwalls walls
  | fst walls == True && snd walls == True = ("|", "---")
  | fst walls == True && snd walls == False = ("|", "   ")
  | fst walls == False && snd walls == False = (" ", "   ")
  | fst walls == False && snd walls == True = (" ", "---")

--doing some mod stuff
getnext :: Int -> Int -> Int -> Int -> (Int, Int)
getnext width height x y = if (x+1 >= width && y+1 <= height-1) then (0, y+1) else (x+1, y)

--mipos thelei putStr allios to unlines
--test :: Int -> Int -> String
test width height = putStr (showMaze (makeMaze width height) [])
test2 = decide_right 1 1 0 0 0 0 [(True,True)]
test3 = (fillboard 4 3 0 [
  (False, True), (False, False),
  (False, False), (True, False),
  (False, False), (True, False),
  (True,True), (True,False),
  (True, True), (True, False),
  (True, True), (False, True),
  (True, True), (True, True)] []) ++ "\n"
test4 = putStr (showMaze (Maze [(True, True), (True, True), (True, True), (False, True)] 2 2) [])
test_kr = putStr (showMaze (kruskal (makeMaze 4 3)) [])

first_line :: Int -> String
first_line x = if (x== 0) then "+\n" else "+---" ++ (first_line (x-1))
