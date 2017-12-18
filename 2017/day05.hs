import Control.Monad.State
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type Maze = Seq Int
type MazeState = (Maze,Int,Int)

jump :: Int -> State MazeState Int
jump ge3jmp = do
    (maze, pos, nr_jumps) <- get
    let jmp = Seq.index maze pos
    let field_adjust = if jmp >= 3 then ge3jmp else 1
    let newmaze = Seq.adjust (+field_adjust) pos maze
    let newpos = pos + jmp
    if newpos >= Seq.length maze || newpos < 0 then
        return (nr_jumps + 1)
    else do
        put (newmaze, pos + jmp, nr_jumps + 1)
        jump ge3jmp

readMaze :: String -> Maze
readMaze =
    Seq.fromList . map read . lines

solveA :: Maze -> Int
solveA maze =
    fst $ runState (jump 1) (maze, 0, 0)

solveB :: Maze -> Int
solveB maze =
    fst $ runState (jump (-1)) (maze, 0, 0)

main = do
    input <- readMaze <$> getContents
    print $ solveA input