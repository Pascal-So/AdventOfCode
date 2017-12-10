import Control.Monad.State
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type Maze = Seq Int
type MazeState = (Maze,Int,Int)

jump :: State MazeState Int
jump = do
    (maze, pos, nr_jumps) <- get
    let jmp = Seq.index maze pos
    let field_adjust = if jmp >= 3 then -1 else 1
    let newmaze = Seq.adjust (+field_adjust) pos maze
    let newpos = pos + jmp
    if newpos >= Seq.length maze || newpos < 0 then
        return (nr_jumps + 1)
    else do
        put (newmaze, pos + jmp, nr_jumps + 1)
        jump


readInput :: IO Maze
readInput =
    (Seq.fromList . map read . lines) `fmap` getContents

main = do
    maze <- readInput
    print $ fst $ runState jump  (maze, 0, 0)