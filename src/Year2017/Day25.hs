module Year2017.Day25 (solveA) where

import Tape (Tape)
import qualified Tape

data State = A | B | C | D | E | F deriving (Eq, Show)

type Machine = (State, Tape Bool)

step :: Machine -> Machine
step (state, tape) =
    case (state, Tape.head tape) of
        (A, False) -> (B, right $ write True )
        (A, True ) -> (C, left  $ write False)

        (B, False) -> (A, left  $ write True )
        (B, True ) -> (D, left  $ write True )

        (C, False) -> (D, right $ write True )
        (C, True ) -> (C, right $ write False)

        (D, False) -> (B, left  $ write False)
        (D, True ) -> (E, right $ write False)

        (E, False) -> (C, right $ write True )
        (E, True ) -> (F, left  $ write True )

        (F, False) -> (E, left  $ write True )
        (F, True ) -> (A, right $ write True )
    where
        write a = Tape.update a tape
        right   = Tape.moveRight False
        left    = Tape.moveLeft  False

checksum :: Machine -> Int
checksum (_,tape) =
    length $ filter (id) $ Tape.toList tape

initial :: Machine
initial = (A, tape) where
    tape = Tape.fromList [False]

solveA :: Int -> Int
solveA steps =
    checksum $ foldr (.) id (replicate steps step) initial

main = do
    print $ solveA 12656374