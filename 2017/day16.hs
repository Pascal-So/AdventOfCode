import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.Foldable
import Data.Char
--import Debug.Trace
import Data.Monoid

data Instruction = Spin Int | Exchange Int Int | Partner Char Char
newtype Permutation = Permutation (Seq Int) deriving Show

instance Monoid Permutation where
    mempty = Permutation $ Seq.fromList [0..15]
    mappend (Permutation a) (Permutation b) =
        Permutation $ fmap (Seq.index a) b

fastPow :: (Monoid a) => Int -> a -> a
fastPow 0 _ = mempty
fastPow 1 a = a
fastPow n a =
    half <> half <> rest
    where
        half = fastPow (n `div` 2) a
        rest = fastPow (n `mod` 2) a

arrangementToPermutation :: Seq Char -> Permutation
arrangementToPermutation = 
    Permutation . fmap (\c -> ord c - ord 'a')

applyPermutation :: Permutation -> Seq a -> Seq a
applyPermutation (Permutation p) seq =
    fmap (Seq.index seq) p

spin :: Int -> Seq a -> Seq a
spin n xs =
    back >< front
    where
        (front, back) = Seq.splitAt (length xs - n) xs

exchange :: Int -> Int -> Seq a -> Seq a
exchange a b xs =
    Seq.update a vb $ Seq.update b va xs
    where
        va = Seq.index xs a
        vb = Seq.index xs b

partner :: (Eq a) => a -> a -> Seq a -> Seq a
partner a b xs =
    exchange pa pb xs
    where
        pa = 0 `fromMaybe` Seq.elemIndexL a xs
        pb = 0 `fromMaybe` Seq.elemIndexL b xs

runInstruction :: Instruction -> Seq Char -> Seq Char
runInstruction (Spin n) = spin n
runInstruction (Exchange a b) = exchange a b
runInstruction (Partner a b) = partner a b

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy c str =
    block : bs
    where
        (block, rest) = break (== c) str
        bs = if null rest 
                then [] 
                else splitBy c (tail rest)

readInstruction :: String -> Instruction
readInstruction ('s':xs) = Spin (read xs)
readInstruction ('x':xs) = 
    Exchange (read a) (read b)
        where
            [a,b] = splitBy '/' xs
readInstruction ('p':xs) = 
    Partner (head a) (head b)
        where
            [a,b] = splitBy '/' xs

readInput :: String -> [Instruction]
readInput =
    map readInstruction . splitBy ','

startArrangement :: Seq Char
startArrangement =
    Seq.fromList ['a' .. 'p']

showArrangement :: Seq Char -> String
showArrangement = toList

solveA :: Seq Char -> [Instruction] -> Seq Char
solveA init = foldl (flip runInstruction) init

-- this does not work yet. The permutations aren't always
-- the same, because of the partner operation.
solveB :: Seq Char -> [Instruction] -> Seq Char
solveB init inst =
    applyPermutation p init
    where
        single = arrangementToPermutation $ solveA init inst
        p = fastPow 2 single

main = do
    input <- readInput <$> getLine
    print $ solveA startArrangement input