{-# LANGUAGE TemplateHaskell #-}

module Year2017.Day23 where

import Data.Numbers.Primes
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.List as List
import qualified Data.Either as Either
import Text.Parsec hiding (State)
import Data.Char
import qualified Data.Maybe as Maybe
import Control.Lens
import Queue (Queue)
import qualified Queue as Queue

data Memory = Memory { _registers :: Seq Int
                     , _instruction_pointer :: Int
                     } deriving (Show)
makeLenses ''Memory

type Register = Int

data Value = Reg Register | Val Int deriving (Show)

data Instruction = Set Register Value
                 | Add Register Value
                 | Sub Register Value
                 | Mul Register Value
                 | Jnz Value Value
                 deriving (Show)

incrementPointer :: Memory -> Memory
incrementPointer = over instruction_pointer (+1)

mapRegister :: (Int -> Int) -> Int -> Memory -> Memory
mapRegister f i =
    over registers (Seq.adjust f i)

setRegister :: Int -> Int -> Memory -> Memory
setRegister v =
    mapRegister (const v)

runInstruction :: Instruction -> Memory -> Memory
runInstruction (Set register value) mem =
    newmem
    where
        newmem = incrementPointer . setRegister (valueOf mem value) register $ mem
runInstruction (Add register value) mem =
    newmem
    where
        newmem = incrementPointer . mapRegister (+ valueOf mem value) register $ mem
runInstruction (Sub register value) mem =
    newmem
    where
        newmem = incrementPointer . mapRegister (\n -> n - valueOf mem value) register $ mem
runInstruction (Mul register value) mem =
    newmem
    where
        newmem = incrementPointer . mapRegister (* valueOf mem value) register $ mem
runInstruction (Jnz value shft) mem =
    if valueOf mem value /= 0 then
        newmem
    else
        incrementPointer mem
    where
        shift = valueOf mem shft
        newmem = over instruction_pointer (+shift) $ mem

valueOf :: Memory -> Value -> Int
valueOf mem (Val a) = a
valueOf mem (Reg r) = Seq.index (_registers mem) r


parseInstruction :: Parsec String () Instruction
parseInstruction = do
    spaces
    inst <- many1 $ oneOf ['a'..'z']
    case inst of
        "set" -> do
            Set <$> parseRegister <*> parseValue
        "sub" -> do
            Sub <$> parseRegister <*> parseValue
        "add" -> do
            Add <$> parseRegister <*> parseValue
        "mul" -> do
            Mul <$> parseRegister <*> parseValue
        "jnz" -> do
            Jnz <$> parseValue <*> parseValue
        _ -> do
            unexpected $ "invalid instruction: " ++ inst

parseRegister :: Parsec String () Register
parseRegister = do
    spaces
    c <- oneOf ['a'..'z']
    return $ ord c - ord 'a'

parseInt :: Parsec String () Int
parseInt = do
    spaces
    neg <- optionMaybe $ char '-'
    number <- read <$> many1 (oneOf ['0'..'9'])
    case neg of
        Nothing ->
            return number
        _ ->
            return $ -number

parseValue :: Parsec String () Value
parseValue = do
    spaces
    (Reg <$> parseRegister) <|> (Val <$> parseInt)

stepSingleProgram :: Seq Instruction -> Memory -> Maybe Memory
stepSingleProgram insts mem =
    runInstruction <$> (getInst insts mem) <*> (Just mem)

getInst :: Seq Instruction -> Memory -> Maybe Instruction
getInst insts mem =
    insts Seq.!? pointer
    where
        pointer = _instruction_pointer mem

initialMemory :: Memory
initialMemory =
    Memory reg 0
    where
        reg = Seq.fromList $ 1 : replicate 7 0

isMul :: Maybe Instruction -> Bool
isMul (Just (Mul _ _)) = True
isMul _ = False

solveA :: Seq Instruction -> Int
solveA insts =
    length $ filter (\m -> isMul $ getInst insts m) $ runInstructions insts
    where
        runInstructions insts =
            List.unfoldr f initialMemory
            where
                f mem = (\x -> (x,x)) <$> stepSingleProgram insts mem

solveB :: Int
solveB =
    length $ filter (not . isPrime) [start, start + inc .. end]
    where
        start = 93 * 100 + 100000
        end = start + 17000
        inc = 17

readInput :: String -> Seq Instruction
readInput =
    Seq.fromList . Either.rights . map (parse parseInstruction "") . lines

main = do
    input <- readInput <$> getContents
    print $ solveB