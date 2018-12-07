{-# LANGUAGE TemplateHaskell #-}

module Year2017.Day18 (solveA, solveB) where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.List as List
import qualified Data.Either as Either
import Text.Parsec hiding (State)
import Data.Char
import qualified Data.Maybe as Maybe
import Data.Foldable
import Control.Lens
import Queue (Queue)
import qualified Queue as Queue

data Memory = Memory { _registers :: Seq Int
                     , _received :: Queue Int
                     , _instruction_pointer :: Int
                     } deriving (Show)
makeLenses ''Memory

type Register = Int

data Value = Reg Register | Val Int deriving (Show)

data Subtask = TaskA | TaskB deriving (Eq)

data Instruction = Snd Value
                 | Set Register Value
                 | Add Register Value
                 | Mul Register Value
                 | Mod Register Value
                 | Rcv Register
                 | Jgz Value Value
                 deriving (Show)

incrementPointer :: Memory -> Memory
incrementPointer = over instruction_pointer (+1)

mapRegister :: (Int -> Int) -> Int -> Memory -> Memory
mapRegister f i =
    over registers (Seq.adjust f i)

setRegister :: Int -> Int -> Memory -> Memory
setRegister v =
    mapRegister (const v)

runInstruction :: Subtask -> Instruction -> Memory -> (Memory, Maybe Int)
runInstruction _ (Snd value) mem =
    (incrementPointer mem, Just $ valueOf mem value)
runInstruction _ (Set register value) mem =
    (newmem, Nothing)
    where
        newmem = incrementPointer . setRegister (valueOf mem value) register $ mem
runInstruction _ (Add register value) mem =
    (newmem, Nothing)
    where
        newmem = incrementPointer . mapRegister (+ valueOf mem value) register $ mem
runInstruction _ (Mul register value) mem =
    (newmem, Nothing)
    where
        newmem = incrementPointer . mapRegister (* valueOf mem value) register $ mem
runInstruction _ (Mod register value) mem =
    (newmem, Nothing)
    where
        newmem = incrementPointer . mapRegister (`mod` valueOf mem value) register $ mem
runInstruction TaskA (Rcv _) mem =
    (incrementPointer mem, Nothing)
runInstruction TaskB (Rcv register) mem =
    if Queue.null $ _received mem then
        (mem, Nothing)
    else
        runInstruction TaskB (Set register (Val val)) $ set received rec mem
        where
            (val, rec) = Maybe.fromJust $ Queue.dequeue $ _received mem
runInstruction _ (Jgz value shft) mem =
    if valueOf mem value > 0 then
        (newmem, Nothing)
    else
        (incrementPointer mem, Nothing)
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
        "snd" -> do
            Snd <$> parseValue
        "set" -> do
            Set <$> parseRegister <*> parseValue
        "add" -> do
            Add <$> parseRegister <*> parseValue
        "mul" -> do
            Mul <$> parseRegister <*> parseValue
        "mod" -> do
            Mod <$> parseRegister <*> parseValue
        "rcv" -> do
            Rcv <$> parseRegister
        "jgz" -> do
            Jgz <$> parseValue <*> parseValue
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

stepSingleProgram :: Subtask -> Seq Instruction -> Memory -> Maybe (Memory, Maybe Int)
stepSingleProgram task insts mem =
    if taskAOver then
        Nothing
    else
        runInstruction task <$> inst <*> Just mem
    where
        pointer = _instruction_pointer mem
        inst = insts Seq.!? pointer
        taskAOver =
            case inst of
                Just (Rcv v) -> task == TaskA && Seq.index (_registers mem) v /= 0
                _            -> False

initialMemory :: Memory
initialMemory =
    Memory reg Queue.empty 0
    where
        reg = Seq.fromList $ replicate 26 0

solveA :: String -> Int
solveA =
    last . Maybe.catMaybes . map snd . runInstructions . readInput
    where
        runInstructions insts =
            List.unfoldr f (initialMemory, Nothing)
            where
                f (mem, _) = (\x -> (x,x)) <$> stepSingleProgram TaskA insts mem

type State = (Maybe (Memory, Maybe Int), Maybe (Memory, Maybe Int))

stepProgramPair :: Seq Instruction -> State -> State
stepProgramPair insts (sA, sB) =
    if (programWaiting <$> mA, programWaiting <$> mB) == (Just True, Just True) then
        (Nothing, Nothing)
    else
        (stepSingleProgram TaskB insts =<< mA, stepSingleProgram TaskB insts =<< mB)
    where
        maybeQueue mval q = case mval of
            Nothing -> q
            Just v -> Queue.enqueue v q
        programWaiting mem =
            case inst of
                Rcv _ -> Queue.null $ _received mem
                _     -> False
            where
                inst = Seq.index insts $ _instruction_pointer mem
        mA = over received <$> (maybeQueue . snd <$> sB) <*> (fst <$> sA)
        mB = over received <$> (maybeQueue . snd <$> sA) <*> (fst <$> sB)

solveB :: String -> Int
solveB input =
    length $ Maybe.catMaybes $ map snd $ Maybe.catMaybes $ map snd $ executed
    where
        insts = readInput input
        initA = (initialMemory, Nothing)
        initB = (over registers (Seq.update (ord 'p' - ord 'a') 1) initialMemory, Nothing)
        executed = takeWhile (\(f,s) -> Maybe.isJust f || Maybe.isJust s) $ iterate (stepProgramPair insts) (Just initA, Just initB)

readInput :: String -> Seq Instruction
readInput =
    Seq.fromList . Either.rights . map (parse parseInstruction "") . lines
