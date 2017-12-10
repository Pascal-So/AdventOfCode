import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

type Memory = Map String Int

data Comparison = Greater | GreaterEqual | Less | LessEqual | Equal | NotEqual
data Condition = Condition String Comparison Int
data Command = Command String Int Condition


instance Show Comparison where
    show Greater = ">"
    show GreaterEqual = ">="
    show Less = "<"
    show LessEqual = "<="
    show Equal = "=="
    show NotEqual = "!="

instance Show Condition where
    show (Condition name comp val) = name ++ " " ++ show comp ++ " " ++ show val

instance Show Command where
    show (Command name inc comp) = name ++ " += " ++ show inc ++ " if " ++ show comp

runComp :: Comparison -> Int -> Int -> Bool
runComp Greater = (>)
runComp GreaterEqual = (>=)
runComp Less = (<)
runComp LessEqual = (<=)
runComp Equal = (==)
runComp NotEqual = (/=)




checkCond :: Condition -> Memory -> Bool
checkCond (Condition name comp val) mem =
    fromMaybe False $ runComp comp <$> Map.lookup name mem <*> (Just val)


readComp :: String -> Maybe Comparison
readComp ">" = Just Greater
readComp ">=" = Just GreaterEqual
readComp "<" = Just Less
readComp "<=" = Just LessEqual
readComp "==" = Just Equal
readComp "!=" = Just NotEqual
readComp _ = Nothing

readCond :: String -> Maybe Condition
readCond str =
    case words str of
        [name, comp, val] -> Condition name <$> readComp comp <*> Just (read val)
        _ -> Nothing

readCommand :: String -> Maybe Command
readCommand str =
    case words str of
        (name:op:val:xs) -> Command name diff <$> readCond (unwords $ tail xs)
                                where diff = if op == "inc" then read val else -(read val)
        _ -> Nothing

getVars :: Command -> Set String
getVars (Command a _ (Condition b _ _)) =
    Set.fromList [a,b]

initMemory :: [Command] -> Memory
initMemory = 
    Map.fromList . map (\n -> (n,0)) . Set.toList . Set.unions . map getVars

runCommand :: Memory -> Command -> Memory
runCommand mem (Command name diff cond) =
    if checkCond cond mem then
        Map.adjust (+diff) name mem
    else
        mem

biggestRegisterVal :: Memory -> Int
biggestRegisterVal = maximum . Map.elems

solveA :: [Command] -> Int
solveA cmds = 
    biggestRegisterVal $ foldl runCommand (initMemory cmds) cmds

solveB :: [Command] -> Int
solveB cmds =
    snd $ foldl f init cmds
    where
        init = (initMemory cmds, 0)
        f (mem, maxval) cmd =
            (newmem, newmax)
            where
                newmem = runCommand mem cmd
                newmax = max maxval $ biggestRegisterVal newmem


main = do
    input <- getContents
    let cmds = catMaybes $ map readCommand $ lines input
    print $ solveB cmds