module CableCar.FL (printFL, printFLs) where

import Data.List (partition)
import Control.Monad
import Control.Monad.State
import CableCar.Types

-- Print monad

data S = S { ls :: [String], level :: Int }

type Print = State S

runPrint :: Print () -> [String]
runPrint = ls . flip execState (S [] 0)

println :: String -> Print ()
println s = modify $ \st -> st { ls = ls st ++ [(concat $ replicate (level st) "    ") ++ s] }

indent :: Print ()
indent = modify $ \st -> st { level = level st + 1 }

outdent :: Print ()
outdent = modify $ \st -> st { level = level st - 1 }

-- Main things

printFL :: Hardware -> String
printFL = unlines . printFLs

printFLs :: Hardware -> [String]
printFLs = runPrint . pHW

pHW :: Hardware -> Print ()
pHW hw = do
    println $ "// Generated by " ++ nameAndVersionString
    println $ "let " ++ hw_name hw ++ " ="
    indent
    let (internals, inouts) = partition ((== Internal) . fst) $ hw_wires hw
    mapM_ pWireDecl inouts
    mapM_ pWireDecl internals
    println $ "CELL " ++ (show $ hw_name hw) ++ " ["
    indent
    let withComma = reverse $ zip (False : repeat True) $ reverse $ hw_insts hw
    mapM_ (uncurry pInst) withComma
    outdent
    println "];"
    outdent

pWireDecl :: Wire -> Print ()
pWireDecl (role, (name, hwt)) = println $ concat $ [typeStr, roleStr, " ", name, "."]
    where
        typeStr = case hwt of
                    Nothing -> ""
                    Just t -> t ++ "_"
        roleStr = case role of
                    Input -> "input"
                    Output -> "output"
                    Internal -> "internal"

pInst :: Bool -> Instantiation -> Print ()
pInst comma inst =
    let ls = exceptFirst ("    " ++) $ (inst_module inst) : (map parenIfNeeded $ inst_args inst)
        ls' = if comma then ls ++ [","] else ls
    in mapM_ println ls'

onlyFirst :: (a -> a) -> [a] -> [a]
onlyFirst f ls =
    case ls of
        [] -> []
        (x:xs) -> f x : xs

onlyLast :: (a -> a) -> [a] -> [a]
onlyLast f = reverse . onlyFirst f . reverse

exceptFirst :: (a -> a) -> [a] -> [a]
exceptFirst f ls =
    case ls of
        [] -> []
        (x:xs) -> x:(map f xs)

exceptLast :: (a -> a) -> [a] -> [a]
exceptLast f = reverse . exceptFirst f . reverse

paren :: String -> String
paren s = "(" ++ s ++ ")"

parenIfNeeded :: String -> String
parenIfNeeded s = if all (`elem` safe) s then s else paren s
    where
        safe = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_'"
