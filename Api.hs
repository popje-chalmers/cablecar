module CableCar.Api where

import Control.Monad.State
import CableCar.Types

runCableCar :: CableCar () -> Plan
runCableCar = check . flip execState (Plan (error "Untitled main module!") [] [] [])
    where
        check p = case unpluggedCableRefs p of
                     [] -> p
                     refs -> error $ "The following cables are not plugged in on both ends: " ++ show (map (c_name . deref_cable p) refs)
        unpluggedCableRefs p = filter (not . isPluggedIn p) $ all_cable_refs p
        isPluggedIn p ref =
            let ends = map pl_end $ filter ((== ref) . pl_cable) $ p_plugs p
            in FromEnd `elem` ends && ToEnd `elem` ends

begin :: Name -> CableCar ()
begin name = put $ Plan name [] [] []

cabletype :: PowerHandshake -> DataHandshake -> [Line] -> [Line] -> CableType
cabletype = CableType

power :: PowerHandshake
power = PowerHandshake True

nopower :: PowerHandshake
nopower = PowerHandshake False

handshake :: DataHandshake
handshake = DataHandshake True True

nohandshake :: DataHandshake
nohandshake = DataHandshake False False

typed :: Name -> HWType -> Line
typed name hwtype = (name, Just hwtype)

untyped :: Name -> Line
untyped name = (name, Nothing)

cable :: Name -> CableType -> CableCar CableRef
cable name ctype = do
    p <- get
    let ref = CableRef $ length $ p_cables p
    when (any (\c -> c_name c == name) $ p_cables p ) $ error $ "There is already a cable with name " ++ show name
    put $ p { p_cables = p_cables p ++ [Cable ref name ctype] }
    return ref

make :: Name -> [String] -> CableCar ThingRef
make modname args = do
    p <- get
    let ref = ModuleRef $ length $ p_modules p
    put $ p { p_modules = p_modules p ++ [ModuleCreate ref modname args] }
    return $ TheModule $ ref

plug :: CableRef -> PlugEnd -> ThingRef -> CableCar ()
plug cable end thing = do
    p <- get
    when (any (\pl -> pl_cable pl == cable && pl_end pl == end) $ p_plugs p) $ error $ "Cable " ++ show (c_name $ p_cables p !! uncableref cable) ++ " is already connected at end " ++ show end
    put $ p { p_plugs = p_plugs p ++ [Plug cable end thing] }

from :: PlugEnd
from = FromEnd

into :: PlugEnd
into = ToEnd

world :: ThingRef
world = Universe
