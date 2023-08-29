module CableCar.Hardware (makeHardware) where

import Control.Monad
import Control.Monad.State
import CableCar.Types

type HW = State Hardware

type DirectedLine = (Direction, Line)
data Direction = Fwd | Bwd

makeHardware :: Plan -> Hardware
makeHardware p = flip execState (Hardware (p_name p) [] []) $ do
    addWire (Input, (clk, Just "bit"))
    addWire (Input, (reset, Just "bit"))
    makeUniversePlugs p
    mapM_ (makeWiresForCable p) $ filter (cableIsInternal p) $ p_cables p
    mapM_ (makeModule p) $ p_modules p
    return ()

makeUniversePlugs :: Plan -> HW ()
makeUniversePlugs p =
    let plugs = plugsForThing p Universe
    in mapM_ (makeWiresForCable p . (deref_cable p) . pl_cable) plugs

makeWiresForCable :: Plan -> Cable -> HW ()
makeWiresForCable p cable =
    let fwdRole = cableRole p cable
        bwdRole = case fwdRole of
                       Input -> Output
                       Output -> Input
                       Internal -> Internal
        dls = linesForCable cable
    in  forM_ dls $ \(direction, line) ->
            let role = case direction of
                        Fwd -> fwdRole
                        Bwd -> bwdRole
            in addWire (role, line)


linesForCable :: Cable -> [DirectedLine]
linesForCable cable =
    let ct = c_type cable
        ls = getPowerHandshakeLines (ct_power ct) ++ getDataHandshakeLines (ct_data ct) ++ getDataLines ct
        ls' = map (\(dir, line) -> (dir, rename (forCable cable) line)) ls
    in ls'

getPowerHandshakeLines :: PowerHandshake -> [DirectedLine]
getPowerHandshakeLines h =
    if ph_req h then [(Fwd, (pwrreq, Just "bit"))] else []

getDataHandshakeLines :: DataHandshake -> [DirectedLine]
getDataHandshakeLines h =
    (if dh_req h then [(Fwd, (req, Just "bit"))] else []) ++ (if dh_ack h then [(Bwd, (ack, Just "bit"))] else [])

getDataLines :: CableType -> [DirectedLine]
getDataLines ct = (map (\line -> (Fwd, rename din line)) $ ct_ins ct) ++ (map (\line -> (Bwd, rename dout line)) $ ct_outs ct)

makeModule :: Plan -> ModuleCreate -> HW ()
makeModule p mc =
    let plugs = plugsForThing p $ TheModule $ mc_myref mc
        args = mc_args mc ++ [clk, reset] ++ (do
                plug <- plugs
                map (fst . snd) $ linesForCable $ deref_cable p $ pl_cable plug)
    in addInstantiation $ Instantiation (mc_module mc) args

-- The IORole for a cable as a whole. Internal if the cable only plugs with modules, Input/Output if it plugs from/into Universe.
-- Some wires within it (e.g. the ack wire) might have the opposite role.
cableRole :: Plan -> Cable -> IORole
cableRole p cable =
    let (p1, p2) = plugsForCable p cable
        (t1, t2) = (pl_thing p1, pl_thing p2)
    in case (t1, t2) of
        (Universe, Universe) -> error $ "Both ends of cable external: " ++ show (c_name cable)
        (Universe, TheModule _) -> Input
        (TheModule _, Universe) -> Output
        (TheModule _, TheModule _) -> Internal

cableIsInternal :: Plan -> Cable -> Bool
cableIsInternal p cable = cableRole p cable == Internal

-- All plugs, in order, for a particular ThingRef.
plugsForThing :: Plan -> ThingRef -> [Plug]
plugsForThing p t = filter ((== t) . pl_thing) $ p_plugs p

-- Returned tuple is in FromEnd, ToEnd order.
plugsForCable :: Plan -> Cable -> (Plug, Plug)
plugsForCable p cable =
    let ref = c_myref cable
        plugs = filter ((== ref) . pl_cable) $ p_plugs p
    in case plugs of
        [p1, p2] -> case (p1, p2) of
            (Plug _ FromEnd _, Plug _ ToEnd _) -> (p1, p2)
            (Plug _ ToEnd _, Plug _ FromEnd _) -> (p2, p1)
            _ -> failed
        _ -> failed
    where
        failed = error $ "Bad plugs for cable " ++ show (c_name cable)

addWire :: Wire -> HW ()
addWire wire@(role, (name, hwt)) = do
    hw <- get
    when (not $ null $ filter ((== name) . fst . snd) $ hw_wires hw) $ error $ "Duplicate wire " ++ show name
    put $ hw { hw_wires = hw_wires hw ++ [wire] }

addInstantiation :: Instantiation -> HW ()
addInstantiation inst = modify $ \hw -> hw { hw_insts = hw_insts hw ++ [inst] }

rename :: (Name -> Name) -> Line -> Line
rename f (name, hwt) = (f name, hwt)

-- Wire names

clk :: Name
clk = "clk"

reset :: Name
reset = "reset"

pwrreq :: Name
pwrreq = "pwrreq"

pwrack :: Name
pwrack = "pwrack"

req :: Name
req = "req"

ack :: Name
ack = "ack"

din :: Name -> Name
din = ("in'" ++)

dout :: Name -> Name
dout = ("out'" ++)

forCable :: Cable -> Name -> Name
forCable cable name = (c_name cable) ++ "'" ++ name
