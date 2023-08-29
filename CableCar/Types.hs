module CableCar.Types where

import Data.Maybe (fromJust)
import Data.List (findIndex)
import Control.Monad.State

-- Misc

nameAndVersionString :: String
nameAndVersionString = "CableCar v0.0.1"

-- Frontend

data Plan = Plan
    { p_name :: Name
    , p_cables :: [Cable]
    , p_modules :: [ModuleCreate]
    , p_plugs :: [Plug]
    }

type Name = String

data Cable = Cable
    { c_myref :: CableRef
    , c_name :: Name
    , c_type :: CableType
    }
  deriving Eq

data CableType = CableType
    { ct_power :: PowerHandshake
    , ct_data :: DataHandshake
    , ct_ins :: [Line]
    , ct_outs :: [Line]
    }
  deriving Eq

data PowerHandshake = PowerHandshake
    { ph_req :: Bool  -- Is there a req wire?
    }
  deriving Eq

data DataHandshake = DataHandshake
    { dh_req :: Bool  -- Is there a req wire?
    , dh_ack :: Bool  -- Is there an ack wire?
    }
  deriving Eq

type Line = (Name, Maybe HWType)

type HWType = String

data ModuleCreate = ModuleCreate
    { mc_myref :: ModuleRef
    , mc_module :: Name
    , mc_args :: [String]
    }

data Plug = Plug
    { pl_cable :: CableRef
    , pl_end :: PlugEnd
    , pl_thing :: ThingRef
    }

data CableRef = CableRef { uncableref :: Int }
  deriving Eq

data PlugEnd = FromEnd | ToEnd
  deriving (Eq, Show)

data ThingRef = Universe | TheModule ModuleRef
  deriving Eq

data ModuleRef = ModuleRef { unmoduleref :: Int }
  deriving Eq

type CableCar = State Plan

-- Backend

data Hardware = Hardware
    { hw_name :: Name
    , hw_wires :: [Wire]
    , hw_insts :: [Instantiation]
    }

type Wire = (IORole, Line)

data IORole = Input | Output | Internal
  deriving Eq

data Instantiation = Instantiation
    { inst_module :: Name
    , inst_args :: [String]
    }

-- Utility functions

all_cable_refs :: Plan -> [CableRef]
all_cable_refs p = map c_myref $ p_cables p

all_module_refs :: Plan -> [ModuleRef]
all_module_refs p = map mc_myref $ p_modules p

deref_cable :: Plan -> CableRef -> Cable
deref_cable p ref = (p_cables p) !! (uncableref ref)

deref_module :: Plan -> ModuleRef -> ModuleCreate
deref_module p ref = (p_modules p) !! (unmoduleref ref)
