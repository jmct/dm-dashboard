module Dash.State where

import qualified Data.Map as M
import Data.Vector

import Brick.Widgets.List

type Name = String
type Description = String

-- | The overall gamestate. Just a preliminary version, I'm sure more will be
--   added. Right now we're using `Strin`s as names for the widgets, but
--   We should probably move to a custom enumeration
data GameState =
    GameState { characters :: List String (Name, Character)
              , npcs :: List String (Name, Description)
              }

-- | All information for an individual *player's* character
data Character =
  Character { hp :: Int
            , classType :: Class
            , race :: Race
            , gear :: [Equipment]
            , alignment :: Alignment
            }
  deriving (Show, Eq)

-- | The available classes for player characters
data Class = Rogue
           | Fighter
           | Warlock
           | Sorcerer
           | Bard
  deriving (Show, Eq)

-- | The available races for player characters
data Race = Human
          | Dwarf
          | Halfling
          | Dragonborn
  deriving (Show, Eq)

-- | All possible equipment along with related attributes (like number of
--   arrows)
data Equipment = Shortsword
               | Longsword
               | Axe
               | Longbow Int
               | Crossbow Int
               | Gold Int
  deriving (Show, Eq)

data Alignment = Lawful Morality
               | Chaotic Morality
               | Unlawful Morality
  deriving (Show, Eq)

data Morality = Good | Neutral | Evil
  deriving (Show, Eq)
