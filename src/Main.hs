module Main where

import Dash.State
import qualified Data.Vector as Vec
import Data.Map

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.AttrMap as A
import Brick.Types (Widget)
import Brick.Util (on)
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  )

-- | Draw the main dashboard screen
drawUI :: GameState -> [Widget String]
drawUI gs = [ui]
  where
    chars     = characters gs
    charLabel = str "Characters (" <+> (str $ show $ length chars) <+> str ")"
    charBox   = B.borderWithLabel charLabel $
                hLimit 20 $
                vLimit 10 $
                L.renderList listDrawCharacter True chars
    ui = vBox [ charBox
              , str ""
              , str "Press Esc to exit the dashboard"
              ]

-- | Rendering a character as part of a list is just rendering their name
listDrawCharacter :: Bool -> (Name, Character) -> Widget String
listDrawCharacter _ (n,_) = str n

-- | How we handle UI events.
appEvent :: GameState -> V.Event -> T.EventM String (T.Next (GameState))
appEvent state (V.EvKey V.KEsc []) = M.halt state
appEvent state ev                  = L.handleListEvent ev (characters state) >>=
                                     M.continue . updateChars
  where
    updateChars l = state { characters = l }
                                     
myMap :: A.AttrMap
myMap = A.attrMap V.defAttr
          [ (L.listAttr,         V.white `on` V.blue)
          , (L.listSelectedAttr, V.blue `on` V.white)
          ]

dashboard =
  M.App { M.appDraw         = drawUI
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent  = appEvent
        , M.appStartEvent   = return
        , M.appAttrMap      = const myMap
        , M.appLiftVtyEvent = id
        }

initCharacters :: [(Name, Character)]
initCharacters = [ ("Eric", Character 10 Rogue Human [] (Chaotic Good))
                 , ("Alain", Character 11 Fighter Dwarf [Shortsword] (Lawful Good))
                 ]

initNPCs :: [(Name, Description)]
initNPCs = [ ("Emilia", "A small girl that was found in an abandonded house in Greenest")
           ]

initState =
  GameState (L.list "chars" (Vec.fromList initCharacters) (length initCharacters)) 
            (L.list "npcs"  (Vec.fromList initNPCs) (length initNPCs))
            

main = M.defaultMain dashboard initState
