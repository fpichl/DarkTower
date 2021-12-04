module Gamestate where
import Texts

import System.IO
import Data.Array
import Data.List
import Data.Maybe
import qualified Data.Map as Map


-- classes ---------------------------------------------------------------------

data Location = Location { 
  id' :: Int, --used to identify a location
  name :: String, --name of location
  desc :: String, --descriptive text that pops up at the bottom
  art :: String, --the ascii art
  
  --all interactions with people and objects but also for switching locations
  interactions :: Map.Map String (GameState -> IO GameState )}

data GameState  = GameState {
  playerName :: String, --the players name
  location :: Location, --current location
  inventory :: Map.Map String Int, --Item and amount
  
  --used to save events e.g. keeping track of changes in a location
  events :: Map.Map String Int} 
                    
-- functions -------------------------------------------------------------------

-- displays text at the bottom of the screen and makes it "scrollable" by 
-- pressing Return
displayText :: String -> [String] -> Int -> IO ()
displayText art strings i
  | length (strings) <= i = do return ()
  | length (strings) > (i + 1) = do putStrLn art
                                    putStrLn (strings !! i)
                                    enterPress <- getLine
                                    displayText art strings (i+1)
  | otherwise = do putStrLn art
                   putStrLn (strings !! i)
                   displayText art strings (i+1)


-- returns all possible interactions
interactables :: Location -> String 
interactables location = 
  if result == lookaroundEndMessage 
  then lookaroundNothingMessage 
  else
    if length result <=80 --adds linebreaks if necessary
    then result ++ "\n\n" 
    else
      if length result <=160
      then result ++ "\n"
      else result
      
        --formats and converts the list of interactables and adds an end message
        where result = (intercalate ", " (nub $ interactables' location 0)) ++ 
                lookaroundEndMessage
                
-- returns a list of interactables
interactables' :: Location -> Int -> [String] 
interactables' location i
  | (length allKeys) <= i = []
  | (length allKeys) > i = 
    if (allKeys !! i ) !! 0 == 'B' || (allKeys !! i ) !! 0 == 'R' ||
      (allKeys !! i ) !! 0 == 'U'
    then [tail (allKeys !! i)] ++ (interactables' location (i+1))
    else interactables' location (i+1)
      where allKeys = Map.keys $ interactions location



-- returns all items in the players inventory
showInventory :: Map.Map String Int -> String 
showInventory inv = 
  if result == "" 
  then inventoryEmptyMessage
  else 
    if length result <=80 --adds linebreaks if necessary
    then result ++ "\n\n" 
    else
      if length result <=160
      then result ++ "\n"
      else result
      
        -- formats and converts the list of interactables 
        where result = (intercalate ", " (nub $ showInventory' inv 0)) 

-- returns a list of all Items and their amount
showInventory' :: Map.Map String Int -> Int -> [String] 
showInventory' inv i
  | (length allPairs) <= i = []
  | (length allPairs) > i = 
    if (snd $ allPairs !! i) > 0
    then [(fst $ allPairs !! i) ++ " x" ++ (show $ snd $ allPairs !! i)] ++ 
      showInventory' inv (i+1)
    else showInventory' inv (i+1)
        where allPairs = Map.assocs inv


-- merges a gamestate and a new location
changeLoc :: GameState -> Location -> GameState
changeLoc gmst loc =
  GameState (playerName gmst) loc (inventory gmst) (events gmst)
  
-- adds an item to a gamestate. Works with negative numbers
addItem :: GameState -> String -> Int -> GameState
addItem gmst item amount =
  if (isNothing $ Map.lookup item $ inventory gmst)
  then GameState (playerName gmst) (location gmst) (
        Map.insert item (max amount 0) $ inventory gmst) (events gmst)
  else GameState (playerName gmst) (location gmst) (
        Map.insert item newAmount $ inventory gmst)(events gmst)
          where newAmount =
                  max ((fromJust $ Map.lookup item $ inventory gmst) + amount) 0
     
-- removes an item from the inventory
rmItem :: GameState -> String -> GameState
rmItem gmst item = GameState (playerName gmst) (location gmst) (
        Map.insert item 0 $ inventory gmst) (events gmst)

-- returns the amount of an item
getItem:: GameState -> String -> Int
getItem gmst item
  | isNothing $ Map.lookup item $ inventory gmst = 0
  | otherwise = fromJust $ Map.lookup item $ inventory gmst
  
-- adds or overwrites an event  
setEvent :: GameState -> String -> Int -> GameState
setEvent gmst evnt i = 
  GameState (playerName gmst) (location gmst) (inventory gmst) (
    Map.insert evnt i $ events gmst)

-- returns the value of an event. Returns 0 if the event is unknown
getEvent :: GameState -> String -> Int 
getEvent gmst evnt
  | isNothing $ Map.lookup evnt $ events gmst = 0
  | otherwise = fromJust $ Map.lookup evnt $ events gmst

-- basically addItem and setEvent merged
setItemEvent :: GameState -> String -> Int -> String -> Int -> GameState
setItemEvent gmst item amount event i = 
  setEvent (addItem gmst item amount) event i
  
-- basically changeLoc and setEvent merged
setLocEvent :: GameState -> Location -> String -> Int -> GameState
setLocEvent gmst loc event i =
  setEvent (changeLoc gmst loc) event i
