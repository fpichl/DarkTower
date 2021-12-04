import Texts
import Gamestate
import SSP

import Control.Exception
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as Txt
import qualified Data.Map as Map
import System.IO
import System.Directory
import Text.Read

-- This is needed to make some lines fit in the 80 char limit lol
notNothing :: Maybe a -> Bool
notNothing x = not $ isNothing x

-- This is needed to make some lines fit in the 80 char limit lol x2
noJust :: Maybe a -> a
noJust = fromJust 


-- main game loop
-- gmst = abbreviation for gamestate
-- sTxt = showText: That's whats printed on screen. Used for dialogue, 
---- interactions etc etc
gameLoop :: GameState -> [String] -> IO GameState
gameLoop gmst sTxt =        
        
  -- has the game just started?
  if (getEvent gmst "startup") == 1
  then do
    putStrLn (welcomeArt)
    putStrLn (nameQuestion)
    name <- getLine
    putStrLn (welcomeArt)
    putStrLn (welcomeTip)
    enterPress <- getLine
    
    let newG = GameState name (location gmst) (inventory gmst) (events gmst)
    
    -- another gameLoop starts, but "startup" is set to 0 and wthe name is set
    gameLoop (setEvent newG "startup" 0) [""]
        
  else do
    putStrLn ("--------------------------\n") 
    putStrLn (art $ location gmst)
        
    if null (sTxt !! 0)
    then do putStrLn (desc $ location gmst)
    else do
      displayText (art $ location gmst) sTxt 0
            
    input' <- getLine
    let input = (map toLower input')
        
    if (input `elem` quitCharacter) 
    then do return gmst -- quitCharacters quit the game
    else do -- pattern matching for valid input  
      case input of 
      
        -- command that moves you to a different location
        ('n':'a':'c':'h':' ':a) -> 
          if (notNothing $ Map.lookup ("N"++ a) (interactions $ location gmst))
          then (noJust $ Map.lookup ("N"++a)(interactions $ location gmst)) gmst
          else gameLoop gmst [""]
                                                    
        -- this command prints all interactable objects and people
        ('u':'m':'s':'e':'h':'e':'n':a) -> 
          gameLoop gmst [lookaroundMessage ++ (interactables $ location gmst)]
                          
        -- this command prints the current location
        ('w':'o':' ':'b':'i':'n':' ':'i':'c':'h':a) -> 
          gameLoop gmst [whereAmIMessage ++(name $ location gmst)++ "\n\n\n"]
                          
        -- this command initiates an interaction with objects an people
        ('r':'e':'d':'e':' ':'m':'i':'t':' ':a) -> 
          if (notNothing $ Map.lookup ("R"++ a) (interactions $ location gmst)) 
          then (noJust $ Map.lookup ("R"++a)(interactions $ location gmst)) gmst
          else 
            if (notNothing $ Map.lookup ("R"++ a) generalActions) 
            then (noJust $ Map.lookup ("R"++ a) generalActions) gmst 
            else gameLoop gmst [""]
                                                                          
        -- this command initiates an interaction with items, objects an people
        -- ich glaube ich hab das hier doof geloast?
        -- ich habe aber keine Zeit mehr um mich da reinzudenken, das zu aendern
        -- und alle events auf Fehler zu ueberpruefen.
        ('b':'e':'n':'u':'t':'z':'e':' ':a) -> 
          if (notNothing $ Map.lookup ("B"++ a) (interactions $ location gmst)) 
          then (noJust $ Map.lookup ("B"++a)(interactions $ location gmst)) gmst
          else 
            if (notNothing $ Map.lookup ("I"++a)(interactions $ location gmst))
            then (noJust$ Map.lookup("I"++a)(interactions $ location gmst))gmst
            else 
              if (getItem gmst a) >= 1 && 
                   (notNothing $ Map.lookup ("B"++ a) generalActions)
              then (noJust $ Map.lookup ("B"++ a) generalActions) gmst 
              else gameLoop gmst [""]
                                                                      
        -- this command prints all items in inventory
        ('i':'n':'v':'e':'n':'t':'a':'r':a) -> do
          gameLoop gmst [inventoryOpenMessage ++(showInventory$ inventory gmst)]
                          
        -- this command initiates a type of interaction with items,
        -- objects an people
        ('u':'n':'t':'e':'r':'s':'u':'c':'h':'e':' ':a) -> 
          if (getItem gmst a) >= 1
          then (noJust $ Map.lookup ("U"++ a) generalActions) gmst
          else 
            if (notNothing $ Map.lookup ("U"++a) (interactions $ location gmst))
            then (noJust$ Map.lookup ("U"++a)(interactions $ location gmst))gmst
            else gameLoop gmst [""]
                                     
        -- this command is for saving the current gamestate
        ('s':'a':'v':'e':a) -> do
          let i = readMaybe a :: Maybe Int
          if (notNothing i)
          then do catch (save gmst (noJust i) savedSuccess) (saveError gmst)
          else do gameLoop gmst savedFailure
          
        -- this command is for loading from a saved file
        ('l':'o':'a':'d':a) -> do
          let i = readMaybe a :: Maybe Int                         
          if (notNothing i)
          then do catch (load $ noJust i) (loadError gmst)
          else do gameLoop gmst loadedFailure
          
        -- this command is for teleporting. Used for debugging and stuff.
        --('t':'p':' ':a) -> 
          --if notNothing (Map.lookup a tpLocations)
          --then gameLoop (GameState (playerName gmst) (noJust $ Map.lookup a 
            --tpLocations) (inventory gmst) (events gmst)) ["WooOOOoosh!\n\n\n"]
          --else gameLoop gmst [""]
                          
        -- this command initiates interactions that don't fall in any categories
        -- above. This includes options in dialogue
        a -> 
          if (notNothing $ Map.lookup a (interactions $ location gmst))
          then (noJust $ Map.lookup a (interactions $ location gmst)) gmst 
          else do gameLoop gmst [""]

-- functions -------------------------------------------------------------------

-- to start the game
main :: IO ()
main = do
  gameLoop start [""]
  return ()

-- saves the current gamestate to a numbered save file with custom text for the
-- next scene
save :: GameState -> Int -> [String] -> IO GameState
save gmst i text = do
  createDirectoryIfMissing False "./saves"
  writeFile ("./saves/save" ++ (show i) ++ ".blackTower") content
  gameLoop gmst text
    where content = (playerName gmst) ++ "\n" ++
                    (show $ id' $ location gmst) ++ "\n" ++
                    (show $ Map.assocs $ inventory gmst) ++ "\n" ++
                    (show $ Map.assocs $ events gmst)
                              
                  
-- loads a numbered save file
load :: Int -> IO GameState
load i = do
  handle <- openFile ("./saves/save" ++ (show i) ++ ".blackTower") ReadMode
  content' <- hGetContents handle
  let content = map Txt.unpack (Txt.splitOn (Txt.pack "\n") (Txt.pack content'))
      name = (content !! 0)
      
      locid = read (content !! 1) :: Int
      loc = noJust $ Map.lookup locid svLocations
      
      invlist = read (content !! 2) :: [(String, Int)]
      inv = Map.fromList invlist
      
      evntlist = read (content !! 3) :: [(String, Int)]
      evnt = Map.fromList evntlist
      gmst = (GameState name loc inv evnt)
  gameLoop (gmst) loadedSuccess
  
-- lets the player know that something went wrong while saving. Calls gameLoop
saveError :: GameState -> IOError -> IO GameState
saveError gmst e = gameLoop gmst savedError

-- lets the player know that something went wrong while loading. Calls gameLoop
loadError :: GameState -> IOError -> IO GameState
loadError gmst e = gameLoop gmst loadedError

--locations --------------------------------------------------------------------

-- this is for the teleportation command to look up locations
tpLocations :: Map.Map String Location
tpLocations = Map.fromList [
  ("strand", beach), --the beach with santa
  ("turm", blackTower),
  ("ostweg", easternPath),
  ("berg", mountain),
  ("hoehle", mountainCave),
  ("platz", vilTownSquare), --"village town square"
  ("westweg", westernPath),
  ("westweg2", westernPath2), --western path after the sword is picked up
  ("dorf", vilEntrWest), --"village entrance west"
  ("insel", perlinselnPortEmpty),
  ("inselGF", perlinselnPort),
  ("inselNOGF", perlinselnPortGFDead),
  ("gruft", tombEntrance)] 

-- this is for the "load" method to look up locations.
svLocations :: Map.Map Int Location
svLocations = Map.fromList [
               (1, beach),
               (2, coast),
               (3, cstlInside),
               (4, cstlOutside),
               (5, blackTower),
               (6, easternPath),
               (7, mountain),
               (8, mountainCave),
               (9, mountainCaveR),
               (10, mountainCaveS),
               (11, mountainCaveT),
               (12, vilFountain),
               (13, vilPickle),
               (14, vilTownSquare),
               (15, westernPath),
               (16, westernPath2),
               (50, westernPath2),
               (17, vilEntrWest),
               (18, dog),
               (19, elder),
               (20, frida),
               (21, gottfried),
               (22, harald),
               (23, vilPickleGuard),
               (24, wpsword),
               (30, gottfriedGone),
               (35, perlinselnPort),
               (36, perlinselnPortGFDead),
               (37, perlinselnsouthernPathGF),
               (38, perlinselnsouthernPath),
               (39, perlinselnCamp),
               (40, perlinselnCamp2),
               (41, perlinselnCampGF),
               (42, siegfriede),
               (43, gfsf),
               (60, tombEntrance),
               (61, tombMiddle),
               (62, tombMiddleOrbs),
               (63, tombLeft),
               (64, tombRigth),
               (65, mountainSF),
               (66, mountainGF),
               (0, god),
               (100, endScreen),
               (99, haraldTips)]

god = Location 0 god_Name god_Desc god_Art god_Int

god2 = Location 99 god2_Name god2_Desc god2_Art god2_Int

endScreen = Location 100 endScreen_Name endScreen_Desc endScreen_Art
  endScreen_Int

beach = Location 1 beach_Name beach_Desc beach_Art beach_Int

coast = Location 2 coast_Name coast_Desc coast_Art coast_Int

cstlInside = Location 3 cstlInside_Name cstlInside_Desc cstlInside_Art
  cstlInside_Int

cstlOutside = Location 4 cstlOutside_Name cstlOutside_Desc cstlOutside_Art 
  cstlOutside_Int

blackTower = Location 5 blackTower_Name blackTower_Desc blackTower_Art
  blackTower_Int

easternPath = Location 6 easternPath_Name easternPath_Desc easternPath_Art 
  easternPath_Int

mountain = Location 7 mountain_Name mountain_Desc mountain_Art mountain_Int
mountainSF = Location 65 mountainSF_Name mountainSF_Desc mountainSF_Art 
  mountainSF_Int
mountainGF = Location 66 mountainGF_Name mountainGF_Desc mountainGF_Art 
  mountainGF_Int

mountainCave = Location 8 mountainCave_Name mountainCave_Desc mountainCave_Art 
  mountainCave_Int
mountainCaveR = Location 9 mountainCaveR_Name mountainCaveR_Desc 
  mountainCaveR_Art mountainCaveR_Int
mountainCaveS = Location 10 mountainCaveS_Name mountainCaveS_Desc 
  mountainCaveS_Art mountainCaveS_Int
mountainCaveT = Location 11 mountainCaveT_Name mountainCaveT_Desc 
  mountainCaveT_Art mountainCaveT_Int

perlinselnCamp = Location 39 perlinselnCamp_Name perlinselnCamp_Desc
  perlinselnCamp_Art perlinselnCamp_Int

perlinselnCamp2 = Location 40 perlinselnCamp2_Name perlinselnCamp2_Desc
  perlinselnCamp2_Art perlinselnCamp2_Int

perlinselnCampGF = Location 41 perlinselnCampGF_Name perlinselnCampGF_Desc
  perlinselnCampGF_Art perlinselnCampGF_Int

perlinselnPort = Location 35 perlinselnPort_Name perlinselnPort_Desc
  perlinselnPort_Art perlinselnPort_Int
  
perlinselnPortEmpty = Location 42 perlinselnPortEmpty_Name 
  perlinselnPortEmpty_Desc perlinselnPortEmpty_Art perlinselnPortEmpty_Int

perlinselnPortGFDead = Location 36 perlinselnPortGFDead_Name 
  perlinselnPortGFDead_Desc perlinselnPortGFDead_Art perlinselnPortGFDead_Int
  
perlinselnsouthernPathGF = Location 37 perlinselnsouthernPathGF_Name
  perlinselnsouthernPathGF_Desc perlinselnsouthernPathGF_Art
  perlinselnsouthernPathGF_Int

perlinselnsouthernPath = Location 38 perlinselnsouthernPath_Name
  perlinselnsouthernPath_Desc perlinselnsouthernPath_Art
  perlinselnsouthernPath_Int

tombEntrance = Location 60 tombEntrance_Name tombEntrance_Desc tombEntrance_Art
  tombEntrance_Int

tombMiddle = Location 61 tombMiddle_Name tombMiddle_Desc tombMiddle_Art
  tombMiddle_Int

tombMiddleOrbs = Location 62 tombMiddleOrbs_Name tombMiddleOrbs_Desc
  tombMiddleOrbs_Art tombMiddleOrbs_Int

tombLeft = Location 63 tombLeft_Name tombLeft_Desc tombLeft_Art tombLeft_Int

tombRigth =Location 64 tombRigth_Name tombRigth_Desc tombRigth_Art tombRigth_Int

vilFountain = Location 12 vilFountain_Name vilFountain_Desc vilFountain_Art
  vilFountain_Int 

vilPickle = Location 13 vilPickle_Name vilPickle_Desc vilPickle_Art 
  vilPickle_Int

vilTownSquare = Location 14 vilTownSquare_Name vilTownSquare_Desc 
  vilTownSquare_Art vilTownSquare_Int

westernPath = Location 15 westernPath_Name westernPath_Desc westernPath_Art
  westernPath_Int
westernPath2 = Location 16 westernPath2_Name westernPath2_Desc westernPath2_Art
  westernPath2_Int
westernPath3 = Location 50 westernPath3_Name westernPath3_Desc westernPath3_Art
  westernPath3_Int

vilEntrWest = Location 17 vilEntrWest_Name vilEntrWest_Desc vilEntrWest_Art
  vilEntrWest_Int


-- conversations (they're still locations) -------------------------------------

beachGuard = Location 40 beachGuard_Name beachGuard_Desc beachGuard_Art
  beachGuard_Int

dog = Location 18 dog_Name dog_Desc dog_Art dog_Int

elder = Location 19 elder_Name elder_Desc elder_Art elder_Int

frida = Location 20 frida_Name frida_Desc frida_Art frida_Int

gfsf = Location 43 gfsf_Name gfsf_Desc gfsf_Art gfsf_Int

gottfried = Location 21 gottfried_Name gottfried_Desc gottfried_Art 
  gottfried_Int
  
gottfriedGone = Location 30 gottfriedGone_Name gottfriedGone_Desc
  gottfriedGone_Art gottfriedGone_Int

harald = Location 22 harald_Name harald_Desc harald_Art harald_Int
haraldTips = Location 99 haraldTips_Name haraldTips_Desc haraldTips_Art
  haraldTips_Int

siegfriede = Location 42 siegfriede_Name siegfriede_Desc siegfriede_Art
  siegfriede_Int

vilPickleGuard = Location 23 vilPickleGuard_Name vilPickleGuard_Desc 
  vilPickleGuard_Art vilPickleGuard_Int

wpsword = Location 24 wpsword_Name wpsword_Desc wpsword_Art wpsword_Int



-- interactions of the locations listed above ----------------------------------

-- all possible interactions for the beach (west of the coast)
beach_Int :: Map.Map String (GameState -> IO GameState) 
beach_Int = Map.fromList [
  ("Nnorden", (\gmst -> do gameLoop (changeLoc gmst vilEntrWest) [""])),
  
  ("Nwesten", (\gmst -> do
    if (getEvent gmst "beachWest") == 1
    then gameLoop (setEvent gmst "beachWest" 0) n_BCwest2
    else gameLoop (setEvent gmst "beachWest" 1) n_BCwest)),
    
  ("Nosten", (\gmst -> do gameLoop (changeLoc gmst coast) [""])),
  
  ("Nsueden", (\gmst -> do
    if (getEvent gmst "PIPguardQstn") == 1
    then gameLoop (setEvent gmst "beachGuardPIP" 1) r_beachGuardPIP
    else
      if (getEvent gmst "gottfriedGone") == 1
      then gameLoop (changeLoc gmst beachGuard) [""]
      else
        if (getEvent gmst "beachSouth") == 1
        then gameLoop (setEvent gmst "beachSouth" 0) n_BCsouth2
        else gameLoop (setEvent gmst "beachSouth" 1) n_BCsouth)),    
                          
  ("Rwache", (\gmst -> do
    if (getEvent gmst "PIPguardQstn") == 1
    then gameLoop (setEvent gmst "beachGuardPIP" 1) r_beachGuardPIP
    else
      if (getEvent gmst "gottfriedGone") == 1
      then gameLoop (changeLoc gmst beachGuard) [""]
      else
        if (getEvent gmst "talkedToBeachGuard") == 1
        then gameLoop (setEvent gmst "talkedToBeachGuard" 0) r_BCguard2
        else gameLoop (setEvent gmst "talkedToBeachGuard" 1) r_BCguard)),
    
  ("Uwache", (\gmst -> do gameLoop gmst u_guard)),  
                          
  ("Rsanta", (\gmst -> do
    if (getEvent gmst "talkedtoSanta") == 1
    then gameLoop gmst r_BCsanta2
    else gameLoop (setItemEvent gmst "eis" 1 "talkedtoSanta" 1) r_BCsanta)),

  ("Usanta", (\gmst -> do gameLoop gmst u_BCsanta)),  
                          
  ("Beiswagen", (\gmst -> do
    if (getItem gmst "eis") < 4
    then gameLoop (setItemEvent gmst "eis" 1 "iceThief" 1) b_BCicetrolley
    else gameLoop gmst b_BCicetrolley2)),
  ("Ueiswagen", (\gmst -> do gameLoop gmst u_BCicetrolley)),
    
  ("j", (\gmst -> 
    if (getEvent gmst "beachGuardPIP" ) == 1
    then gameLoop (changeLoc gmst perlinselnPortEmpty) [""]
    else do gameLoop gmst [""]))]
                          
-- all interactions with the guard at the beach before visiting perlinseln
beachGuard_Int :: Map.Map String (GameState -> IO GameState)
beachGuard_Int = Map.fromList [
  ("zu den perlinseln", (\gmst -> do 
    if (getEvent gmst "gottfriedMissing") /= 1
    then gameLoop gmst r_BCguard3
    else gameLoop gmst r_BCguard4)),
    
  ("j", (\gmst -> do
    if (getEvent gmst "gottfriedMissing") /= 1
    then gameLoop (changeLoc gmst perlinselnPort) [""]
    else gameLoop (changeLoc gmst perlinselnPortGFDead) [""])), 
    
  ("n", (\gmst -> do gameLoop (changeLoc gmst beach) [""])),
  
  ("gottfried", (\gmst -> do 
    if (getEvent gmst "gottfriedMissing") /= 1
    then gameLoop gmst r_BCguard5
    else gameLoop gmst r_BCguard6)),
    
  ("tschau", (\gmst -> do gameLoop (changeLoc gmst beach) [""]))]

-- all possible interactions for the black tower
blackTower_Int :: Map.Map String (GameState -> IO GameState)
blackTower_Int = Map.fromList [
  ("Nosten", (\gmst -> do 
    if (getEvent gmst "swordPickedUp") == 0
    then gameLoop (changeLoc gmst westernPath) [""]
    else gameLoop (changeLoc gmst westernPath2) [""])),
        
  ("Nwesten", (\gmst -> do gameLoop gmst n_DTwest)),  
  ("Nnorden", (\gmst -> do gameLoop gmst n_DTnorth)),
  ("Nsueden", (\gmst -> do gameLoop gmst n_DTsouth)),
  ("Bturmtor", (\gmst -> do 
    if (getEvent gmst "final") == 0
    then gameLoop gmst b_DTclosedGate
    else gameLoop (changeLoc gmst god) $ godHallo1 ++ 
           [godHallo2 ++ playerName gmst ++ ".\n\n\n"] ++ godHallo3 )),
  ("Rturmtor", (\gmst -> do gameLoop gmst r_DTclosedGate)),
  ("Uturmtor", (\gmst -> do gameLoop gmst u_DTclosedGate)),
  ("Iroter armreif",(\gmst -> do gameLoop gmst b_bracelet2))]     
    
-- all possible interactions for coast (east of the beach)
coast_Int :: Map.Map String (GameState -> IO GameState) 
coast_Int = Map.fromList [
  ("Nnorden", (\gmst -> do gameLoop (changeLoc gmst vilTownSquare) [""])),
  ("Nwesten", (\gmst -> do gameLoop (changeLoc gmst beach) [""])),
  ("Nosten", (\gmst -> do gameLoop gmst n_COAeast)),
  ("Nsueden", (\gmst -> do gameLoop gmst n_COAsouth)),
  ("Ranglerin", (\gmst -> do 
    if (getEvent gmst "blueOrbPickedUp") == 1
    then gameLoop gmst r_COAfisher4
    else
      if (getItem gmst "fisch") > 2
      then gameLoop (setItemEvent gmst "blauer orb" 1 "blueOrbPickedUp" 1) 
             r_COAfisher3
      else
        if (getItem gmst "angel") > 0
        then gameLoop gmst r_COAfisher2
        else gameLoop (addItem gmst "angel" 1) r_COAfisher))]
                          
-- all possible interactions for the inside of the castle
cstlInside_Int :: Map.Map String (GameState -> IO GameState)
cstlInside_Int = Map.fromList [
  ("Nnorden", (\gmst -> do 
    if (getEvent gmst "gottfriedGone") == 0
    then gameLoop (changeLoc gmst gottfried) [""]
    else gameLoop (changeLoc gmst gottfriedGone) [""])),
  ("Nsueden", (\gmst -> do gameLoop (changeLoc gmst cstlOutside) [""])),
  ("Btueren", (\gmst -> do gameLoop gmst b_CIdoors)),
  ("Utueren", (\gmst -> do gameLoop gmst u_CIdoors))]
                          
-- all possible interactions for the outside of the castle 
-- (south of the eastern path)
cstlOutside_Int :: Map.Map String (GameState -> IO GameState)
cstlOutside_Int = Map.fromList [
  ("Nnorden", (\gmst -> do gameLoop (changeLoc gmst easternPath) [""])),
  ("Nwesten", (\gmst -> do gameLoop gmst n_COwest)),
  ("Nosten", (\gmst -> do gameLoop (changeLoc gmst easternPath) n_COeast)),
  ("Nsueden", (\gmst -> do gameLoop gmst n_COsouth)),
  ("Bburgtor", (\gmst -> do gameLoop (changeLoc gmst cstlInside) [""])),
  ("Rwachen", (\gmst -> do 
    if (getEvent gmst "gottfriedGone") == 1
    then gameLoop gmst r_COguardGFGone
    else gameLoop gmst r_COguard))]
                        
-- all possible interactions with the dog
dog_Int :: Map.Map String (GameState -> IO GameState) 
dog_Int = Map.fromList [
  ("stoeckchen werfen", (\gmst -> do
    if (getItem gmst "stock") >= 1
    then gameLoop gmst dogThrowStick
    else gameLoop gmst dogThrowStick2)),
    
  ("streicheln", (\gmst -> gameLoop gmst dogPet)),
  ("rekursion erklaeren", (\gmst -> gameLoop gmst dogRecursion)),
  ("weggehen", (\gmst -> gameLoop (changeLoc gmst vilEntrWest) dogLeave))]
                                                          

-- all possible interactions for the eastern path
easternPath_Int :: Map.Map String (GameState -> IO GameState) 
easternPath_Int = Map.fromList [
  ("Nnorden", (\gmst -> do gameLoop gmst n_EPnorth)),
  ("Nwesten", (\gmst -> do gameLoop (changeLoc gmst vilTownSquare) [""])),
  ("Nosten", (\gmst -> do  
    if (getEvent gmst "mountainAccess") == 1
    then 
      if not $ (getEvent gmst "gottfriedGone") == 1
      then gameLoop (changeLoc gmst mountain) [""]
      else 
        if (getEvent gmst "gottfriedMissing") == 1
        then gameLoop (changeLoc gmst mountainSF) [""]
        else gameLoop (changeLoc gmst mountainGF) [""]
    else
      if (getEvent gmst "gottfriedHallo" ) == 1|| 
           (getEvent gmst "gottfriedMissing" ) == 1
      then gameLoop (setEvent gmst "EPguardQuestion" 1) r_EPguard2
      else gameLoop gmst r_EPguard)),
  ("Nsueden", (\gmst -> do gameLoop (changeLoc gmst cstlOutside) [""])),
  ("Uwegweiser", (\gmst -> do gameLoop gmst u_EPsign)),
  ("Rwache", (\gmst -> do   
    if (getEvent gmst "mountainAccess") == 1
    then gameLoop gmst r_EPguard3
    else
      if (getEvent gmst "gottfriedHallo" ) == 1 || 
           (getEvent gmst "gottfriedMissing" ) == 1
      then gameLoop (setEvent gmst "EPguardQuestion" 1) r_EPguard2
      else gameLoop gmst r_EPguard)),
  ("j", (\gmst -> do 
    if (getEvent gmst "EPguardQuestion" ) == 1
    then gameLoop (changeLoc (setEvent gmst "mountainAccess" 1) mountain) [""]
    else do gameLoop gmst [""]))]
    
-- all possible interactions with the elder at the fountain
elder_Int :: Map.Map String (GameState -> IO GameState) 
elder_Int = Map.fromList [
  ("hallo", (\gmst -> do
  if (getEvent gmst "elderHallo") == 0
  then gameLoop gmst elderHallo
  else gameLoop gmst elderHallo2)),
  
  ("wer bist du", (\gmst -> do gameLoop gmst elderWerBistDu)),
  ("was geht hier vor", (\gmst -> do gameLoop gmst elderWasGehtHierVor)),
  ("die lange", (\gmst -> do gameLoop gmst elderLangeVersion)),
  ("die kurze", (\gmst -> do gameLoop gmst elderKurzeVersion)),
  ("orden", (\gmst -> do gameLoop gmst elderOrden)),
  ("monster", (\gmst -> do gameLoop gmst elderMonster)),
  ("helden", (\gmst -> do gameLoop gmst elderHelden)),
  ("andere Siedlungen", (\gmst -> do gameLoop gmst elderAndereSiedlungen)),
  ("was soll ich tun", (\gmst -> do gameLoop gmst elderWasSollIchTun)),  
  ("tschau", (\gmst -> do gameLoop (setLocEvent gmst vilFountain "elderHallo" 1)
                           [""]))]
                               
-- no interactions for the endscreen!
endScreen_Int :: Map.Map String (GameState -> IO GameState)
endScreen_Int = Map.fromList []

-- all possible interactions with frida at the fountain
frida_Int :: Map.Map String (GameState -> IO GameState) 
frida_Int = Map.fromList [
  ("hallo", (\gmst -> do
    if (getEvent gmst "elderHallo") == 0
    then gameLoop (changeLoc gmst vilFountain) fridaHallo
    else gameLoop gmst fridaHallo2)),
  
  ("dhalien", (\gmst -> do gameLoop gmst fridaDhalien)),
  ("helden", (\gmst -> do gameLoop gmst fridaHelden)),
  ("grab vor dem dorf", (\gmst -> do gameLoop gmst fridaGrabVorDemDorf)),
  ("tschau", (\gmst -> do gameLoop (changeLoc gmst vilFountain) [""]))]

-- all possible interactions with gottfried and siegfriede in camp
gfsf_Int :: Map.Map String (GameState -> IO GameState) 
gfsf_Int = Map.fromList [
  ("siegfriede", (\gmst -> do gameLoop gmst gfsfSiegfriede)),
  ("was ist passiert", (\gmst -> do gameLoop gmst gfsfWasIstPassiert)),
  ("plan", (\gmst -> do gameLoop (setEvent gmst "metGFSF" 1) gfsfPlan)),
  ("tschau", (\gmst -> do gameLoop (changeLoc gmst perlinselnCampGF) [""]))]

-- the endscreen
god_Int :: Map.Map String (GameState -> IO GameState) 
god_Int = Map.fromList [
  ("harald", (\gmst -> do gameLoop gmst godHarald)),
  ("monster und helden", (\gmst -> do gameLoop gmst godMuH)),
  ("der turm", (\gmst -> do gameLoop gmst godTurm)),
  ("was jetzt?", (\gmst -> do gameLoop (changeLoc gmst god2) godWasJetzt)),
  ("was jetzt", (\gmst -> do gameLoop (changeLoc gmst god2) godWasJetzt))]

god2_Int :: Map.Map String (GameState -> IO GameState) 
god2_Int = Map.fromList [
  ("nein", (\gmst -> 
    do putStrLn $ art $ location gmst
       putStrLn god2Nein
       _ <- getLine
       gameLoop (changeLoc gmst endScreen) [""])),
  ("ich habe gerne geholfen", (\gmst -> 
    do putStrLn $ art $ location gmst
       putStrLn god2Gerne
       _ <- getLine
       gameLoop (changeLoc gmst endScreen) [""])),
  ("arschloch", (\gmst -> 
    do putStrLn $ art $ location gmst
       putStrLn god2Arsch
       _ <- getLine
       gameLoop (changeLoc gmst endScreen) [""])),
  ("nein ich habe gerne geholfen arschloch", (\gmst -> 
    do putStrLn $ art $ location gmst
       putStrLn god2Alles
       _ <- getLine
       gameLoop (changeLoc gmst endScreen) [""])),
  ("ich habe gerne geholfen arschloch", (\gmst -> 
    do putStrLn $ art $ location gmst
       putStrLn god2Alles
       _ <- getLine
       gameLoop (changeLoc gmst endScreen) [""])),
  ("nein, ich habe gerne geholfen, arschloch", (\gmst -> 
    do putStrLn $ art $ location gmst
       putStrLn god2Alles
       _ <- getLine
       gameLoop (changeLoc gmst endScreen) [""])),
  ("ich habe gerne geholfen, arschloch", (\gmst -> 
    do putStrLn $ art $ location gmst
       putStrLn god2Alles
       _ <- getLine
       gameLoop (changeLoc gmst endScreen) [""])),
  ("nein ich habe gerne geholfen, arschloch", (\gmst -> 
    do putStrLn $ art $ location gmst
       putStrLn god2Alles
       _ <- getLine
       gameLoop (changeLoc gmst endScreen) [""])),
  ("nein, ich habe gerne geholfen arschloch", (\gmst -> 
    do putStrLn $ art $ location gmst
       putStrLn god2Alles
       _ <- getLine
       gameLoop (changeLoc gmst endScreen) [""]))]
  
-- all possible interactions with gottfried in the castle
gottfried_Int :: Map.Map String (GameState -> IO GameState) 
gottfried_Int = Map.fromList [
  ("hallo", (\gmst -> do 
    if (getEvent gmst "bowTaken") == 1
    then gameLoop (setEvent gmst "gottfriedGone" 1) gottfriedHalloBogen
    else
      if (getEvent gmst "gottfriedHallo") == 0
      then gameLoop gmst gottfriedHallo
      else gameLoop gmst gottfriedHallo2)),
    
  ("aufgabe", (\gmst -> do
    gameLoop (setEvent gmst "gottfriedAufgabe" 1) gottfriedAufgabe)),
  ("n", (\gmst -> do 
    if (getEvent gmst "gottfriedAufgabe") == 1
    then gameLoop (setEvent gmst "gottfriedJN" 1) gottfriedNein 
    else gameLoop gmst gottfriedAufgabe)),
    
  ("j", (\gmst -> do 
    if (getEvent gmst "gottfriedAufgabe") == 1
    then gameLoop (setEvent gmst "gottfriedJN" 2) gottfriedJa
    else gameLoop gmst gottfriedAufgabe)),
    
  ("du hast recht", (\gmst -> do
    if (getEvent gmst "gottfriedJN") == 1
    then gameLoop (setEvent (setLocEvent gmst gottfriedGone "gottfriedGone" 1)
           "gottfriedMissing" 1) gottfriedNeinRecht
    else 
      if (getEvent gmst "gottfriedJN") == 2
      then gameLoop (setEvent (setEvent gmst "gottfriedJN" 0) 
           "gottfriedAufgabe" 0) gottfriedJaRecht
      else gameLoop (setEvent gmst "gottfriedJN" 0) gottfriedAufgabe)),
    
  ("was ist passiert", (\gmst -> do 
    gameLoop (setEvent gmst "gottfriedHallo" 1) gottfriedWasIstPassiert)),
    
  ("perlinseln", (\gmst -> do gameLoop gmst gottfriedPerlinseln)),
  ("helden", (\gmst -> do gameLoop gmst gottfriedHelden)),
  ("orden", (\gmst -> do gameLoop gmst gottfriedOrden)),
  ("tschau", (\gmst -> do gameLoop (changeLoc gmst cstlInside) [""]))]
  
-- the castle's main room when gottfried's gone
gottfriedGone_Int :: Map.Map String (GameState -> IO GameState) 
gottfriedGone_Int = Map.fromList [
  ("Nsueden", (\gmst -> do gameLoop (changeLoc gmst cstlInside) [""])),
  ("Nwesten", (\gmst -> do gameLoop (changeLoc gmst cstlInside) [""])),
  ("Nnorden", (\gmst -> do gameLoop (changeLoc gmst cstlInside) [""])),
  ("Nosten", (\gmst -> do gameLoop (changeLoc gmst cstlInside) [""])),
  ("tschau", (\gmst -> do gameLoop (changeLoc gmst cstlInside) [""]))]

-- all possible interactions with Harald
harald_Int :: Map.Map String (GameState -> IO GameState) 
harald_Int =  Map.fromList [
  ("hallo", (\gmst -> do gameLoop gmst haraldHallo)),
  
  ("ich bin verwirrt",(\gmst -> do 
    gameLoop (setEvent gmst "haraldConfused" 1) haraldIchBinVerwirrt)),
    
  ("wer bist du", (\gmst -> do
    if (getEvent gmst "haraldConfused") == 1
    then gameLoop gmst haraldWerBistDu
    else gameLoop gmst haraldWerBistDu2)),
    
  ("erzaehl mir mehr", (\gmst -> do gameLoop gmst haraldErzaehlMirMehr)),
    
  ("tschau", (\gmst -> do gameLoop (changeLoc gmst vilEntrWest) [""]))]

-- all possible interactions with Harald after meeting the elder
haraldTips_Int :: Map.Map String (GameState -> IO GameState) 
haraldTips_Int =  Map.fromList [
  ("n", (\gmst -> do gameLoop (changeLoc gmst vilEntrWest) [""])),
  ("tschau", (\gmst -> do gameLoop (changeLoc gmst vilEntrWest) [""])),
  ("j", (\gmst -> do 
    if (getEvent gmst "tombMiddleOrb") == 1 &&
       (getEvent gmst "tombLeftOrb") == 1 &&
       (getEvent gmst "tombRightOrb") == 1
    then gameLoop gmst hTipsFire
    else 
      if (getEvent gmst "orbPickedUp") == 1
      then gameLoop (changeLoc gmst vilEntrWest) hTipsOrbs
      else
        if (getEvent gmst "activatedBracelet") == 1
        then gameLoop (changeLoc gmst vilEntrWest) hTipsBracelet
        else
          if (getEvent gmst "campGuardFish") == 2
          then gameLoop (changeLoc gmst vilEntrWest) hTipsCoin
          else
            if (getEvent gmst "campGuardFish") == 1
            then gameLoop (changeLoc gmst vilEntrWest) hTipsFRod
            else
              if (getEvent gmst "metSF") == 1 || (getEvent gmst "metGFSF") == 1
              then gameLoop (changeLoc gmst vilEntrWest) hTipsCamp
              else 
                if (getEvent gmst "bowFound") == 1
                then gameLoop (changeLoc gmst vilEntrWest) hTipsPuzzlePost
                else 
                  if (getEvent gmst "mountainAccess") == 1
                  then gameLoop (changeLoc gmst vilEntrWest) hTipsPuzzlePre
                  else gameLoop (changeLoc gmst vilEntrWest) hTipsOther))]
                                
-- all possible interactions for the outside of the castle 
-- (south of the eastern path)                                
mountain_Int :: Map.Map String (GameState -> IO GameState)
mountain_Int = Map.fromList [
  ("Nnorden", (\gmst -> do
    if (getEvent gmst "MTguardBribed") == 0
    then gameLoop gmst n_MTnorth
    else 
      if (getEvent gmst "bowFound") == 0
      then gameLoop (changeLoc gmst mountainCave) [""]
      else
        if (getEvent gmst "bowTaken") == 0
        then gameLoop (changeLoc gmst mountainCaveS) [""]
        else gameLoop (changeLoc gmst mountainCaveT) [""])),
        
  ("Nwesten", (\gmst -> do gameLoop (changeLoc gmst easternPath) [""])),
  
  ("Nosten", (\gmst -> do 
    if (getEvent gmst "mountainEast") == 0
    then gameLoop (setEvent gmst "mountainEast" 1) n_MTeast
    else gameLoop (setEvent gmst "mountainEast" 0) n_MTeast2)),
    
  ("Nsueden", (\gmst -> do gameLoop gmst n_MTsouth)),
  
  ("Rrechte wache", (\gmst -> do 
    if (getEvent gmst "mountainEast") == 0
    then gameLoop (setEvent gmst "mountainEast" 1) n_MTeast
    else gameLoop (setEvent gmst "mountainEast" 0) n_MTeast2)),
    
  ("Rhoehlen wache", (\gmst -> do gameLoop gmst r_MTguard)),
                             
  ("Igurke", (\gmst -> do 
  if (getItem gmst "gurke") >= 1 
  then gameLoop (addItem (setEvent gmst "MTguardBribed" 1) "gurke" (-1))
                  b_MTpickle
  else gameLoop gmst [""])),
  
  ("Ieis", (\gmst -> do 
  if (getItem gmst "eis") >= 1 
  then gameLoop gmst b_MTice
  else gameLoop gmst [""])),
  
  ("Briss", (\gmst -> do
    if (getEvent gmst "greenOrbPickedUp") == 0
    then gameLoop (setItemEvent gmst "gruener orb" 1 "greenOrbPickedUp" 1) 
           b_MTcrack
    else gameLoop gmst b_MTcrack2)),
  
  ("Uriss", (\gmst -> do 
    if (getEvent gmst "greenOrbPickedUp") == 1
    then gameLoop gmst u_MTcrack
    else gameLoop gmst u_MTcrack2))]
  
-- all possible interactions for the outside of the castle 
-- (after returning from perlinseln with Gottfried dead)
mountainSF_Int :: Map.Map String (GameState -> IO GameState)
mountainSF_Int = Map.fromList [
  ("Nnorden", (\gmst -> do
    if (getEvent gmst "MTguardBribed") == 0
    then gameLoop gmst n_MTnorth
    else 
      if (getEvent gmst "bowFound") == 0
      then gameLoop (changeLoc gmst mountainCave) [""]
      else
        if (getEvent gmst "bowTaken") == 0
        then gameLoop (changeLoc gmst mountainCaveS) [""]
        else gameLoop (changeLoc gmst mountainCaveT) [""])),
        
  ("Nwesten", (\gmst -> do gameLoop (changeLoc gmst easternPath) [""])),
  
  ("Nosten", (\gmst -> do 
    if (getEvent gmst "mountainEast") == 0
    then gameLoop (setEvent gmst "mountainEast" 1) n_MTeast
    else gameLoop (setEvent gmst "mountainEast" 0) n_MTeast2)),
    
  ("Nsueden", (\gmst -> do gameLoop gmst n_MTsouth)),
  
  ("Rrechte wache", (\gmst -> do 
    if (getEvent gmst "mountainEast") == 0
    then gameLoop (setEvent gmst "mountainEast" 1) n_MTeast
    else gameLoop (setEvent gmst "mountainEast" 0) n_MTeast2)),
    
  ("Rsiegfriede", (\gmst -> do gameLoop gmst r_MTsiegfriede)),
    
  ("Rhoehlen wache", (\gmst -> do gameLoop gmst r_MTguard)),
  
  ("Briss", (\gmst -> do
    if (getEvent gmst "greenOrbPickedUp") == 0
    then gameLoop (setItemEvent gmst "gruener orb" 1 "greenOrbPickedUp" 1) 
           b_MTcrack
    else gameLoop gmst b_MTcrack2)),
  
  ("Uriss", (\gmst -> do 
    if (getEvent gmst "greenOrbPickedUp") == 1
    then gameLoop gmst u_MTcrack
    else gameLoop gmst u_MTcrack2))]
  
-- all possible interactions for the outside of the castle 
-- (after returning from perlinseln with Gottfried still alive)
mountainGF_Int :: Map.Map String (GameState -> IO GameState)
mountainGF_Int = Map.fromList [
  ("Nnorden", (\gmst -> do
    if (getEvent gmst "MTguardBribed") == 0
    then gameLoop gmst n_MTnorth
    else 
      if (getEvent gmst "bowFound") == 0
      then gameLoop (changeLoc gmst mountainCave) [""]
      else
        if (getEvent gmst "bowTaken") == 0
        then gameLoop (changeLoc gmst mountainCaveS) [""]
        else gameLoop (changeLoc gmst mountainCaveT) [""])),
        
  ("Nwesten", (\gmst -> do gameLoop (changeLoc gmst easternPath) [""])),
  
  ("Nosten", (\gmst -> do 
    if (getEvent gmst "mountainEast") == 0
    then gameLoop (setEvent gmst "mountainEast" 1) n_MTeast
    else gameLoop (setEvent gmst "mountainEast" 0) n_MTeast2)),
    
  ("Nsueden", (\gmst -> do gameLoop gmst n_MTsouth)),
  
  ("Rrechte wache", (\gmst -> do 
    if (getEvent gmst "mountainEast") == 0
    then gameLoop (setEvent gmst "mountainEast" 1) n_MTeast
    else gameLoop (setEvent gmst "mountainEast" 0) n_MTeast2)),
    
  ("Rsiegfriede", (\gmst -> do gameLoop gmst r_MTsiegfriede)),
  ("Rgottfried", (\gmst -> do gameLoop gmst r_MTgottfried)),
    
  ("Rhoehlen wache", (\gmst -> do gameLoop gmst r_MTguard)),
  
  ("Briss", (\gmst -> do
    if (getEvent gmst "greenOrbPickedUp") == 0
    then gameLoop (setItemEvent gmst "gruener orb" 1 "greenOrbPickedUp" 1) 
           b_MTcrack
    else gameLoop gmst b_MTcrack2)),
  
  ("Uriss", (\gmst -> do 
    if (getEvent gmst "greenOrbPickedUp") == 1
    then gameLoop gmst u_MTcrack
    else gameLoop gmst u_MTcrack2))]

-- all possible interactions for the riddle cave                             
mountainCave_Int :: Map.Map String (GameState -> IO GameState) 
mountainCave_Int = Map.fromList [
  ("Nsueden", (\gmst -> do 
    if not $ (getEvent gmst "gottfriedGone") == 1
    then gameLoop (changeLoc gmst mountain) [""]
    else 
      if (getEvent gmst "gottfriedMissing") == 1
      then gameLoop (changeLoc gmst mountainSF) [""]
      else gameLoop (changeLoc gmst mountainGF) [""])),

  ("Rwache", (\gmst -> do
    if (getEvent gmst "mountainEast") == 0
    then gameLoop (setEvent gmst "mountainEast" 1) r_MTCguard
    else
      if (getEvent gmst "mountainEast") == 1
      then gameLoop (setEvent gmst "mountainEast" 2) r_MTCguard2
      else gameLoop (setEvent gmst "mountainEast" 0) r_MTCguard3)),
      
  ("Uwache", (\gmst -> gameLoop gmst u_guard)),
                                 
  ("Bhebel", (\gmst -> do gameLoop (changeLoc gmst mountainCaveR) [""]))]
        
-- all possible interactions with the levers in the riddle cave
mountainCaveR_Int :: Map.Map String (GameState -> IO GameState) 
mountainCaveR_Int = Map.fromList [
  ("Nsueden", (\gmst -> do gameLoop (changeLoc gmst mountainCave) [""])),
  ("Nnorden", (\gmst -> do gameLoop (changeLoc gmst mountainCave) [""])),
  ("Nwesten", (\gmst -> do gameLoop (changeLoc gmst mountainCave) [""])),
  ("Nosten", (\gmst -> do gameLoop (changeLoc gmst mountainCave) [""])),
  ("zurueck", (\gmst -> do gameLoop (changeLoc gmst mountainCave) [""])),
                              
  ("9630528417", (\gmst -> do gameLoop (GameState (playerName gmst) 
                               mountainCaveS (inventory gmst) 
                                (Map.insert "bowFound" 1 $ events gmst)) [""]))]
                                  
-- all possible interactions for the riddle cave after the bow has been found
mountainCaveS_Int :: Map.Map String (GameState -> IO GameState) 
mountainCaveS_Int = Map.fromList [
  ("Nsueden", (\gmst -> do 
    if not $ (getEvent gmst "gottfriedGone") == 1
    then gameLoop (changeLoc gmst mountain) [""]
    else 
      if (getEvent gmst "gottfriedMissing") == 1
      then gameLoop (changeLoc gmst mountainSF) [""]
      else gameLoop (changeLoc gmst mountainGF) [""])),
  ("Rwache", (\gmst -> do gameLoop gmst r_MTCguard4)),
  ("Uwache", (\gmst -> do gameLoop gmst u_guard)),                             
  ("Bbogen", (\gmst -> do 
    gameLoop (changeLoc (setItemEvent gmst "bogen" 1 "bowTaken" 1) mountainCaveT
      ) b_MTCbow)),
  ("Ubogen", (\gmst -> do gameLoop gmst u_MTCbow)), 
  ("Bhebel", (\gmst -> do gameLoop gmst b_MTChebel))]
                                                                   
-- all possible interactions for the riddle cave after the bow has been taken
mountainCaveT_Int :: Map.Map String (GameState -> IO GameState) 
mountainCaveT_Int = Map.fromList [
  ("Nsueden", (\gmst -> do 
    if not $ (getEvent gmst "gottfriedGone") == 1
    then gameLoop (changeLoc gmst mountain) [""]
    else 
      if (getEvent gmst "gottfriedMissing") == 1
      then gameLoop (changeLoc gmst mountainSF) [""]
      else gameLoop (changeLoc gmst mountainGF) [""])),
  ("Bhebel", (\gmst -> do gameLoop gmst b_MTChebel))]
  
-- all possible inteactions for the camp
-- gottfried is dead
perlinselnCamp_Int :: Map.Map String (GameState -> IO GameState) 
perlinselnCamp_Int = Map.fromList [
  ("Nnorden", (\gmst -> do 
    gameLoop (changeLoc gmst perlinselnsouthernPath) [""])),
  ("Rsiegfriede", (\gmst -> do gameLoop (changeLoc gmst siegfriede) [""])),
  ("Rverletzter", (\gmst -> do gameLoop gmst r_PICwounded)),
  ("Rwache", (\gmst -> do gameLoop gmst r_PICguard))]

-- all possible inteactions for the abandoned camp    
perlinselnCamp2_Int :: Map.Map String (GameState -> IO GameState) 
perlinselnCamp2_Int = Map.fromList [
  ("Nnorden", (\gmst -> do 
    gameLoop (changeLoc gmst perlinselnsouthernPath) [""])),
  ("Ifisch", (\gmst -> do
    if (getItem gmst "fisch") > 0 && (getEvent gmst "campGuardFish") == 1
    then do putStrLn ("-----------------------\n")
            putStrLn (art $ location gmst)
            putStrLn $ r_PICFishguard !! 0
            _ <- getLine
            won <- gameStart
            
            if won
            then gameLoop (setItemEvent (addItem gmst "fisch" (-1)) "muenze" 1 
                             "campGuardFish" 2) r_PICFishguard4
            else gameLoop gmst r_PICFishguard5
    else gameLoop gmst [""])),
  ("Rwache", (\gmst -> do
    if (getItem gmst "fisch") > 0 && (getEvent gmst "campGuardFish") == 1
    then do putStrLn ("-----------------------\n")
            putStrLn (art $ location gmst)
            putStrLn $ r_PICFishguard !! 0
            _ <- getLine
            won <- gameStart
            
            if won
            then gameLoop (setItemEvent (addItem gmst "fisch" (-1)) "muenze" 1 
                             "campGuardFish" 2) r_PICFishguard4
            else gameLoop gmst r_PICFishguard5
    else 
      if (getEvent gmst "campGuardFish") == 2
      then do _ <- gameStart
              gameLoop gmst r_PICFishguard6
      else gameLoop (setEvent gmst "campGuardFish" 1) r_PICFishguard3))]
    
-- all possible inteactions for the camp
-- gottfried is alive
perlinselnCampGF_Int :: Map.Map String (GameState -> IO GameState) 
perlinselnCampGF_Int = Map.fromList [
  ("Nnorden", (\gmst -> do 
    gameLoop (changeLoc gmst perlinselnsouthernPath) [""])),
  ("Rgottfried", (\gmst -> do gameLoop (changeLoc gmst gfsf) [""])),
  ("Rverletzter", (\gmst -> do gameLoop gmst r_PICwounded)),
  ("Rwache", (\gmst -> do gameLoop gmst r_PICguard))]
  
-- all possible inteactions for the southern path on the perlinseln
-- gottfried is dead
perlinselnsouthernPath_Int :: Map.Map String (GameState -> IO GameState) 
perlinselnsouthernPath_Int = Map.fromList [
  ("Nnorden", (\gmst -> do gameLoop (changeLoc gmst perlinselnPortEmpty) [""])),
  ("Nsueden", (\gmst -> do
    if (getEvent gmst "metSF") == 1 || (getEvent gmst "metGFSF") == 1
    then gameLoop (changeLoc gmst perlinselnCamp2) [""]
    else 
      if (getEvent gmst "gottfriedMissing") == 1
      then gameLoop (changeLoc gmst perlinselnCamp) [""]
      else gameLoop (changeLoc gmst perlinselnCampGF) [""])),
  ("Bstuhl", (\gmst -> do 
    if (getItem gmst "angel") > 0
    then gameLoop (addItem gmst "fisch" 1) b_PIWchair
    else gameLoop gmst b_PIWchair2)),
  ("Ustuhl", (\gmst -> do gameLoop gmst u_PIWchair)),
  ("Iangel", (\gmst -> do
    if (getItem gmst "angel") > 0
    then gameLoop (addItem gmst "fisch" 1) b_PIWangel
    else gameLoop gmst [""]))]
  
-- all possible inteactions for the southern path on the perlinseln  
perlinselnsouthernPathGF_Int :: Map.Map String (GameState -> IO GameState) 
perlinselnsouthernPathGF_Int = Map.fromList [
  ("Nnorden", (\gmst -> do gameLoop (changeLoc gmst perlinselnPortEmpty) [""])),
  ("Nsueden", (\gmst -> do gameLoop (changeLoc gmst perlinselnCampGF) [""])),
  ("Rgottfried", (\gmst -> do gameLoop gmst r_PIWgottfried)),
  ("Bstuhl", (\gmst -> do gameLoop gmst r_PIWgottfried2)),
  ("Ustuhl", (\gmst -> do gameLoop gmst r_PIWgottfried2))]

-- all possible interactions for the port on the perlinseln
perlinselnPort_Int :: Map.Map String (GameState -> IO GameState) 
perlinselnPort_Int = Map.fromList [
  ("Nsueden", (\gmst -> do 
    gameLoop (changeLoc gmst perlinselnsouthernPathGF) [""])),
  ("Rgottfried", (\gmst -> do gameLoop gmst r_PIPgottfried)),
  ("Rwachen", (\gmst -> do gameLoop gmst r_PIPwachen)),
  ("Bfaesser", (\gmst -> do 
    if (getEvent gmst "PIPbarrels") == 0
    then gameLoop (setEvent gmst "PIPbarrels" 1) b_PIPbarrels
    else 
      if (getEvent gmst "PIPbarrels") == 1
      then gameLoop (setEvent gmst "PIPbarrels" 2) b_PIPbarrels2
      else gameLoop (setEvent gmst "PIPbarrels" 0) b_PIPbarrels3)),
  ("Ufaesser", (\gmst -> do gameLoop gmst u_PIPbarrels)),
  ("Bkatapult", (\gmst -> do gameLoop gmst b_PIPcatapult)),
  ("Ukatapult", (\gmst -> do gameLoop gmst u_PIPcatapult))]
  
-- all possible interactions for the port on the perlinseln, the second visit
perlinselnPortEmpty_Int :: Map.Map String (GameState -> IO GameState) 
perlinselnPortEmpty_Int = Map.fromList [
  ("Nsueden", (\gmst -> do 
    gameLoop (changeLoc gmst perlinselnsouthernPath) [""])),
  ("Nnorden",(\gmst -> do 
    gameLoop (setEvent gmst "PIPguardQstn" 1) r_PIPwachen2)),
  ("Rwachen", (\gmst -> do 
    gameLoop (setEvent gmst "PIPguardQstn" 1) r_PIPwachen2)),
  ("j", (\gmst -> do
    if (getEvent gmst "PIPguardQstn") == 1
    then gameLoop (changeLoc gmst beach) [""]
    else gameLoop gmst [""])),
  ("Bfaesser", (\gmst -> do 
    if (getEvent gmst "PIPbarrels") == 0
    then gameLoop (setEvent gmst "PIPbarrels" 1) b_PIPbarrels
    else 
      if (getEvent gmst "PIPbarrels") == 1
      then gameLoop (setEvent gmst "PIPbarrels" 2) b_PIPbarrels2
      else gameLoop (setEvent gmst "PIPbarrels" 0) b_PIPbarrels3)),
  ("Ufaesser", (\gmst -> do gameLoop gmst u_PIPbarrels)),
  ("Bkatapult", (\gmst -> do gameLoop gmst b_PIPcatapult)),
  ("Ukatapult", (\gmst -> do gameLoop gmst u_PIPcatapult))]

-- all possible interactions for the port on the perlinseln. Gottfried is dead
perlinselnPortGFDead_Int :: Map.Map String (GameState -> IO GameState) 
perlinselnPortGFDead_Int = Map.fromList [ 
  ("Nsueden", (\gmst -> do 
    gameLoop (changeLoc gmst perlinselnsouthernPath) [""])),
  ("Rwachen", (\gmst -> do gameLoop gmst r_PIPwachenGFDead)),
  ("Bfaesser", (\gmst -> do 
    if (getEvent gmst "PIPbarrels") == 0
    then gameLoop (setEvent gmst "PIPbarrels" 1) b_PIPbarrels
    else 
      if (getEvent gmst "PIPbarrels") == 1
      then gameLoop (setEvent gmst "PIPbarrels" 2) b_PIPbarrels2
      else gameLoop (setEvent gmst "PIPbarrels" 0) b_PIPbarrels3)),
  ("Ufaesser", (\gmst -> do gameLoop gmst u_PIPbarrels)),
  ("Bkatapult", (\gmst -> do gameLoop gmst b_PIPcatapult)),
  ("Ukatapult", (\gmst -> do gameLoop gmst u_PIPcatapult))]
  
--all possible interactions with siegfriede in camp
siegfriede_Int :: Map.Map String (GameState -> IO GameState) 
siegfriede_Int = Map.fromList [
  ("siegfriede", (\gmst -> do gameLoop gmst siegfriedeSiegfriede)),
  ("was ist passiert", (\gmst -> do gameLoop gmst siegfriedeWasIstPassiert)),
  ("gottfried", (\gmst -> do gameLoop gmst siegfriedeGottfried)),
  ("plan", (\gmst -> do gameLoop (setEvent gmst "metSF" 1) siegfriedePlan)),
  ("tschau", (\gmst -> do gameLoop (changeLoc gmst perlinselnCamp) [""]))]

-- all possible interactions for the tomb entrance
tombEntrance_Int :: Map.Map String (GameState -> IO GameState) 
tombEntrance_Int = Map.fromList [
  ("Nnorden", (\gmst -> do 
    if (getEvent gmst "orbPickedUp") == 1
    then gameLoop (changeLoc gmst tombMiddle) [""]
    else gameLoop (changeLoc gmst tombMiddleOrbs) [""])),
  ("Nsueden", (\gmst -> do gameLoop (changeLoc gmst westernPath3) [""])),
  ("Ubild links", (\gmst -> do gameLoop gmst u_TBEpicleft)),
  ("Ubild rechts", (\gmst -> do gameLoop gmst u_TBEpicright))] 

-- all possible interactions for the tomb middle part
tombMiddle_Int :: Map.Map String (GameState -> IO GameState) 
tombMiddle_Int = Map.fromList [
  ("Nwesten", (\gmst -> do gameLoop (changeLoc gmst tombLeft) [""])),
  ("Nosten", (\gmst -> do gameLoop (changeLoc gmst tombRigth) [""])),
  ("Nsueden", (\gmst -> do gameLoop (changeLoc gmst tombEntrance) [""])),
  ("Ustatue", (\gmst -> do gameLoop gmst u_TBMstatue)),
  ("Uklappen", (\gmst -> do gameLoop gmst u_TBMklappen)),
  ("Ufeuerschalen", (\gmst -> do gameLoop gmst u_TBMfire)),
  ("Bfeuerschalen", (\gmst -> do 
    if (getEvent gmst "tombMiddleOrb") == 1 &&
       (getEvent gmst "tombLeftOrb") == 1 &&
       (getEvent gmst "tombRightOrb") == 1
    then gameLoop (setEvent gmst "final" 1) b_TBMfire2
    else gameLoop gmst b_TBMfire)),
  ("Iroter orb", (\gmst -> do 
    if (getItem gmst "roter orb") >= 1
    then gameLoop (setItemEvent gmst "roter orb" (-1) "tombMiddleOrb" 1) b_TBred
    else gameLoop gmst [""])),
  
  ("Igruener orb", (\gmst -> do gameLoop gmst b_TBunfit)),  
  ("Iblauer orb", (\gmst -> do gameLoop gmst b_TBunfit))] 

-- all possible interactions for the tomb middle part with orbs
tombMiddleOrbs_Int :: Map.Map String (GameState -> IO GameState) 
tombMiddleOrbs_Int = Map.fromList [
  ("Nwesten", (\gmst -> do gameLoop (changeLoc gmst tombLeft) [""])),
  ("Nosten", (\gmst -> do gameLoop (changeLoc gmst tombRigth) [""])),
  ("Nsueden", (\gmst -> do gameLoop (changeLoc gmst tombEntrance) [""])),
  ("Ustatue", (\gmst -> do gameLoop gmst u_TBMstatue)),
  ("Uklappen", (\gmst -> do gameLoop gmst u_TBMklappen)),
  ("Ufeuerschalen", (\gmst -> do gameLoop gmst u_TBMfire)),
  ("Bfeuerschalen", (\gmst -> do gameLoop gmst b_TBMfire)),
  ("Bkugel", (\gmst -> do gameLoop (setItemEvent (changeLoc gmst tombMiddle) 
                             "roter orb" 1 "orbPickedUp" 1) b_TBMorb)),
  ("Ukugel", (\gmst -> do gameLoop gmst u_TBMorb))] 

-- all possible interactions for the tomb left part
tombLeft_Int :: Map.Map String (GameState -> IO GameState) 
tombLeft_Int = Map.fromList [
  ("Nosten", (\gmst -> do 
    if (getEvent gmst "orbPickedUp") == 1
    then gameLoop (changeLoc gmst tombMiddle) [""]
    else gameLoop (changeLoc gmst tombMiddleOrbs) [""])),
  ("Ustatue", (\gmst -> do gameLoop gmst u_TBLstatue)),
  ("Ugemaelde", (\gmst -> do gameLoop gmst u_TBLpainting)),
  ("Uklappen", (\gmst -> do gameLoop gmst u_TBLklappen)),
  ("Ufeuerschalen", (\gmst -> do gameLoop gmst u_TBLfire)),
  ("Iroter orb", (\gmst -> do gameLoop gmst b_TBunfit)),
  
  ("Igruener orb", (\gmst -> do gameLoop gmst b_TBunfit)),
  
  ("Iblauer orb", (\gmst -> do 
    if (getItem gmst "blauer orb") >= 1
    then gameLoop (setItemEvent gmst "blauer orb" (-1) "tombLeftOrb" 1) b_TBblue
    else gameLoop gmst [""]))] 

-- all possible interactions for the tomb right part
tombRigth_Int :: Map.Map String (GameState -> IO GameState) 
tombRigth_Int = Map.fromList [
  ("Nwesten", (\gmst -> do 
    if (getEvent gmst "orbPickedUp") == 1
    then gameLoop (changeLoc gmst tombMiddle) [""]
    else gameLoop (changeLoc gmst tombMiddleOrbs) [""])),
  ("Ustatue", (\gmst -> do gameLoop gmst u_TBRstatue)),
  ("Ugemaelde", (\gmst -> do gameLoop gmst u_TBRpainting)),
  ("Uklappen", (\gmst -> do gameLoop gmst u_TBRklappen)),
  ("Ulaternen", (\gmst -> do gameLoop gmst u_TBRfire)),
  ("Iroter orb", (\gmst -> do gameLoop gmst b_TBunfit)),
  
  ("Igruener orb", (\gmst -> do 
    if (getItem gmst "gruener orb") >= 1
    then gameLoop (setItemEvent gmst "gruener orb"(-1)"tombRightOrb" 1) b_TBgreen
    else gameLoop gmst [""])),
  
  ("Iblauer orb", (\gmst -> do gameLoop gmst b_TBunfit))] 

-- all possible interactions for the the village Entrance near the black Tower
vilEntrWest_Int :: Map.Map String (GameState -> IO GameState) 
vilEntrWest_Int = Map.fromList [ 
  ("Nwesten", (\gmst -> do 
    if (getEvent gmst "swordPickedUp") == 0
    then gameLoop (changeLoc gmst westernPath) [""]
    else gameLoop (changeLoc gmst westernPath2) [""])),
        
  ("Nnorden", (\gmst -> do gameLoop (changeLoc gmst vilFountain) [""])),
  ("Nosten", (\gmst -> do gameLoop (changeLoc gmst vilTownSquare) [""])),
  ("Nsueden", (\gmst -> do gameLoop (changeLoc gmst beach) [""])),
  ("Barbeiter", (\gmst -> do 
    if (getEvent gmst "dogPetted") == 1
    then gameLoop gmst b_VEWharaldPet
    else gameLoop gmst b_VEWharaldStandard)), 
    
  ("Rarbeiter", (\gmst -> do 
    if (getEvent gmst "elderHallo") == 0
    then gameLoop (changeLoc gmst harald) [""]
    else gameLoop (changeLoc gmst haraldTips) [""])),
  ("Uarbeiter", (\gmst -> do gameLoop gmst u_VEWarbeiter)),   
  ("Bhund", (\gmst -> do gameLoop (setEvent gmst "dogPetted" 1) b_VEWdog)),     
  ("Rhund", (\gmst -> do gameLoop (changeLoc gmst dog) [""])),  
  ("Uhund", (\gmst -> do gameLoop gmst u_VEWdog)),                              
  ("Bbaum", (\gmst -> do 
    if (getEvent gmst "stickAtTree") == 1
    then gameLoop gmst b_VEWtree2
    else gameLoop (setItemEvent gmst "stock" 1 "stickAtTree" 1) b_VEWtree)),   
                                                      
  ("Rbaum", (\gmst -> gameLoop gmst r_VEWbaum)),         
  ("Ubaum", (\gmst -> gameLoop gmst u_VEWbaum)),  
  ("Bbaumloch", (\gmst -> do gameLoop gmst b_VEWtreehole)),   
  ("Rbaumloch", (\gmst -> do gameLoop gmst r_VEWbaumloch)),   
  ("Ubaumloch", (\gmst -> do gameLoop gmst u_VEWbaumloch)),   
  ("Istock", (\gmst -> do
    if (getItem gmst "stock") >= 1
    then gameLoop gmst dogThrowStick
    else gameLoop gmst [""]))]

-- all possible interactions for the fountain area in the village
vilFountain_Int :: Map.Map String (GameState -> IO GameState) 
vilFountain_Int = Map.fromList [
  ("Nosten", (\gmst -> do gameLoop (changeLoc gmst vilPickle) [""])),
  ("Nsueden", (\gmst -> do gameLoop (changeLoc gmst vilEntrWest) [""])),
  ("Rvorsteher", (\gmst -> do gameLoop (changeLoc gmst elder) [""])),
  ("Uvorsteher", (\gmst -> do gameLoop gmst u_VFelder)),
  ("Rgaertnerin", (\gmst -> do gameLoop (changeLoc gmst frida) [""])),
  ("Ugaertnerin", (\gmst -> do gameLoop gmst u_VFgaertnerin)),
  ("Ubrunnen", (\gmst -> do gameLoop gmst u_VFfountain)),
  ("Bbrunnen", (\gmst -> do 
    if (getEvent gmst "activatedBracelet") == 0 && (getItem gmst "muenze") > 0
    then gameLoop (setItemEvent gmst "muenze" (-1) "activatedBracelet" 1) 
           b_VFfountain2
    else 
      if (getEvent gmst "activatedBracelet") == 0
      then gameLoop gmst b_VFfountain
      else gameLoop gmst b_VFfountain3)),
  ("Imuenze", (\gmst -> do 
    if (getEvent gmst "activatedBracelet") == 0 && (getItem gmst "muenze") > 0
    then gameLoop (setItemEvent gmst "muenze" (-1) "activatedBracelet" 1) 
           b_VFfountain2
    else 
      if (getEvent gmst "activatedBracelet") == 0
      then gameLoop gmst b_VFfountain
      else gameLoop gmst b_VFfountain3))]
                              
--  all possible interactions for the outside of the castle
-- (south of the eastern path)
vilPickle_Int :: Map.Map String (GameState -> IO GameState) 
vilPickle_Int = Map.fromList [
  ("Nwesten", (\gmst -> do gameLoop (changeLoc gmst vilFountain) [""])),
  ("Nsueden", (\gmst -> do gameLoop (changeLoc gmst vilTownSquare) [""])), 
  ("Rwache", (\gmst -> do gameLoop (changeLoc gmst vilPickleGuard) [""])),
  ("Uwache", (\gmst -> do gameLoop gmst u_VPIguard)),
  ("Bgasthaus", (\gmst -> do gameLoop gmst b_VPIinn)),
  ("Ugasthaus", (\gmst -> do gameLoop gmst u_VPIinn))]
                                          
-- all possible interactions for the pickle guard
vilPickleGuard_Int :: Map.Map String (GameState -> IO GameState) 
vilPickleGuard_Int = Map.fromList [
  ("hallo", (\gmst -> do gameLoop gmst pickleGuardHallo)),
  ("wer bist du", (\gmst -> do 
    if (getEvent gmst "pickleGuard") == 0
    then gameLoop gmst pickleGuardWerBistDu
    else gameLoop gmst pickleGuardWerBistDu2)),
    
  ("was machst du", (\gmst -> do gameLoop (setEvent gmst "pickleGuard" 1) 
                                   pickleGuardWasmachstDu)),
                                   
  ("kann ich ne gurke haben", (\gmst -> do 
    if (getEvent gmst "pickleGuard") == 0
    then gameLoop gmst [""]
    else gameLoop (addItem gmst "gurke" 1) pickleGuardGurke)),
    
  ("tschau", (\gmst -> do gameLoop (changeLoc gmst vilPickle) 
                            pickleGuardTschau))]
                             
-- all possible interactions for the outside of the castle
-- (south of the eastern path)
vilTownSquare_Int :: Map.Map String (GameState -> IO GameState)
vilTownSquare_Int = Map.fromList [
  ("Nnorden", (\gmst -> do gameLoop (changeLoc gmst vilPickle) [""])),
  ("Nwesten", (\gmst -> do gameLoop (changeLoc gmst vilEntrWest) [""])),
  ("Nosten", (\gmst -> do gameLoop (changeLoc gmst easternPath) [""])),
  ("Nsueden", (\gmst -> do gameLoop (changeLoc gmst coast) [""])),
  ("Rhelfer", (\gmst -> do gameLoop gmst r_TSguy)),
  ("Uweihnachtsbaum", (\gmst -> do gameLoop gmst u_TStree))] 
                                             
--all possible interactions for the path between the black tower and the village
westernPath_Int :: Map.Map String (GameState -> IO GameState)
westernPath_Int = Map.fromList [
  ("Nwesten", (\gmst -> do gameLoop (changeLoc gmst blackTower) [""])),
  ("Nosten", (\gmst -> do gameLoop (changeLoc gmst vilEntrWest) [""])),
  ("Nnorden", (\gmst -> do gameLoop gmst n_WPnorth)),
  ("Nsueden", (\gmst -> do gameLoop gmst n_WPsouth)),
  ("Bsenke", (\gmst -> do gameLoop gmst b_WPhole)),
  ("Rsenke", (\gmst -> do gameLoop gmst r_WPhole)),
  ("Usenke", (\gmst -> do gameLoop gmst u_WPhole)),
  ("Bkreuz", (\gmst -> do gameLoop gmst b_WPcross)),
  ("Rkreuz", (\gmst -> do gameLoop gmst r_WPcross)),
  ("Ukreuz", (\gmst -> do gameLoop gmst u_WPcross)),
  ("Bschwert", (\gmst -> do gameLoop (changeLoc gmst wpsword) [""])),
  ("Rschwert", (\gmst -> do gameLoop gmst r_WPsword)),
  ("Uschwert", (\gmst -> do gameLoop gmst u_WPsword)),
  ("Iroter armreif",(\gmst -> do gameLoop gmst b_bracelet3))]
                                                   
 -- this is just to confirm that the player really wants to take the sword
wpsword_Int :: Map.Map String (GameState -> IO GameState)
wpsword_Int = Map.fromList [
  ("j", (\gmst -> do gameLoop (GameState (playerName gmst) westernPath2
                       (Map.insert "schwert" 1 $ inventory gmst)
                         (Map.insert "swordPickedUp" 1 $ events gmst))
                           b_WPswordj)),
                           
  ("n", (\gmst -> do gameLoop (changeLoc gmst westernPath) b_WPswordn))]
             
--all possible interactions for the path between the black tower and the village
-- (without the sword)
westernPath2_Int :: Map.Map String (GameState -> IO GameState) 
westernPath2_Int = Map.fromList [
  ("Nwesten", (\gmst -> do gameLoop (changeLoc gmst blackTower) [""])),
  ("Nosten", (\gmst -> do gameLoop (changeLoc gmst vilEntrWest) [""])),
  ("Nnorden", (\gmst -> do gameLoop gmst n_WPnorth)),
  ("Nsueden", (\gmst -> do gameLoop gmst n_WPsouth)),
  ("Bsenke", (\gmst -> do gameLoop gmst b_WPhole)),
  ("Rsenke", (\gmst -> do gameLoop gmst r_WPhole)),
  ("Usenke", (\gmst -> do gameLoop gmst u_WPhole)),
  ("Bkreuz", (\gmst -> do gameLoop gmst b_WPcross)),
  ("Rkreuz", (\gmst -> do gameLoop gmst r_WPcross)),
  ("Ukreuz", (\gmst -> do gameLoop gmst u_WPcross)),
  ("Iroter armreif",(\gmst -> do 
    if (getEvent gmst "activatedBracelet") == 1
    then gameLoop (changeLoc gmst westernPath3) b_bracelet4
    else gameLoop gmst b_bracelet2))]

--all possible interactions for the path between the black tower and the village
-- (after opening the tomb)
westernPath3_Int :: Map.Map String (GameState -> IO GameState) 
westernPath3_Int = Map.fromList [
  ("Nwesten", (\gmst -> do gameLoop (changeLoc gmst blackTower) [""])),
  ("Nosten", (\gmst -> do gameLoop (changeLoc gmst vilEntrWest) [""])),
  ("Nnorden", (\gmst -> do gameLoop (changeLoc gmst tombEntrance) [""])),
  ("Nsueden", (\gmst -> do gameLoop gmst n_WPsouth)),
  ("Bloch", (\gmst -> do gameLoop (changeLoc gmst tombEntrance) [""])),
  ("Uloch", (\gmst -> do gameLoop gmst u_WPhole2))]



-- stuff -----------------------------------------------------------------------

 --this is the gamestate on startup
start =  GameState "PlatzhalterName" blackTower startInv startEvents
        where startEvents = Map.fromList [("PlayerMoney", 0), ("startup", 1)]

-- input that exits the game
quitCharacter = [":q", ":e", "exit", "quit"] 

 --the players items at start
startInv = Map.fromList [("roter armreif", 1)]

--actions that are not bound to locations such as interactions with items
generalActions = Map.fromList [
  ("Bangel", (\gmst -> do gameLoop gmst b_angel)),
  ("Uangel", (\gmst -> do gameLoop gmst i_angel)),
  ("Beis", (\gmst -> do 
    if (getEvent gmst "iceThief") == 0
    then gameLoop (addItem gmst "eis" $ -1) b_icecream2
    else do gameLoop (addItem gmst "eis" $ -1) b_icecream)),
    
  ("Ueis", (\gmst -> do gameLoop gmst i_icecream)),
  ("Bfisch", (\gmst -> do gameLoop gmst b_fisch)),
  ("Ufisch", (\gmst -> do gameLoop gmst i_fisch)),
  ("Ugurke", (\gmst -> do gameLoop gmst i_pickle)),
  ("Bgurke", (\gmst -> do gameLoop (addItem gmst "gurke" $ -1) b_pickle)),
  ("Umuenze", (\gmst -> do gameLoop gmst i_muenze)), 
  ("Broter armreif", (\gmst -> do gameLoop gmst b_bracelet)),               
  ("Uroter armreif", (\gmst -> do
    if (getEvent gmst "activatedBracelet") == 1
    then gameLoop gmst i_redBracelet2
    else gameLoop gmst i_redBracelet)),
  ("Bschwert", (\gmst -> do gameLoop gmst b_sword)), 
  ("Bstock",(\gmst -> do gameLoop gmst b_stick)),
  ("Ustock", (\gmst -> do gameLoop gmst i_stick))]
                               
                               

