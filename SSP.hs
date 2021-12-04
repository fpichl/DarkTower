module SSP where

import Data.Time.Clock.POSIX
import Data.Maybe
import System.IO

-- starts the minigame. Returns True if the Player won
gameStart ::  IO Bool
gameStart = do finalState <- gamePlay ((0,0), (False, 420))
               if (fst $ fst finalState) >= 3
               then return True
               else return False   
                        
-- this is the gameLoop-equivalent. The parameters from left to right:
-- wins for the player, wins for the cpu, has the player won last round, 
-- the cpus hand last round
gamePlay :: ((Int,Int), (Bool, Int)) -> IO ((Int,Int), (Bool, Int))
gamePlay (wins, (won, hand)) = 
  if (fst wins )>= 3 || (snd wins) >= 3
  then do
      putStr art1
      putStr (eyes $ (fst wins) >= 3 )
      putStr art2
      putStr (bars $ fst wins)
      putStr art3
      putStr (bars $ snd wins)
      putStr art4
      putStrLn $ boxes wins won hand
      getEnter <- getLine
      return (wins, (won, hand))
  else
  
    if (fst wins) == 0 && (snd wins) == 0 && not won && hand == 420
    then do putStrLn startArt
            getEnter <- getLine
            gamePlay ((0,0), (True, 0))
    else do
      putStr art1
      putStr (eyes $ (fst wins) >= 3 )
      putStr art2
      putStr (bars $ fst wins)
      putStr art3
      putStr (bars $ snd wins)
      putStr art4
      putStrLn $ boxes wins won hand
    
      playerHand <- getHand
      cpuHand <- randomHand
    
      if playerHand == 0
      then
        if cpuHand == 0
        then gamePlay (wins, (False, 0))
        else
          if cpuHand == 1
          then gamePlay ((fst wins , (snd wins) + 1), (False, 1))
          else gamePlay (((fst wins) + 1, snd wins), (True, 2))
      else
        if playerHand == 1
        then
          if cpuHand == 0
          then gamePlay (((fst wins) + 1, snd wins), (True, 0))
          else
            if cpuHand == 1
            then gamePlay (wins, (False, 1))
            else gamePlay ((fst wins , (snd wins) + 1), (False,2))
        else
          if playerHand == 2
          then
            if cpuHand == 0
            then gamePlay ((fst wins , (snd wins) + 1), (False, 0))
            else
              if cpuHand == 1
              then gamePlay (((fst wins) + 1, snd wins), (True, 1))
              else gamePlay (wins, (False, 2))
          else return (wins, (won, hand))
  
  
-- gets the input
getHand :: IO Int  
getHand =  do hand' <- getLine
              case hand' of
                "schere" -> return 0
                "schere!" -> return 0
                "Schere" -> return 0
                "Schere!" -> return 0
                   
                "stein" -> return 1
                "stein!" -> return 1
                "Stein" -> return 1
                "Stein!" -> return 1
                
                "papier" -> return 2
                "papier!" -> return 2
                "Papier" -> return 2
                "Papier!" -> return 2   
                
                "flucht" -> return 3
                "flucht!" -> return 3
                "Flucht" -> return 3
                "Flucht!" -> return 3      
                
                _ -> do x <- getHand
                        return x        
                   
                   
  
-- returns a random Int in range [0,2]  
randomHand :: IO Int
randomHand = do i <- randomInt
                let a = i `mod` 3
                return a
  
-- returns a random Int in range [0,999]  
randomInt :: IO Int  
randomInt = do 
  zeit <- getPOSIXTime
  let random = read (lastThree $ init $ show zeit) :: Int
  return random
    where    
      lastThree ls 
        | length ls > 3 = lastThree (drop 1 ls)
        | otherwise = ls


-- decides wich eyes are used
eyes :: Bool -> String
eyes bool 
  | not bool = eyesOpen
  | bool = eyesClosed
  
-- decides wich bar is used
bars :: Int -> String
bars wins
  | wins == 0 = barFull
  | wins == 1 = bar23
  | wins == 2 = bar13
  | otherwise = barEmpty
  
-- decides wich box is used
boxes :: (Int, Int) -> Bool -> Int -> String
boxes (wins) won hand
  | (fst wins) >= 3 = boxHwin
  | (snd wins) >= 3 = boxWwin
  | ( (fst wins > 0) || (snd wins > 0) ) && won = if hand == 0
                                                  then boxEffSc
                                                  else
                                                    if hand == 1
                                                    then boxEffSt
                                                    else boxEffPa
  | ( (fst wins > 0) || (snd wins > 0) ) && not won = if hand == 0
                                                      then boxNEffSc
                                                      else
                                                        if hand == 1
                                                        then boxNEffSt
                                                        else boxNEffPa
  | otherwise = boxKampf

-- ascii art -------------------------------------------------------------------

startArt = art1 ++eyesOpen ++art2 ++barFull ++art3 ++barFull ++art4 ++boxStart

art1 = "                                                      ____\n" ++                      
       "___________________                                  /____\\___\n" ++                   
       "| Wache      Lv 3 |                                  | "

eyesOpen = "--"

eyesClosed = "><"

art2 = " |                  \n" ++     
       "|     ___________ |                                   \\__/\n" ++                        
       "|    "

barFull = "|###########|"                                                                  
                                                                                
bar23 = "|#######____|"                                                                   
                                                                                
bar13 = "|###________|"                                                                   
                                                                                
barEmpty = "|___________|"

art3 = "|                                /__|  | \n" ++                      
       "|_________________|                                   |__|\\ \n" ++                     
       "            _______                                 __|__| \\ \n" ++                    
       "        ___/_______\\                               |     |     \n" ++                  
       "           |  '  ' |                               |     |__   \n" ++                  
       "            \\ ___ /  .                                   ___________________\n" ++     
       "             |   |__/                                    | Held       Lv 5 |\n" ++     
       "            /|   |                                       |     ___________ |\n" ++     
       "           / |___|                                       |    "

art4 = "|    \n" ++ 
       "           | |   |                                       |_________________|\n" ++     
       " ____________|___|_____________________________________________________________ \n"

boxStart = "|                                                                              |\n" ++  
           "|  Eine wilde Wache erscheint!                                                 |\n" ++  
           "|                                                                              |\n" ++  
           "|                                                                              |\n" ++  
           "|                                                                              |\n" ++  
           "|                                                                              |\n" ++  
           "|______________________________________________________________________________|\n"

boxKampf = "|                                            |                                 |\n" ++ 
           "|  Was willst du tun?                        | Schere!           Papier!       |\n" ++ 
           "|                                            |                                 |\n" ++ 
           "|                                            |                                 |\n" ++ 
           "|                                            | Stein!            Flucht!       |\n" ++ 
           "|                                            |                                 |\n" ++ 
           "|____________________________________________|_________________________________|\n"

boxEffSc = "|                                            |                                 |\n" ++ 
           "|  Wache(Wild) setzt Schere ein!             | Schere!           Papier!       |\n" ++ 
           "|                                            |                                 |\n" ++ 
           "|  Du warst sehr effektiv!                   |                                 |\n" ++ 
           "|                                            | Stein!            Flucht!       |\n" ++ 
           "|  Was willst du tun?                        |                                 |\n" ++ 
           "|____________________________________________|_________________________________|\n"
           
boxEffSt = "|                                            |                                 |\n" ++ 
           "|  Wache(Wild) setzt Stein ein!              | Schere!           Papier!       |\n" ++ 
           "|                                            |                                 |\n" ++ 
           "|  Du warst sehr effektiv!                   |                                 |\n" ++ 
           "|                                            | Stein!            Flucht!       |\n" ++ 
           "|  Was willst du tun?                        |                                 |\n" ++ 
           "|____________________________________________|_________________________________|\n"
           
boxEffPa = "|                                            |                                 |\n" ++ 
           "|  Wache(Wild) setzt Papier ein!             | Schere!           Papier!       |\n" ++ 
           "|                                            |                                 |\n" ++ 
           "|  Du warst sehr effektiv!                   |                                 |\n" ++ 
           "|                                            | Stein!            Flucht!       |\n" ++ 
           "|  Was willst du tun?                        |                                 |\n" ++ 
           "|____________________________________________|_________________________________|\n"



boxNEffSc = "|                                            |                                 |\n" ++  
            "|  Wache(Wild) setzt Schere ein!             | Schere!           Papier!       |\n" ++ 
            "|                                            |                                 |\n" ++ 
            "|  Du warst nicht sehr effektiv!             |                                 |\n" ++ 
            "|                                            | Stein!            Flucht!       |\n" ++ 
            "|  Was willst du tun?                        |                                 |\n" ++ 
            "|____________________________________________|_________________________________|\n"
           
boxNEffSt = "|                                            |                                 |\n" ++ 
            "|  Wache(Wild) setzt Stein ein!              | Schere!           Papier!       |\n" ++ 
            "|                                            |                                 |\n" ++ 
            "|  Du warst nicht sehr effektiv!             |                                 |\n" ++ 
            "|                                            | Stein!            Flucht!       |\n" ++ 
            "|  Was willst du tun?                        |                                 |\n" ++ 
            "|____________________________________________|_________________________________|\n"
           
boxNEffPa = "|                                            |                                 |\n" ++ 
            "|  Wache(Wild) setzt Papier ein!             | Schere!           Papier!       |\n" ++ 
            "|                                            |                                 |\n" ++ 
            "|  Du warst nicht sehr effektiv!             |                                 |\n" ++ 
            "|                                            | Stein!            Flucht!       |\n" ++ 
            "|  Was willst du tun?                        |                                 |\n" ++ 
            "|____________________________________________|_________________________________|\n"

boxWwin = "|                                                                              |\n" ++ 
          "|  Die Wache gewinnt!                                                          |\n" ++ 
          "|                                                                              |\n" ++ 
          "|                                                                              |\n" ++ 
          "|                                                                              |\n" ++ 
          "|                                                                              |\n" ++ 
          "|______________________________________________________________________________|\n"

boxHwin = "|                                                                              |\n" ++ 
          "|  Der Held gewinnt!                                                           |\n" ++ 
          "|                                                                              |\n" ++ 
          "|  Der Held erhaelt 1$!                                                        |\n" ++ 
          "|                                                                              |\n" ++ 
          "|                                                                              |\n" ++ 
          "|______________________________________________________________________________|\n"
          
