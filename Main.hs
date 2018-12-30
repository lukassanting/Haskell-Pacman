module Main(main) where

import System.Exit(exitSuccess)
import System.IO(readFile)
import Graphics.Gloss.Interface.IO.Game

-- Import our modules
import InitialBoard
import Controller
import Model
import HelperFunctions(convPixel, convPixelInt, tidyScores, getUnivFilePath)
import View

-------------------------------------------------------------------
-- | update the GameState. It contains all the functions, which handle the current GameState and modify it under certain circumstances
update  :: Float          -- ^ timeStep in seconds
        -> GameState      -- ^ former state of the game
        -> IO GameState   -- ^ current state of the game
update seconds = loadHighscore . lookForChest . (updatePosition seconds) . (updateTime seconds) . lookForFood . lookForTimeComp . lookForCol . lookForGhostCol . getMovement .lookGameOverCond

-------------------------------------------------------------------
-------------------------------------------------------------------
-- IO-main
-- | draws the GUI, so that users can see it with using IO-objects
main :: IO ()
main = do
        -- Load the level from file
        loadLevel <- readFile (getUnivFilePath getPathToLvlFre)
        let heightBoard = length $ lines loadLevel
            listEntries = words loadLevel
            widthBoard = floor $ (fromIntegral(length $ listEntries) / fromIntegral(heightBoard))
            -- generate the initial board
            boardPropTripel = (widthBoard, heightBoard, fixedSquareSize)
            bS = tripel2BoardSize boardPropTripel
            level = map read listEntries
            initBoard = genInitBoard level boardPropTripel
            -- generate the game state
            iGS = (initialGameState initBoard)
            --generate a window
            window :: Display -- ^ output-window
            window = InWindow "Pacman" (convPixelInt (getSizeSquare bS) (getWidthBoard bS), convPixelInt (getSizeSquare bS) (getHeightBoard bS)) (10, 10)
            -- generates the background color of the window
            background :: Color -- ^ background color of the window
            background = white

        playIO window background fps (iGS) drawContIO handleKeyEventIO update

  where
    -- convert to IO-objects
    drawContIO :: GameState -> IO Picture
    drawContIO = return . drawCont
    handleKeyEventIO :: Event -> GameState -> IO GameState
    handleKeyEventIO (EventKey (SpecialKey KeyEsc) Down _ _) (Game wo bo bS Pause ti) = exitSuccess
    handleKeyEventIO (EventKey (SpecialKey KeyEsc) Down _ _) (GameOver sc NoLoadedScores bS) = exitSuccess
    handleKeyEventIO (EventKey (SpecialKey KeyEsc) Down _ _) (GameOver sc (LoadedScores _) bS) = exitSuccess
    handleKeyEventIO e gS = return $ (handleKeyEvent e gS)
