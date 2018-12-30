module InitialBoard where

import Data.List(elemIndex, elemIndices)

-- import our modlues

import Model
import HelperFunctions(convPixel, convPixelInt)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- function, which converts the sizeValues from InitBoard-module to BoardSize-object

-- | function, which generates a BoardSize with a given tripel
tripel2BoardSize  :: (Int, Int, Float)  -- ^ board-values-tupel
                  -> BoardSize          -- ^ BoardSize-object
tripel2BoardSize (w, h, s) = BoardSize w h s

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- convert-functions to generate the initial Board

-- | convert-function: converts InitBoard-object to a list of walls with the certain positions, !decide if [Wall] or [[Wall]]!
convertToWalls  :: InitBoard  -- ^ InitBoard-Object
                -> Int        -- ^ start-parameter for the row-recursion. It should be 0.
                -> [Wall]     -- ^ list of wall-objects
convertToWalls (InitBoard [] _)  _ = []
convertToWalls (InitBoard bins sB@(BoardSize w h s)) m = convertRow  (take w bins) 0 ++ convertToWalls (InitBoard(drop w bins) sB) (m+1)
  where
    convertRow [] _ = []
    convertRow (b:bs) n   | b == 2                           = [OutOfField (Pt (xOffset+ fromIntegral(n) * s) y')] ++ convertRow bs (n+1)
                          | b == 1                           = [Wall (Pt (xOffset+ fromIntegral(n) * s) y')] ++ convertRow bs (n+1)
                          | (b == 0) || (b == 8) || (b == 9) = [Path (Pt (xOffset + fromIntegral(n) * s) y')] ++ convertRow bs (n+1)
                          | b == 10                          = [GhostStart (Pt (xOffset+ fromIntegral(n) * s) y')] ++ convertRow bs (n+1)
                          | b == 20                          = [FreWall (Pt (xOffset+ fromIntegral(n) * s) y')] ++ convertRow bs (n+1)
                          | otherwise                        = [NotAllowedForPM (Pt (xOffset+ fromIntegral(n) * s) y')] ++ convertRow bs (n+1)

    xOffset = 0.5 * s
    y' = (convPixel s h) - (0.5 * s + fromIntegral(m) * s)

-- | convert-function: converts InitBoard-object to a pacman-object with the certain position
convertToPacman :: InitBoard  -- ^ InitBoard-object
                -> Pacman     -- ^ Pacman-object
convertToPacman (InitBoard bins sB@(BoardSize w h s)) = Player NormalMode NoAnimation Mortal (HorizontalDirection 0.0) (Pt (0.5 * s + fromIntegral(fst coord) * s) ((convPixel s h) - (0.5 * s + fromIntegral(snd coord) * s))) (NoTimer) NotCollided
  where
    coord = case (elemIndex 9 bins) of
              Nothing -> (0, 0)
              Just a  -> (a `mod` w, floor(fromIntegral(a)/fromIntegral(w)))

-- | convert-function: converts InitBoard-object to the ghost-objects with certain positions
convertToGhosts :: InitBoard  -- ^ InitBoard-object
                -> [Pt]       -- ^ list of target points for the FixedPattern Ghost
                -> [Ghost]    -- ^ list of Ghost-objects, It should be 4, so each from a different constructor
convertToGhosts (InitBoard bins sB@(BoardSize w h s)) pts = convGhost 3
  where
    convGhost 7 = []
    convGhost n = case n of
                    3 -> [FixedPatternOne Immortal (GhostAnimation 0 (SetTimer changeAnGhosts)) (HorizontalDirection fixedVelocityGhost) (genPt n) pts InBase (Destiny (Pt (getX (genPt n)+s) (getY (genPt n)) ) ) (NoTimer) NotCollided] ++ convGhost (n+1)
                    4 -> [FollowTypeOne Immortal (GhostAnimation 0 (SetTimer changeAnGhosts)) (HorizontalDirection fixedVelocityGhost) (genPt n) InBase (Destiny (Pt (getX (genPt n)+s) (getY (genPt n)) ) ) (NoTimer) NotCollided] ++ convGhost (n+1)
                    5 -> [FollowTypeTwo Immortal (GhostAnimation 0 (SetTimer changeAnGhosts)) (HorizontalDirection ((-1) *fixedVelocityGhost)) (genPt n) InBase (Destiny (Pt (getX (genPt n)-s) (getY (genPt n)) ) ) (NoTimer) NotCollided] ++ convGhost (n+1)
                    6 -> [FollowTypeThree Immortal (GhostAnimation 0 (SetTimer changeAnGhosts)) (HorizontalDirection ((-1) *fixedVelocityGhost)) (genPt n) InBase (Destiny (Pt (getX (genPt n)-s) (getY (genPt n)) ) ) (NoTimer) NotCollided] ++ convGhost (n+1)

    genPt x = Pt (0.5 * s + fromIntegral(fst (coord x)) * s) ((convPixel s h) - (0.5 * s + fromIntegral(snd (coord x)) * s))

    coord el = case (elemIndex el bins) of
                  Nothing -> (0, 0)
                  Just a  -> (a `mod` w, floor(fromIntegral(a)/fromIntegral(w)))

-- | convert-function: converts InitBoard-object to the food-objects with certain positions
convertToFoods  :: InitBoard  -- ^ InitBoard-object
                -> [Food]     -- ^ list of food-objects
convertToFoods (InitBoard bins sB@(BoardSize w h s)) = genFood coodList
  where
    genFood [] = []
    genFood (i:is) = (Fd (genPt (fst i) (snd i))):genFood is

    genPt x y = Pt (0.5 * s + fromIntegral(x) * s) ((convPixel s h) - (0.5 * s + fromIntegral(y) * s))

    coodList = map coods (elemIndices 0 bins)

    coods a = (a `mod` w, floor(fromIntegral(a)/fromIntegral(w)))

-- | convert-function: converts InitBoard-object to the food-objects with certain positions
convertToPowerUps  :: InitBoard     -- ^ InitBoard-object
                   -> [Powerup]     -- ^ list of powerup-objects
convertToPowerUps (InitBoard bins sB@(BoardSize w h s)) =  genPowerUps coodList
  where
    genPowerUps [] = []
    genPowerUps (i:is) = (Chest (genPt (fst i) (snd i))):genPowerUps is

    genPt x y = Pt (0.5 * s + fromIntegral(x) * s) ((convPixel s h) - (0.5 * s + fromIntegral(y) * s))

    coodList = map coods (elemIndices 8 bins)

    coods a = (a `mod` w, floor(fromIntegral(a)/fromIntegral(w)))

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Generate the initial game state

-- | Generate the initial InitBoard-object
genInitBoard  :: [Int]              -- ^ list of Ints, which represents the board
              -> (Int, Int, Float)  -- ^ tripel, which contains the information about the boardsize
              -> InitBoard          -- ^ InitBoard-Object, which has all necessary information of the board-configuration
genInitBoard startBoard trBo = InitBoard startBoard (tripel2BoardSize trBo)

-- | Generate the initial GameState
initialGameState  :: InitBoard  -- ^ all information about the inital game board
                  -> GameState  -- ^ the generated initial game board
initialGameState initBoard = Game initBoard (World sc lf tf) (Board (Static walls foods powerups) (Dynamic pacman ghosts )) (Running) (Timer 0.0)
  where
    -- world features
    sc = Score 0
    lf = ThreeLives
    tf = TotalFood (length foods)
    -- board features
    walls = convertToWalls initBoard 0
    foods = convertToFoods initBoard
    powerups = convertToPowerUps initBoard
    pacman = convertToPacman initBoard
    ghosts = convertToGhosts initBoard (map getPULocation powerups)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
