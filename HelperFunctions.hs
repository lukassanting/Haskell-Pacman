module HelperFunctions where

import Data.List(intercalate)
import System.FilePath(pathSeparator)

-- import our modlues

import Model


-- Pixel-Conversion
--------------------------------------------------------------------------------

-- | Convert in float-pixel
convPixel :: Float  -- ^ float-pixel
          -> Int    -- ^ int-pixel
          -> Float  -- ^ output: float-pixel
convPixel a b = a * fromIntegral(b)

-- | Convert in int-pixel
convPixelInt  :: Float  -- ^ float-pixel
              -> Int    -- ^ int-pixel
              -> Int    -- ^ output: int-pixel
convPixelInt a b = ceiling(a * fromIntegral(b))


-- Function to reduce the Velocity-value to zero and move Pacman's position one step back
--------------------------------------------------------------------------------
-- | Returns a Pacman-object with a Velocity-object with Float-value of 0 and with one-step-back Position
setVelToZero  :: Pacman -- ^ former Pacman-object
              -> Pacman -- ^ stopped Pacman-object
setVelToZero (Player st an m (VerticalDirection vx) (Pt x y) t col) = Player st an m (VerticalDirection 0.0) (Pt (x - vx*(1 / fromIntegral(fps))) y) t col
setVelToZero (Player st an m (HorizontalDirection vy) (Pt x y) t col) = Player st an m (HorizontalDirection 0.0) (Pt x (y - vy*(1 / fromIntegral(fps))) ) t col


-- Function to stop the dynamic object-moving
--------------------------------------------------------------------------------
-- | Returns a new GameState, in which Pacman stops to move
stopPacman  :: GameState  -- ^ state of game, in which pacman moves
            -> GameState  -- ^ state of game, in which pacman stands still
stopPacman gO@(GameOver _ _ _) = gO
stopPacman (Game iG wo (Board stBo (Dynamic pc gh) ) to ti) = Game iG wo (Board stBo (Dynamic (setVelToZero pc) gh) ) to ti


-- Function to convert a integer till 9 to a No-object
--------------------------------------------------------------------------------
-- | Returns a No-object to the corresponding Int-value or question mark if bigger than 10
convertIntToNo  :: Int  -- ^ the integer, which should be converted
                -> No   -- ^ the converted structure, which represents a integer
convertIntToNo 0 = Zero
convertIntToNo 1 = One
convertIntToNo 2 = Two
convertIntToNo 3 = Three
convertIntToNo 4 = Four
convertIntToNo 5 = Five
convertIntToNo 6 = Six
convertIntToNo 7 = Seven
convertIntToNo 8 = Eight
convertIntToNo 9 = Nine
convertIntToNo _ = QuestionMark


-- Function to convert an integer till 999 to a DecInt-object
--------------------------------------------------------------------------------
-- | Returns a DecInt-object to the corresponding int-value
convertIntToDecInt  :: Int    -- ^ the integer, which should be converted
                    -> DecInt -- ^ the converted structure, which represents a integer
convertIntToDecInt n  | n > 999 = DecInt QuestionMark QuestionMark QuestionMark
                      | n < 0   = DecInt QuestionMark QuestionMark QuestionMark
                      | otherwise = DecInt (convertIntToNo(floor(fromIntegral n / 100))) (convertIntToNo(floor(fromIntegral (n - floor(fromIntegral n / 100) * 100) / 10))) (convertIntToNo(n - 100 * floor(fromIntegral n / 100) - 10 * floor(fromIntegral (n - floor(fromIntegral n / 100) * 100) / 10)))

--------------------------------------------------------------------------------

-- Function to compare the equality of two PositionInBlocks-tupels
cmpPos  :: PositionInBlocks -- ^ coordinate-tupel of object a
        -> PositionInBlocks -- ^ coordinate-tupel of object b
        -> Bool             -- ^ True if coordinate-tupel of a == coordinate-tupel of b
cmpPos (x1, y1) (x2, y2) = (x1 == x2) && (y1 == y2)

--------------------------------------------------------------------------------

-- Function to toggle between the animation frames
setAnimation  :: Frame              -- ^ maximum number of frames for this animation
              -> Float              -- ^ time, which remains in one frame of the animation
              -> Frame              -- ^ former frame of the animation
              -> (Frame, SetTimer)  -- ^ tupel of current frame and current
setAnimation maxF maxTi cF = checkRestart

  where
    -- real no. starts at 0
    noMaxFrame = maxF - 1
    -- check if the animation have to restart
    checkRestart  | cF == noMaxFrame = (0, SetTimer maxTi)
                  | otherwise = ((cF + 1), SetTimer maxTi)

--------------------------------------------------------------------------------

-- | Function to reduce the timer by a timestep, if it is not less or equal to 0
ifGreaterThanZero   :: (Ord a, Num a)   -- ^ to make it type independed
                    => a                -- ^ number, which should be checked
                    -> a                -- ^ number either 0 or its actual value, which is >0
ifGreaterThanZero checkNo   | checkNo < fromIntegral 0 = fromIntegral 0
                            | otherwise     = checkNo

--------------------------------------------------------------------------------

-- | Reduce the speed of pacman to normal velotcity and keep the direction
vel2Normal  :: Velocity -- ^ former velocity
            -> Velocity -- ^ current velocity, which is normal, but preserves direction
vel2Normal (VerticalDirection vy) = VerticalDirection (signum(vy)*fixedVelocityNormal)
vel2Normal (HorizontalDirection vx) = HorizontalDirection (signum(vx)*fixedVelocityNormal)

-- | Increase the speed of pacman to fast velocity and keep the direction
vel2Fast  :: Velocity -- ^ former velocity
          -> Velocity -- ^ current velocity, which is fast, but preserves direction
vel2Fast (VerticalDirection vy) = VerticalDirection (signum(vy)*fixedVelocityFast)
vel2Fast (HorizontalDirection vx) = HorizontalDirection (signum(vx)*fixedVelocityFast)


-- Function to get the wall object on a certain block position
--------------------------------------------------------------------------------
-- | Returns the wall object of a certain block position
getWallOnPos :: Int               -- ^ Width of the board
             -> PositionInBlocks  -- ^ A tuple of two Ints which represent the block location
             -> [Wall]            -- ^ The Board
             -> Wall              -- ^ Returns the wall on that location
getWallOnPos w (xB, yB) ws = ws!!(w * yB + xB)


-- Function to sort the scores from large to small (provided only the first element is out of place)
--------------------------------------------------------------------------------
-- | Returns a sorted list of the scores from largest to smallest
sortScores :: [Int] -- ^ The list of scores, sorted except for the first element
           -> [Int] -- ^ The list of scores, sorted
sortScores (x:y:[]) | x < y     = y : [x]
                    | otherwise = x : [y]
sortScores (x:y:xs) | x < y     = y : sortScores (x:xs)
                    | otherwise = (x:y:xs)


-- Function to tidy the scores by splitting, sorting and returing the scores
--------------------------------------------------------------------------------
-- | Accepts the list of scores (if empty, then use initHighscore), splits them, tidies them and returns them
tidyScores :: Int      -- ^ The new score, to add to old scores
           -> String   -- ^ The old scores (sorted)
           -> [String] -- ^ The new sorted scores to return
tidyScores newScore []        = scores ( scoresStrings ( scoresInts newScore ( oldInts initHighscore )))
  where oldInts oS        = map read (words oS)
        scoresInts nS oS  = sortScores (nS : oS)
        scoresStrings sI  = map show sI
        scores sS         = take 5 sS

tidyScores newScore oldScores = scores ( scoresStrings ( scoresInts newScore (oldInts oldScores) ) )
  where oldInts oS        = map read (words oS)
        scoresInts nS oS  = sortScores (nS : oS)
        scoresStrings sI  = map show sI
        scores sS         = take 5 sS


-- Function to get a Filepath, which is working for every operating system
--------------------------------------------------------------------------------
-- | This function returns a filepath, depending on the current operating system
getUnivFilePath :: [String] -- ^ filepath-parts, which is splitted at "\" (for Windows)
                -> String   -- ^ universal filepath
getUnivFilePath xs = intercalate [pathSeparator] xs
