module Controller where

import System.Random(randomRIO)
import Graphics.Gloss.Interface.Pure.Game

import System.IO(openFile, IOMode(ReadMode), openTempFile, hGetContents, hPutStr, hClose)
import System.Directory(removeFile, renameFile)
import Data.List(find)

-- import our modlues

import Model
import InitialBoard(initialGameState)
import HelperFunctions

-- Runner, Killer, MinusPoint

-- type class to handle the timer
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | new type class to handle the global timer
class Time a where
  updateTime  :: Float  -- ^ time stepsize in seconds
              -> a      -- ^ object in the former time
              -> a      -- ^ obejct in the current time

-- | updateTime GameState
--implementation of instance Time GameState
instance Time GameState where
  updateTime _ gO@(GameOver _ _ _) = gO
  updateTime deltaTime (Game iG ws bs (Pause) ti) = Game iG ws bs (Pause) ti
  updateTime deltaTime (Game iG ws bs (Running) ti) = Game iG ws (updateTime deltaTime bs) (Running) (updateTime deltaTime ti)

-- | updateTime Timer
--implementation of instance Time Timer
instance Time Timer where
  updateTime deltaTime ti = Timer (deltaTime + getTime ti)

-- | updateTime BoardState
--implementation of instance Time BoardState
instance Time BoardState where
  updateTime deltaTime (Board stB dB) = Board stB (updateTime deltaTime dB)

-- | updateTime DynamicBoard
--implementation of instance Time DynamicBoard
instance Time DynamicBoard where
  updateTime deltaTime (Dynamic p gs) = Dynamic (updateTime deltaTime p) (map (updateTime deltaTime) gs)

-- | updateTime Pacman
--implementation of instance Time Pacman
instance Time Pacman where
  updateTime deltaTime (Player st an@(PacmanAnimation _ _) mo vel pt NoTimer col) = Player st NoAnimation mo vel pt NoTimer col
  updateTime deltaTime (Player st an@(PacmanAnimation _ _) mo vel pt t col) = let outputTime = ifGreaterThanZero(getTimeLeft t - deltaTime)
                                                                                      in case outputTime of
                                                                                              0.0 -> Player st NoAnimation mo vel pt NoTimer col
                                                                                              _   -> Player st (updateTime deltaTime an) mo vel pt (SetTimer $ outputTime) col
  updateTime deltaTime (Player st NoAnimation mo vel pt NoTimer col) = Player st NoAnimation mo vel pt NoTimer col
  updateTime deltaTime (Player st NoAnimation mo vel pt t col) =  let outputTime = ifGreaterThanZero(getTimeLeft t - deltaTime)
                                                                  in case outputTime of
                                                                          0.0 -> Player st NoAnimation mo vel pt NoTimer col
                                                                          _   -> Player st NoAnimation mo vel pt (SetTimer $ outputTime) col

-- | updateTime Ghost
--implementation of instance Time Ghost
instance Time Ghost where
  updateTime deltaTime (FixedPatternOne mo gA vel pt pts loc d NoTimer col) = FixedPatternOne mo (updateTime deltaTime gA) vel pt pts loc d NoTimer col
  updateTime deltaTime (FixedPatternOne mo gA vel pt pts loc d t col) =   let   outputTime = ifGreaterThanZero(getTimeLeft t - deltaTime)
                                                                      in case outputTime of
                                                                              0.0 -> FixedPatternOne mo (updateTime deltaTime gA) vel pt pts loc d NoTimer col
                                                                              _   -> FixedPatternOne mo (updateTime deltaTime gA) vel pt pts loc d (SetTimer $ outputTime) col

  updateTime deltaTime (FollowTypeOne mo gA vel pt loc d NoTimer col) = FollowTypeOne mo (updateTime deltaTime gA) vel pt loc d NoTimer col
  updateTime deltaTime (FollowTypeOne mo gA vel pt loc d t col) =   let   outputTime = ifGreaterThanZero(getTimeLeft t - deltaTime)
                                                                in case outputTime of
                                                                        0.0 -> FollowTypeOne mo (updateTime deltaTime gA) vel pt loc d NoTimer col
                                                                        _   -> FollowTypeOne mo (updateTime deltaTime gA) vel pt loc d (SetTimer $ outputTime) col

  updateTime deltaTime (FollowTypeTwo mo gA vel pt loc d NoTimer col) = FollowTypeTwo mo (updateTime deltaTime gA) vel pt loc d NoTimer col
  updateTime deltaTime (FollowTypeTwo mo gA vel pt loc d t col) =   let   outputTime = ifGreaterThanZero(getTimeLeft t - deltaTime)
                                                                in case outputTime of
                                                                        0.0 -> FollowTypeTwo mo (updateTime deltaTime gA) vel pt loc d NoTimer col
                                                                        _   -> FollowTypeTwo mo (updateTime deltaTime gA) vel pt loc d (SetTimer $ outputTime) col

  updateTime deltaTime (FollowTypeThree mo gA vel pt loc d NoTimer col) = FollowTypeThree mo (updateTime deltaTime gA) vel pt loc d NoTimer col
  updateTime deltaTime (FollowTypeThree mo gA vel pt loc d t col) =   let   outputTime = ifGreaterThanZero(getTimeLeft t - deltaTime)
                                                                  in case outputTime of
                                                                          0.0 -> FollowTypeThree mo (updateTime deltaTime gA) vel pt loc d NoTimer col
                                                                          _   -> FollowTypeThree mo (updateTime deltaTime gA) vel pt loc d (SetTimer $ outputTime) col

-- | updateTime GhostAnimation
--implementation of instance Time GhostAnimation
instance Time GhostAnimation where
  updateTime deltaTime (GhostAnimation cF NoTimer) = (uncurry GhostAnimation) (setAnimation noFramesGhost changeAnGhosts cF)
  updateTime deltaTime (GhostAnimation cF (SetTimer ta)) =  let   outputTime = ifGreaterThanZero(ta - deltaTime)
                                                            in case outputTime of
                                                                    0.0 -> GhostAnimation cF NoTimer
                                                                    _   -> GhostAnimation cF (SetTimer $ outputTime)

-- | updateTime PacmanAnimation
instance Time PacmanAnimation where
  updateTime deltaTime NoAnimation = NoAnimation
  updateTime deltaTime (PacmanAnimation cF NoTimer) = (uncurry PacmanAnimation) (setAnimation noFramesPacman changeAnPacman cF)
  updateTime deltaTime (PacmanAnimation cF (SetTimer ta)) = let outputTime = ifGreaterThanZero(ta - deltaTime)
                                                            in case outputTime of
                                                                    0.0 -> PacmanAnimation cF NoTimer
                                                                    _   -> PacmanAnimation cF (SetTimer $ outputTime)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- type class to move the position of the dynamic objects by their velocity
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | new type class to move the position of the dynamic objects by their velotcity
class Inertia a where
  updatePosition  :: Float  -- ^ time stepsize in seconds
                  -> a      -- ^ object at its former position
                  -> a      -- ^ object after moving for a certain time


-- | updatePosition GameState
--implementation of instance Inertia GameState
instance Inertia GameState where
  updatePosition _ gO@(GameOver _ _ _) = gO
  updatePosition timeStep (Game iG ws bs (Pause) ti) = Game iG ws bs (Pause) ti
  updatePosition timeStep (Game iG ws bs (Running) ti) = Game iG ws (updatePosition timeStep bs) (Running) ti

-- | updatePosition BoardState
--implementation of instance Inertia BoardState
instance Inertia BoardState where
  updatePosition timeStep (Board stB dB) = Board stB (updatePosition timeStep dB)

-- | updatePosition DynamicBoard
--implementation of instance Inertia DynamicBoard
instance Inertia DynamicBoard where
  updatePosition timeStep (Dynamic p gs) = Dynamic (updatePosition timeStep p) (map (updatePosition timeStep) gs)

-- | updatePosition Pacman
--implementation of instance Inertia Pacman
instance Inertia Pacman where
  updatePosition timeStep (Player st an mo vel@(VerticalDirection vy) (Pt x y) t col) = Player st an mo vel (Pt x (y + timeStep * vy)) t col
  updatePosition timeStep (Player st an mo vel@(HorizontalDirection vx) (Pt x y) t col) = Player st an mo vel (Pt (x + timeStep * vx) y) t col

-- | updatePosition Ghost
--implementation of instance Inertia Ghost
instance Inertia Ghost where
  updatePosition timeStep (FixedPatternOne mo an vel@(VerticalDirection vy) (Pt x y) pts loc d t col) = FixedPatternOne mo an vel (Pt x (y + timeStep * vy)) pts loc d t  col
  updatePosition timeStep (FixedPatternOne mo an vel@(HorizontalDirection vx) (Pt x y) pts loc d t col) = FixedPatternOne mo an vel (Pt (x + timeStep * vx) y) pts loc d t  col
  updatePosition timeStep (FollowTypeOne mo an vel@(VerticalDirection vy) (Pt x y) loc d t col) = FollowTypeOne mo an vel (Pt x (y + timeStep * vy)) loc d t  col
  updatePosition timeStep (FollowTypeOne mo an vel@(HorizontalDirection vx) (Pt x y) loc d t col) = FollowTypeOne mo an vel (Pt (x + timeStep * vx) y) loc d t  col
  updatePosition timeStep (FollowTypeTwo mo an vel@(VerticalDirection vy) (Pt x y) loc d t col) = FollowTypeTwo mo an vel (Pt x (y + timeStep * vy)) loc d t  col
  updatePosition timeStep (FollowTypeTwo mo an vel@(HorizontalDirection vx) (Pt x y) loc d t col) = FollowTypeTwo mo an vel (Pt (x + timeStep * vx) y) loc d t  col
  updatePosition timeStep (FollowTypeThree mo an vel@(VerticalDirection vy) (Pt x y) loc d t col) = FollowTypeThree mo an vel (Pt x (y + timeStep * vy)) loc d t  col
  updatePosition timeStep (FollowTypeThree mo an vel@(HorizontalDirection vx) (Pt x y) loc d t col) = FollowTypeThree mo an vel (Pt (x + timeStep * vx) y) loc d t col

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- type class to get the position of the dynamic objects in EdgeAndDistDiff
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | new type class to get the position -heading-to -information of the dynamic objects in EdgeAndDistDiff
class DynamicFigurePos a where
  getDynFigLoc   :: BoardSize        -- ^ BoardSize of the current board
              -> a                -- ^ object, whose block position should be determined
              -> EdgeAndDistDiff    -- ^ edge positions and center postition of the object (x, y) in blocks, here the position-coordinates are a little less than on the object boundaries


-- | getDynFigLoc Pacman
--implementation of instance DynamicFigurePos Pacman
instance DynamicFigurePos Pacman where
  getDynFigLoc (BoardSize w h s) (Player _ _ _ (VerticalDirection v) (Pt x y) _ _)      | v > 0.0 = ((floor(x/s), floor(((convPixel s h) - y - (0.5*s))/s)),  x - (0.5*s) - fromIntegral(floor(x/s))*s)
                                                                                        | v == 0.0 = ((floor(x/s), floor(((convPixel s h) - y)/s)), x - (0.5*s) - fromIntegral(floor(x/s))*s)
                                                                                          | otherwise = ((floor(x/s), floor(((convPixel s h) - y + (0.5*s))/s)),  x - (0.5*s) - fromIntegral(floor(x/s))*s)
  getDynFigLoc (BoardSize w h s) (Player _ _ _ (HorizontalDirection v) (Pt x y) _ _)    | v > 0.0 = ((floor((x + (0.5*s))/s), floor(((convPixel s h) - y)/s)),  (y - (convPixel s h) + 0.5*s + fromIntegral(floor(((convPixel s h) - y)/s))*s))
                                                                                        | v == 0.0 = ((floor(x/s), floor(((convPixel s h) - y)/s)), (y - (convPixel s h) + 0.5*s + fromIntegral(floor(((convPixel s h) - y)/s))*s))
                                                                                        | otherwise = ((floor((x - (0.5*s))/s), floor(((convPixel s h) - y)/s)),  (y - (convPixel s h) + 0.5*s + fromIntegral(floor(((convPixel s h) - y)/s))*s))

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- type class to get the static position of objects in PositionInBlocks
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | new type class to get the position of objects in PositionInBlocks
class StaticFigurePos a where
  getStatFigLoc   :: BoardSize        -- ^ BoardSize of the current board
                  -> a                -- ^ object, whose block position should be determined
                  -> PositionInBlocks    -- ^ center postition of the object (x, y) in blocks


-- | getStatFigLoc Food
--implementation of instance StaticFigurePos Food
instance StaticFigurePos Food where
  getStatFigLoc (BoardSize w h s) (Fd (Pt x y)) = (floor(x/s), floor(((convPixel s h) - y)/s))
  getStatFigLoc (BoardSize w h s) (NoFd (Pt x y)) = (floor(x/s), floor(((convPixel s h) - y)/s))

-- | getStatFigLoc Chest
--implementation of instance StaticFigurePos Chest
instance StaticFigurePos Powerup where
  getStatFigLoc (BoardSize w h s) (EmptyChest (Pt x y)) = (floor(x/s), floor(((convPixel s h) - y)/s))
  getStatFigLoc (BoardSize w h s) (Chest (Pt x y)) = (floor(x/s), floor(((convPixel s h) - y)/s))

-- | getStatFigLoc Pacman
--implementation of instance StaticFigurePos Pacman
instance StaticFigurePos Pacman where
  getStatFigLoc (BoardSize w h s) (Player _ _ _ _ (Pt x y) _ _) = (floor(x/s), floor(((convPixel s h) - y)/s))

-- | getStatFigLoc Ghost
--implementation of instance StaticFigurePos Ghost
instance StaticFigurePos Ghost where
  getStatFigLoc (BoardSize w h s) (FixedPatternOne _ _ _ (Pt x y) _ _ _ _ _)   = (floor(x/s), floor(((convPixel s h) - y)/s))
  getStatFigLoc (BoardSize w h s) (FollowTypeOne _ _ _ (Pt x y) _ _ _ _)       = (floor(x/s), floor(((convPixel s h) - y)/s))
  getStatFigLoc (BoardSize w h s) (FollowTypeTwo _ _ _ (Pt x y) _ _ _ _)       = (floor(x/s), floor(((convPixel s h) - y)/s))
  getStatFigLoc (BoardSize w h s) (FollowTypeThree _ _ _ (Pt x y) _ _ _ _)     = (floor(x/s), floor(((convPixel s h) - y)/s))

-- | getStatFigLoc Wall
--implementation of instance StaticFigurePos Wall
instance StaticFigurePos Wall where
  getStatFigLoc (BoardSize w h s) (Wall (Pt x y))            = (floor(x/s), floor(((convPixel s h) - y)/s))
  getStatFigLoc (BoardSize w h s) (Path (Pt x y))            = (floor(x/s), floor(((convPixel s h) - y)/s))
  getStatFigLoc (BoardSize w h s) (NotAllowedForPM (Pt x y)) = (floor(x/s), floor(((convPixel s h) - y)/s))
  getStatFigLoc (BoardSize w h s) (OutOfField (Pt x y))      = (floor(x/s), floor(((convPixel s h) - y)/s))
  getStatFigLoc (BoardSize w h s) (GhostStart (Pt x y))      = (floor(x/s), floor(((convPixel s h) - y)/s))
  getStatFigLoc (BoardSize w h s) (FreWall (Pt x y))      = (floor(x/s), floor(((convPixel s h) - y)/s))

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- type class to get the key interaction to control the game
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | Returns the state of the game, which was modified by a key event
class KeyHandler a where
  handleKeyEvent  :: Event       -- ^ keyevent, which should change something in the state of game
                  -> a           -- ^ object, who gets en keyevent
                  -> a           -- ^ object, which acted on the key event


-- | handleKeyEvent GameState
--implementation of instance KeyHandler GameState
instance KeyHandler GameState where
  handleKeyEvent (EventKey (Char 'h') Down _ _) (GameOver sc NoLoadedScores iG) = GameOver sc LoadScores iG
  handleKeyEvent (EventKey (Char 'r') Down _ _) (GameOver sc NoLoadedScores iG) = initialGameState iG
  handleKeyEvent (EventKey (Char 'r') Down _ _) (GameOver sc (LoadedScores _) iG) = initialGameState iG
  handleKeyEvent _ gO@(GameOver _ _ _) = gO
  handleKeyEvent (EventKey (Char 'p') Down _ _) (Game iG wo bo Pause ti) = Game iG wo bo Running ti
  handleKeyEvent (EventKey (Char 'r') Down _ _) (Game iG wo bo Pause ti) = initialGameState iG
  handleKeyEvent (EventKey (Char 'p') Down _ _) (Game iG wo bo Running ti) = Game iG wo bo Pause ti
  handleKeyEvent _ (Game iG wo bo Pause ti) = (Game iG wo bo Pause ti)
  handleKeyEvent e (Game iG wo (Board stBo (Dynamic pc gh )) Running ti) = Game iG wo (Board stBo (Dynamic (handleKeyEvent e pc) gh )) Running ti

-- | handleKeyEvent Pacman
--implementation of instance KeyHandler Pacman
instance KeyHandler Pacman where
  handleKeyEvent (EventKey (SpecialKey key) Down _ _) pc@(Player st an m vO pt t col) =
    case (key, st) of
      (KeyUp, FastMode)   -> Player st an m (VerticalDirection fixedVelocityFast) pt t col
      (KeyUp, _)          -> Player st an m (VerticalDirection fixedVelocityNormal) pt t col
      (KeyDown, FastMode) -> Player st an m (VerticalDirection ((-1)*fixedVelocityFast)) pt t col
      (KeyDown, _)        -> Player st an m (VerticalDirection ((-1)*fixedVelocityNormal)) pt t col
      (KeyLeft, FastMode) -> Player st an m (HorizontalDirection ((-1)*fixedVelocityFast)) pt t col
      (KeyLeft, _)        -> Player st an m (HorizontalDirection ((-1)*fixedVelocityNormal)) pt t col
      (KeyRight, FastMode)-> Player st an m (HorizontalDirection fixedVelocityFast) pt t col
      (KeyRight, _)       -> Player st an m (HorizontalDirection fixedVelocityNormal) pt t col
      (_, _)              -> pc
  handleKeyEvent _ pc = pc

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- type class to change world object data
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | Returns a object, whose value was increased or decreased
class ChangeScore a where
  changeFood        :: PacmanState  -- ^ current state of pacman
                    -> a            -- ^ object, which can be increased or decreased
                    -> a            -- ^ object, whose value has changed
  changePowerup     :: a            -- ^ object, which can be increased or decreased
                    -> a            -- ^ object, whose value has changed

-- | changeFood and changePowerup Score
--implementation of instance ChangeScore Score
instance ChangeScore Score where
  changeFood ReallyStuffedMode score = Score (ifGreaterThanZero(getScore score - floor(fromIntegral(increasingFoodFactor) / 2)))
  changeFood StuffedMode score = Score (getScore score + floor(fromIntegral(increasingFoodFactor) / 2))
  changeFood StarvingMode score = Score (getScore score + increasingFoodFactor * 2)
  changeFood _ score = Score (getScore score + increasingFoodFactor)

  changePowerup score = Score (getScore score + increasingChestFactor)

class ChangeTotalFood a where
  increaseTotalFood :: a           -- ^ object, which can be increased or decreased
                    -> a           -- ^ object, whose value has changed
  decreaseTotalFood :: a           -- ^ object, which can be increased or decreased
                    -> a           -- ^ object, whose value has changed

-- | increaseTotalFood and decreaseTotalFood TotalFood
--implementation of instance ChangeTotalFood TotalFood
instance ChangeTotalFood TotalFood where
  increaseTotalFood tf = TotalFood (getNoTotalFood tf + increasingFoodNo) -- Powerup which spawns new foods (idea for further work)
  decreaseTotalFood tf = TotalFood (getNoTotalFood tf - 1)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- type class to modify objects due to a collected Chest-object
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | Returns a object, whose object has been modified by the chest's content
class Modifier a where
  modify  :: PowerupState   -- ^ content of the collected Chest-object
          -> a              -- ^ object, on which the content of the chest has an impact
          -> a              -- ^ modified object

-- | modify Pacman
--implementation of instance Modifier Pacman
instance Modifier Pacman where
  modify MinusScore (Player _ an _ v pt _ col) = Player ReallyStuffedMode an Mortal (vel2Normal v) pt (SetTimer changeToNormal) col
  modify HalfScore (Player _ an _ v pt _ col) = Player StuffedMode an Mortal (vel2Normal v) pt (SetTimer changeToNormal) col
  modify DoubleScore (Player _ an _ v pt _ col) = Player StarvingMode an Mortal (vel2Normal v) pt (SetTimer changeToNormal) col
  modify Runner (Player _ an _ v pt _ col) = Player FastMode an Mortal (vel2Fast v) pt (SetTimer changeToNormal) col
  modify Killer (Player _ an _ v pt _ col) = Player KillerMode an Immortal (vel2Normal v) pt (SetTimer changeToNormal) col
  modify _ pacman = pacman

-- | modify Ghost
--implementation of instance Modifier Ghost
instance Modifier Ghost where
  modify Killer (FixedPatternOne _ a v p pts loc d _ col) = FixedPatternOne Mortal a v p pts loc d (SetTimer changeToNormal) col
  modify Killer (FollowTypeOne _ a v p loc d _ col) = FollowTypeOne Mortal a v p loc d (SetTimer changeToNormal) col
  modify Killer (FollowTypeTwo _ a v p loc d _ col) = FollowTypeTwo Mortal a v p loc d (SetTimer changeToNormal) col
  modify Killer (FollowTypeThree _ a v p loc d _ col) = FollowTypeThree Mortal a v p loc d (SetTimer changeToNormal) col
  modify _ (FixedPatternOne _ a v p pts loc d t col) = FixedPatternOne Immortal a v p pts loc d t col
  modify _ (FollowTypeOne _ a v p loc d t col) = FollowTypeOne Immortal a v p loc d t col
  modify _ (FollowTypeTwo _ a v p loc d t col) = FollowTypeTwo Immortal a v p loc d t col
  modify _ (FollowTypeThree _ a v p loc d t col) = FollowTypeThree Immortal a v p loc d t col

-- | modify WorldState
--implementation of instance Modifier WordlState
instance Modifier WorldState where
  modify Life (World sc li tf) = case li of
                                    OneLife   -> World sc TwoLives tf
                                    TwoLives  -> World sc ThreeLives tf
                                    ThreeLives-> World sc ThreeLives tf
                                    _         -> World sc li tf
  modify _ world = world

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Look-for-function to controll the GameState --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | looks for collision and stop those dynamic objects
lookForCol  :: GameState  -- ^ state of the game, which could have potential collisions
            -> GameState  -- ^ state of the game, in which all collisions are avoided
lookForCol gO@(GameOver _ _ _) = gO
lookForCol gs@(Game _ _ _ Pause _) = gs
lookForCol gs@(Game iG _ (Board (Static walls _ _) (Dynamic pacman ghosts)) Running ti) = case (proofPacCol bS walls pacman) of
                                                                                            KeepRunning -> gs
                                                                                            Stop -> stopPacman gs
  where
    bS = getInitBoardSize iG
    proofPacCol sB@(BoardSize w h s) ws p = case (getWallAtPos w ws (getDynFigLoc sB p)) of
                                              ((Path _), prec)  -> isItPreciseEnough prec (getPacmanState p)
                                              _ -> Stop

    getWallAtPos w ws ((xB, yB), precision) = (ws!!(w * yB + xB), precision)

    isItPreciseEnough pr FastMode | abs(pr) < 7.0 = KeepRunning
                                  | otherwise = Stop
    isItPreciseEnough pr _        | abs(pr) < 4.0 = KeepRunning
                                  | otherwise = Stop

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- |  looks for food item, pacman can collect
lookForFood :: GameState  -- ^ state of the game, in which pacman might hit a food item
            -> GameState  -- ^ state of the game, in which pacman has eaten it or there was no food item
lookForFood gO@(GameOver _ _ _) = gO
lookForFood gs@(Game _ _ _ Pause _) = gs
lookForFood gs@(Game iG wo@(World score life totFood) bo@(Board (Static ws foods pus) dy@(Dynamic pacman _ ) ) Running ti) =
  case (any (ckeckPlayerFoodPos bS pacman) foods) of
    False -> gs
    True  -> Game iG ( World (changeFood (getPacmanState pacman) score) life (decreaseTotalFood totFood) ) ( Board (Static ws (modFoodList eatenFood notEatenFoodList) pus) dy ) Running ti

  where
    bS = getInitBoardSize iG
    -- Get the eaten food-object and the rest of the food-list
    eatenFood = find (ckeckPlayerFoodPos bS pacman) foods
    notEatenFoodList = filter (not.(ckeckPlayerFoodPos bS pacman)) foods
    -- Create a updated food-list
    modFoodList Nothing notEatenFoodList = error "No eatenFood, which should be impossible! You should check this!"
    modFoodList (Just eatenFood) notEatenFoodList = (NoFd (getFLocation eatenFood) ) : notEatenFoodList
    -- Checks whether pacman hit a food-item or not
    ckeckPlayerFoodPos bs pc fd = (cmpPos (getStatFigLoc bs pc) (getStatFigLoc bs fd)) && (fd == (Fd (getFLocation fd)))

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | check if the timer of the dynamic objects reached 0.0
lookForTimeComp :: GameState  -- ^ state of the game, in which a Timer might finished and new global time was updated
                -> GameState  -- ^ state of the game, in which the Timer influenced its object or the gloabl time change the score
lookForTimeComp gO@(GameOver _ _ _) = gO
lookForTimeComp gs@(Game _ _ _ Pause _) = gs
lookForTimeComp (Game iG (World score life totFood) (Board sB (Dynamic pM ghs) ) Running ti) =
  (Game iG (World score life totFood) (Board sB (Dynamic (lookTimerPac pM) (map lookTimerGhost ghs) ) ) Running ti)

  where
    -- check pacman
    lookTimerPac pC   | (getPSetTimer pC) == NoTimer = Player NormalMode NoAnimation Mortal (vel2Normal (getPVelocity pC)) (getPLocation pC) (NoTimer) NotCollided
                      | otherwise = pC

    -- check ghost
    lookTimerGhost gh   | (getGSetTimer gh) == NoTimer = case gh of
                                                             (FixedPatternOne _ a v p pts loc d t Collided) -> FixedPatternOne Immortal a v p pts loc d t NotCollided
                                                             (FollowTypeOne _ a v p loc d t Collided) -> FollowTypeOne Immortal a v p loc d t NotCollided
                                                             (FollowTypeTwo _ a v p loc d t Collided) -> FollowTypeTwo Immortal a v p loc d t NotCollided
                                                             (FollowTypeThree _ a v p loc d t Collided) -> FollowTypeThree Immortal a v p loc d t NotCollided
                                                             (FixedPatternOne _ a v p pts loc d t col) -> FixedPatternOne Immortal a v p pts loc d t NotCollided
                                                             (FollowTypeOne _ a v p loc d t col) -> FollowTypeOne Immortal a v p loc d t NotCollided
                                                             (FollowTypeTwo _ a v p loc d t col) -> FollowTypeTwo Immortal a v p loc d t NotCollided
                                                             (FollowTypeThree _ a v p loc d t col) -> FollowTypeThree Immortal a v p loc d t NotCollided
                       | otherwise = gh

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | check if the game reached and GameOver condition
lookGameOverCond  :: GameState  -- ^ state of the game, in which the player might reached an game over
                  -> GameState  -- ^ state of the game, in which the timer either reached the game over or is allowed to continue to play
lookGameOverCond gO@(GameOver _ _ _) = gO
lookGameOverCond gs@(Game _ _ _ Pause _) = gs
lookGameOverCond gs@(Game iG (World score life totFood) (Board sB (Dynamic pM ghs) ) Running ti) =
  case  ((getNoTotalFood totFood), life) of
        (0, ZeroLives)  -> GameOver score NoLoadedScores iG
        (0, _)          -> GameOver score NoLoadedScores iG
        (_, ZeroLives)  -> GameOver score NoLoadedScores iG
        (_, _)          -> gs

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | looks for chest items, pacman can collect and if he collected one return a random Chest content
lookForChest  :: GameState  -- ^ state of the game, in which pacman might hit a chest item
              -> IO GameState  -- ^ state of the game, in which pacman has activated the chest item or there was no chest item
lookForChest gO@(GameOver _ _ _) = return gO
lookForChest gs@(Game _ _ _ Pause _) = return $ gs
lookForChest gs@(Game iG wo@(World score life totFood) bo@(Board (Static ws foods pus) dy@(Dynamic pacman ghosts) ) Running ti) =
  case (any (ckeckPlayerChestPos bS pacman) pus) of
    False -> return $ gs
    True  -> do randPU <- getRandPU
                return $ Game iG ( modify (randPU) (World (changePowerup score ) life totFood) ) (Board (Static ws foods (modPUList hitChest notHitChestList)) (Dynamic (modify (randPU) pacman) (map (modify (randPU)) ghosts) ) ) Running ti

  where
    bS = getInitBoardSize iG
    -- Get a random powerup-item
    getRandPU = do  randInt <- (randomRIO (0, (noOfPUS-1))) :: IO Int
                    let changePU = (foldr ( . ) id (replicate randInt succ))
                    return $ changePU Killer
    -- Get the hit chest and the rest of the powerup-list
    hitChest = find (ckeckPlayerChestPos bS pacman) pus
    notHitChestList = filter (not.(ckeckPlayerChestPos bS pacman)) pus
    -- Create a updated powerup-list
    modPUList Nothing chestList = error "No hitChest, which should be impossible! You should check this!"
    modPUList (Just hChest) chestList = (EmptyChest (getPULocation hChest) ) : chestList
    -- Checks whether pacman hit a chest or not
    ckeckPlayerChestPos bs pc p = (cmpPos (getStatFigLoc bs pc) (getStatFigLoc bs p)) && (p == (Chest (getPULocation p)))

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | looks for ghost collision with pacman

--------- Case for Pacman chasing Ghost ---------
lookForGhostCol :: GameState -- ^ state of the game, in which pacman may hit a ghost
                -> GameState -- ^ state of the game, in which pacman either hit the ghost or not
lookForGhostCol gs@(Game wo bo bs Pause _) = gs
lookForGhostCol gs@(Game inB@(InitBoard ii bs) wo@(World (Score sc) life tF) bo@(Board stc dy@(Dynamic pc@(Player KillerMode _ _ _ _ _ _) gh)) Running ti) = case (pacCollide pc gh bs) of
                                                                                                                                                         Just a  -> (Game inB (World (Score (sc+50)) life tF) (Board stc (Dynamic pc a)) Running ti)
                                                                                                                                                         Nothing -> gs
  where
    -- function which checks if any ghosts have collided with pacman, if so, return (Just ghosts), otherwise Nothing
    pacCollide :: Pacman -> [Ghost] -> BoardSize -> Maybe [Ghost]
    pacCollide pc xs bS = case (reverse (pacCollision pc xs bS 0)) of
                               (Nothing : gs)  -> Nothing
                               ((Just g) : gs) -> Just (filterNothing ((Just g):gs))

    -- function which checks how many and which ghosts collided with pacman, and returns a list of Maybe Ghosts, (Just newGhost) if collision, (Just oldGhost otherwise)
    pacCollision :: Pacman -> [Ghost] -> BoardSize -> Int -> [Maybe Ghost]
    pacCollision _ [] _ 0 = [Nothing]
    pacCollision _ [] _ _ = []
    pacCollision pc (x:xs) bS n = case (didCollide pc x bS) of
                                     Nothing -> ((Just x) : pacCollision pc xs bS n)
                                     Just a  -> ((Just a) : pacCollision pc xs bS (n+1))

    -- function which checks if a specific ghost collided with pacman by comparing their locations, and returns a non-moving ghost if collision is true
    didCollide :: Pacman -> Ghost -> BoardSize -> Maybe Ghost
    didCollide pc gh@(FixedPatternOne _ a v p pts loc d t NotCollided)  bS | cmpPos (getStatFigLoc bS pc) (getStatFigLoc bS gh) = Just ((FixedPatternOne Immortal a v p pts loc d (SetTimer changeToNormal) Collided))
                                                                           | otherwise = Nothing
    --
    didCollide pc gh@(FollowTypeOne _ a v p loc d t NotCollided)  bS | cmpPos (getStatFigLoc bS pc) (getStatFigLoc bS gh) = Just ((FollowTypeOne Immortal a v p loc d (SetTimer changeToNormal) Collided))
                                                                     | otherwise = Nothing
    --
    didCollide pc gh@(FollowTypeTwo _ a v p loc d t NotCollided)  bS | cmpPos (getStatFigLoc bS pc) (getStatFigLoc bS gh) = Just ((FollowTypeTwo Immortal a v p loc d (SetTimer changeToNormal) Collided))
                                                                     | otherwise = Nothing
    --
    didCollide pc gh@(FollowTypeThree _ a v p loc d t NotCollided)  bS | cmpPos (getStatFigLoc bS pc) (getStatFigLoc bS gh) = Just ((FollowTypeThree Immortal a v p loc d (SetTimer changeToNormal) Collided))
                                                                       | otherwise = Nothing
    didCollide _ _ _ = Nothing
    --
--------- Case for Ghost chasing Pacman ---------
lookForGhostCol gs@(Game inB@(InitBoard ii bs) wo@(World sc life tf) bo@(Board stc dy@(Dynamic pc@(Player _ _ Mortal _ _ _ NotCollided) gh)) Running ti) = case (ghoCollision pc gh bs) of
                                                                                                                                     Just a  -> case life of
                                                                                                                                                     ZeroLives  -> (Game inB (World sc ZeroLives tf) (Board stc (Dynamic a gh)) Running ti)
                                                                                                                                                     OneLife    -> (Game inB (World sc ZeroLives tf) (Board stc (Dynamic a gh)) Running ti)
                                                                                                                                                     TwoLives   -> (Game inB (World sc OneLife tf) (Board stc (Dynamic a gh)) Running ti)
                                                                                                                                                     ThreeLives -> (Game inB (World sc TwoLives tf) (Board stc (Dynamic a gh)) Running ti)
                                                                                                                                     Nothing -> gs
  where
    -- function which checks the list of ghosts to see which if any ghosts collided with pacman, returns (Just newPacman) if any of them match, Notjing otherwise
    ghoCollision :: Pacman -> [Ghost] -> BoardSize -> Maybe Pacman
    ghoCollision _ [] _ = Nothing
    ghoCollision pc (x:xs) bS = case (didCollide pc x bS) of
                                     Just a  -> Just a
                                     Nothing -> ghoCollision pc xs bS
    -- function which checks if a specific ghost collided with pacman, returns an Just immortal pacman with no velocity if true, Nothing otherwise
    didCollide :: Pacman -> Ghost -> BoardSize -> Maybe Pacman
    didCollide pc@(Player st an m vel pt t col) gh bS | cmpPos (getStatFigLoc bS pc) (getStatFigLoc bS gh) = case (getGCollision gh) of
                                                                                                                                       NotCollided -> Just (Player NormalMode (PacmanAnimation 1 (SetTimer changeAnPacman)) Immortal vel pt (SetTimer changeShort) Collided)
                                                                                                                                       Collided    -> Nothing
                                                   | otherwise = Nothing
lookForGhostCol gs = gs

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | load the highscore and store it, if the score is high enough and GamOver is reached
loadHighscore :: IO GameState  -- ^ state of the game, in which the game is still going or the player is waiting for the highscore list
              -> IO GameState  -- ^ state of the game, in which the game is still going or the highscore list was loaded and is shown
loadHighscore ioGS =  do  gS <- ioGS
                          case gS of
                            (Game _ _ _ _ _)                -> return gS
                            (GameOver sc LoadScores iG)     -> loadHList sc iG
                            (GameOver _ _ _)                -> return gS

  where
    loadHList :: Score -> InitBoard -> IO GameState
    loadHList s@(Score score) iGB = do  scoreHandle <- openFile (getUnivFilePath getPathToHiSc) ReadMode
                                        (tempName, tempHandle) <- openTempFile "." "temp"
                                        contents <- hGetContents scoreHandle
                                        let listScores = tidyScores score contents
                                            scores = unlines listScores
                                            scoreInts = map read listScores

                                        -- Write the scores to file
                                        hPutStr tempHandle $ scores

                                        -- Close and rename files
                                        hClose scoreHandle
                                        hClose tempHandle
                                        removeFile (getUnivFilePath getPathToHiSc)
                                        renameFile tempName (getUnivFilePath getPathToHiSc)

                                        -- Return the GameOver state which the list of highscores
                                        return (GameOver s (LoadedScores ( (map Score) scoreInts) ) iGB)



-- Functions to control ghost movement
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | function to oversee the movement of the ghosts, depending on different goals per ghost
getMovement :: GameState -- ^ the gamestate to be changed
            -> GameState -- ^ the changed gamestate by updating ghost movement
getMovement gO@(GameOver _ _ _) = gO
getMovement gS@(Game _ _ _ Pause _) = gS
getMovement gS@(Game iG wo (Board stBo@(Static ws fd pw) (Dynamic pc gh )) Running ti) = Game iG wo (Board stBo (Dynamic pc (map (moveGhost pc bS ws pw) gh )) ) Running ti

  where
    bS = getInitBoardSize iG

-- | function to determine the ghost movement
moveGhost :: Pacman    -- ^ pacman
          -> BoardSize -- ^ the boardsize
          -> [Wall]    -- ^ the board
          -> [Powerup] -- ^ list of powerups
          -> Ghost     -- ^ ghost to change
          -> Ghost     -- ^ the modified ghost
-- Calls functions: "getGoal" & "setGhostDestination"
moveGhost pac bS ws pw gh@(FixedPatternOne m an v p pts@(x:xs) loc d@(NoDestiny dst) t col) = setGhostDirection bS (getGhostGoal (FixedPatternOne m an v p (xs ++ [x]) OnField d t col) ws pw bS pac) ws (FixedPatternOne m an v p (xs ++ [x]) OnField (Destiny dst) t col)
moveGhost pac bS ws pw gh@(FixedPatternOne m an v p pts loc (Destiny dst) t col) = setGhostDirection bS (getGhostGoal gh ws pw bS pac) ws (FixedPatternOne m an v p pts loc (Destiny dst) t col)
moveGhost pac bS ws pw gh@(FollowTypeOne m an v p loc d@(NoDestiny dst) t col) = setGhostDirection bS (getGhostGoal (FollowTypeOne m an v p OnField d t col) ws pw bS pac) ws (FollowTypeOne m an v p OnField (Destiny dst) t col)
moveGhost pac bS ws pw gh@(FollowTypeOne m an v p loc d@(Destiny dst) t col) = setGhostDirection bS (getGhostGoal gh ws pw bS pac) ws (FollowTypeOne m an v p loc d t col)
moveGhost pac bS ws pw gh@(FollowTypeTwo m an v p loc d@(NoDestiny dst) t col) = setGhostDirection bS (getGhostGoal (FollowTypeTwo m an v p OnField d t col) ws pw bS pac) ws (FollowTypeTwo m an v p OnField (Destiny dst) t col)
moveGhost pac bS ws pw gh@(FollowTypeTwo m an v p loc d@(Destiny dst) t col) = setGhostDirection bS (getGhostGoal gh ws pw bS pac) ws (FollowTypeTwo m an v p loc d t col)
moveGhost pac bS ws pw gh@(FollowTypeThree m an v p loc d@(NoDestiny dst) t col) = setGhostDirection bS (getGhostGoal (FollowTypeThree m an v p OnField d t col) ws pw bS pac) ws (FollowTypeThree m an v p OnField (Destiny dst) t col)
moveGhost pac bS ws pw gh@(FollowTypeThree m an v p loc d@(Destiny dst) t col) = setGhostDirection bS (getGhostGoal gh ws pw bS pac) ws (FollowTypeThree m an v p loc d t col)
-- make the rest

-- | function to determine what the goal of a ghost is
getGhostGoal :: Ghost     -- ^ the ghost to determine the goal for
             -> [Wall]    -- ^ the board
             -> [Powerup] -- ^ list of powerups
             -> BoardSize -- ^ the boardsize
             -> Pacman    -- ^ pacman
             -> Wall      -- ^ the goal for the ghost
getGhostGoal gh _ pw _ pac@(Player _ _ Immortal _ p _ _) = Path (furthestFromPM p (map getPULocation pw))
getGhostGoal (FixedPatternOne Immortal an v p pts@(x:xs) loc dst t col) ws pw bS pac = case loc of
                                                                                     OnField -> (Path x)
                                                                                     InBase  -> wallGhostStart
                                                                                     Stand   -> getGhostGoal (FixedPatternOne Immortal an v p pts InBase dst t col) ws pw bS pac
  where
    wallGhostStart = case (find wallToBool ws) of
                          (Just w) -> w
                          Nothing  -> error "No startpoint found"
getGhostGoal (FollowTypeOne Immortal an v gp loc dst t col) ws pw bS pac@(Player _ _ _ _ p _ _) = case loc of
                                                                                          OnField -> (Path p)
                                                                                          InBase  -> wallGhostStart
                                                                                          Stand   -> getGhostGoal (FollowTypeOne Immortal an v gp InBase dst t col) ws pw bS pac
  where
    wallGhostStart = case (find wallToBool ws) of
                          (Just w) -> w
                          Nothing  -> error "No startpoint found"
getGhostGoal (FollowTypeTwo Immortal an v gp loc dst t col) ws pw bS@(BoardSize w h s) pac@(Player _ _ _ pv p _ _) = case loc of
                                                                                                           OnField -> lookAheadPacman pac 4 bS ws
                                                                                                           InBase  -> wallGhostStart
                                                                                                           Stand   -> getGhostGoal (FollowTypeTwo Immortal an v gp InBase dst t col) ws pw bS pac
  where
    wallGhostStart = case (find wallToBool ws) of
                          (Just w) -> w
                          Nothing  -> error "No startpoint found"
getGhostGoal (FollowTypeThree Immortal an v gp loc dst t col) ws pw bS@(BoardSize w h s) pac@(Player _ _ _ pv p _ _) = case loc of
                                                                                                             OnField -> lookAheadPacman pac (-4) bS ws
                                                                                                             InBase  -> wallGhostStart
                                                                                                             Stand   -> getGhostGoal (FollowTypeThree Immortal an v gp InBase dst t col) ws pw bS pac
  where
    wallGhostStart = case (find wallToBool ws) of
                          (Just w) -> w
                          Nothing  -> error "No startpoint found"
getGhostGoal gh _ pw _ pc = Path (furthestFromPM (getPLocation pc) (map getPULocation pw))

-- | Function that when given a point and a list, returns the element of the list furthest from the point (intended for ghost retreat from pacman)
furthestFromPM :: Pt -> [Pt] -> Pt
furthestFromPM _ (p : []) = p
furthestFromPM g (p:q:[]) | (findPtDistance p g) > (findPtDistance q g) = p
                          | otherwise = q
furthestFromPM g (p:q:ps) | (findPtDistance p g) > (findPtDistance q g) = furthestFromPM g (p:ps)
                          | otherwise = furthestFromPM g (q:ps)

-- | function to look ahead/behind pacman, direction determined by the pos/neg nature of 'n'
lookAheadPacman :: Pacman
                -> Int
                -> BoardSize
                -> [Wall]
                -> Wall
lookAheadPacman p@(Player _ _ _ v pt@(Pt px py) _ _) n bS@(BoardSize w h s) ws = case v of
                                                                               (HorizontalDirection vx) -> (horPacSearch ((signum (floor vx)) * n) (signum (floor vx)) pacLoc w ws)
                                                                               (VerticalDirection vy)   -> (verPacSearch ((signum (floor vy)) * n) (signum (floor vy)) pacLoc w ws)
  where
    horPacSearch :: Int -> Int -> PositionInBlocks -> Int -> [Wall] -> Wall
    horPacSearch 0 _ pt w ws = (getWallOnPos w pt ws)
    horPacSearch n i pt@(pacX, pacY) w ws | (((pacX + n) + w * pacY) >= length ws) || (((pacX + n) + w * pacY) < 0) = (getWallOnPos w pt ws) --horPacSearch (n+i) i (pacX, pacY) w ws
                                          | otherwise = case (getWallOnPos w ((pacX+n), pacY) ws) of
                                                          Path a -> Path a
                                                          _      -> horPacSearch (n+i) i (pacX, pacY) w ws
    verPacSearch :: Int -> Int -> PositionInBlocks -> Int -> [Wall] -> Wall
    verPacSearch 0 _ pt w ws = (getWallOnPos w pt ws)
    verPacSearch n i pt@(pacX, pacY) w ws | ((pacX * w * (n + pacY)) >= length ws) || ((pacX * w * (n + pacY)) < 0) = (getWallOnPos w pt ws) --verPacSearch (n+i) i (pacX, pacY) w ws
                                          | otherwise = case (getWallOnPos w (pacX, (pacY+n)) ws) of
                                                            Path a -> Path a
                                                            _      -> verPacSearch (n+i) i (pacX, pacY) w ws
    pacLoc@(pcX, pcY) = getStatFigLoc bS p

-- | function which checks if the ghost is at centre of your destiny; if it is, get new destiny (nextBlockforghost) this destiny changes the value of your ghost direction
setGhostDirection :: BoardSize    -- ^ the boardsize
                  -> Wall         -- ^ the goal for the ghost
                  -> [Wall]       -- ^ the board
                  -> Ghost        -- ^ the ghost to determine direction for
                  -> Ghost        -- ^ output ghost heading in correct direction
setGhostDirection bS goal ws ghost | dist > (fixedTimestep * fixedVelocityGhost) = ghost
                                   | otherwise = setNewDestiny ghost bS goal ws

  where
    dist = sqrt( (getX (getGLocation ghost) -  getX(getDestinyPt(getNextTarget ghost) ) )^2 + (getY (getGLocation ghost) -  getY(getDestinyPt(getNextTarget ghost) ) )^2 )

setNewDestiny :: Ghost        -- ^ the ghost to change velocity and nextTarget
              -> BoardSize    -- ^ the boardsize
              -> Wall         -- ^ the goal
              -> [Wall]       -- ^ the board
              -> Ghost        -- ^ the new ghost type
setNewDestiny g@(FixedPatternOne m an v p pts loc target@(Destiny d) t col) bS goal ws = (FixedPatternOne m an newVel newPt pts loc newDest t col)
  where
    newVel = case ((getDestinyPt target) == (getPt goal)) of
                  True -> (HorizontalDirection 0.0)
                  False -> (setNewV newDest oldDestPt)
    newPt = case ((getDestinyPt target) == (getPt goal)) of
                  True -> p
                  False -> oldDestPt
    newDest = case ((getDestinyPt target) == (getPt goal)) of
                  True -> NoDestiny d
                  False -> (nextBlockforGhost (getStatFigLoc bS g) loc v goal bS ws)
    oldDestPt = (getDestinyPt target)
    setNewV :: NextTarget -> Pt -> Velocity
    setNewV (Destiny (Pt dx dy)) (Pt x y) | (dy == y) && (dx == x) = (HorizontalDirection 0.0)
                                          | dy == y = (HorizontalDirection ((signum (dx-x))*fixedVelocityGhost))
                                          | dx == x = (VerticalDirection ((signum (dy-y))*fixedVelocityGhost))
                                          | otherwise = (HorizontalDirection 0.0)
setNewDestiny g@(FollowTypeOne m an v p loc target@(Destiny d) t col) bS goal ws = (FollowTypeOne m an newVel newPt loc newDest t col)
  where
    newVel = case ((getDestinyPt target) == (getPt goal)) of
                  True -> (HorizontalDirection 0.0)
                  False -> (setNewV newDest oldDestPt)
    newPt = case ((getDestinyPt target) == (getPt goal)) of
                  True -> p
                  False -> oldDestPt
    newDest = case ((getDestinyPt target) == (getPt goal)) of
                  True -> NoDestiny d
                  False -> (nextBlockforGhost (getStatFigLoc bS g) loc v goal bS ws)
    oldDestPt = (getDestinyPt target)
    setNewV :: NextTarget -> Pt -> Velocity
    setNewV (Destiny (Pt dx dy)) (Pt x y) | (dy == y) && (dx == x) = (HorizontalDirection 0.0)
                                          | dy == y = (HorizontalDirection ((signum (dx-x))*fixedVelocityGhost))
                                          | dx == x = (VerticalDirection ((signum (dy-y))*fixedVelocityGhost))
                                          | otherwise = (HorizontalDirection 0.0)
setNewDestiny g@(FollowTypeTwo m an v p loc target@(Destiny d) t col) bS goal ws = (FollowTypeTwo m an newVel newPt loc newDest t col)
  where
    newVel = case ((getDestinyPt target) == (getPt goal)) of
                  True -> (HorizontalDirection 0.0)
                  False -> (setNewV newDest oldDestPt)
    newPt = case ((getDestinyPt target) == (getPt goal)) of
                  True -> p
                  False -> oldDestPt
    newDest = case ((getDestinyPt target) == (getPt goal)) of
                  True -> NoDestiny d
                  False -> (nextBlockforGhost (getStatFigLoc bS g) loc v goal bS ws)
    oldDestPt = (getDestinyPt target)
    setNewV :: NextTarget -> Pt -> Velocity
    setNewV (Destiny (Pt dx dy)) (Pt x y) | (dy == y) && (dx == x) = (HorizontalDirection 0.0)
                                          | dy == y = (HorizontalDirection ((signum (dx-x))*fixedVelocityGhost))
                                          | dx == x = (VerticalDirection ((signum (dy-y))*fixedVelocityGhost))
                                          | otherwise = (HorizontalDirection 0.0)
setNewDestiny g@(FollowTypeThree m an v p loc target@(Destiny d) t col) bS goal ws = (FollowTypeThree m an newVel newPt loc newDest t col)
  where
    newVel = case ((getDestinyPt target) == (getPt goal)) of
                  True -> (HorizontalDirection 0.0)
                  False -> (setNewV newDest oldDestPt)
    newPt = case ((getDestinyPt target) == (getPt goal)) of
                  True -> p
                  False -> oldDestPt
    newDest = case ((getDestinyPt target) == (getPt goal)) of
                  True -> NoDestiny d
                  False -> (nextBlockforGhost (getStatFigLoc bS g) loc v goal bS ws)
    oldDestPt = (getDestinyPt target)
    setNewV :: NextTarget -> Pt -> Velocity
    setNewV (Destiny (Pt dx dy)) (Pt x y) | (dy == y) && (dx == x) = (HorizontalDirection 0.0)
                                          | dy == y = (HorizontalDirection ((signum (dx-x))*fixedVelocityGhost))
                                          | dx == x = (VerticalDirection ((signum (dy-y))*fixedVelocityGhost))
                                          | otherwise = (HorizontalDirection 0.0)

-- | function to determine which is the next block for a ghost (which of the options is closest to goal)
nextBlockforGhost :: PositionInBlocks -- ^ Block location of the ghost
                  -> WhereIsGhost     -- ^ whether ghost is InBase or OnField
                  -> Velocity         -- ^ Velocity
                  -> Wall             -- ^ The Goal wall to go to
                  -> BoardSize        -- ^ the boardsize
                  -> [Wall]           -- ^ The board
                  -> NextTarget       -- ^ The next wall for the ghost to go to, in position blocks
nextBlockforGhost block loc vel goal bS@(BoardSize w h s) ws = Destiny (Pt (((fromIntegral x) + 0.5)*s) ((convPixel s h) - ((0.5+(fromIntegral y))*s)))
  where
    (x, y) = closestToGoal (getStatFigLoc bS goal) paths
    paths = map (getStatFigLoc bS) wallPaths
    wallPaths = getSurrPaths block loc vel bS ws

-- | Function to take a block and return the surrounding blocks
getSurrPaths :: PositionInBlocks -- ^ Block location of the ghost
             -> WhereIsGhost     -- ^ whether ghost is InBase or OnField
             -> Velocity         -- ^ Velocity of the ghost
             -> BoardSize        -- ^ The BoardSize
             -> [Wall]           -- ^ The board
             -> [Wall]           -- ^ The blocks around it that are paths
getSurrPaths (x,y) loc vel bS@(BoardSize w h s) ws = case vel of
                                                          (HorizontalDirection vx) -> case (isPositive vx) of
                                                                                           True  -> filterNothing [rightMaybe, upMaybe, downMaybe]
                                                                                           False -> filterNothing [leftMaybe, upMaybe, downMaybe]
                                                          (VerticalDirection vy)   -> case (isPositive vy) of
                                                                                           True  -> filterNothing [rightMaybe, leftMaybe, upMaybe]
                                                                                           False -> filterNothing [rightMaybe, leftMaybe, downMaybe]
  where
    isPositive v = v >= 0
    -- surrounding blocks (around the current ghost location), Just a if they are paths, Nothing otherwise
    leftMaybe  = pathOrNot (getWallOnPos w ((x-1), y) ws) loc
    rightMaybe = pathOrNot (getWallOnPos w ((x+1), y) ws) loc
    upMaybe    = pathOrNot (getWallOnPos w (x, (y-1)) ws) loc
    downMaybe  = pathOrNot (getWallOnPos w (x, (y+1)) ws) loc

-- | Takes a Wall, and returns Just Wall if it is a Path, otherwise it returns Nothing
pathOrNot :: Wall         -- ^ Wall to check if it is a Path or not
          -> WhereIsGhost -- ^ Whether the ghost is OnField or InBase
          -> Maybe Wall   -- ^ Just Wall if it is a Wall, Nothing otherwise
pathOrNot p@(Path _) _ = Just p
pathOrNot p@(NotAllowedForPM _) loc = case loc of
                                       InBase  -> Just p
                                       Stand   -> Just p
                                       OnField -> Nothing
pathOrNot p@(GhostStart _) loc = case loc of
                                      InBase  -> Just p
                                      Stand   -> Just p
                                      OnField -> Nothing
pathOrNot _ _ = Nothing

-- | Takes a list of Maybe a's and filters out all of the Nothings from that list
filterNothing :: [Maybe a] -- ^ Inputs, list of maybe a's
              -> [a]       -- ^ The return
filterNothing []            = []
filterNothing (Nothing:xs)  = filterNothing xs
filterNothing ((Just x):xs) = x : filterNothing xs

-- | Function to determine which block of a list of blocks is closest to a goal block
closestToGoal :: PositionInBlocks   -- ^ The goal block
              -> [PositionInBlocks] -- ^ List of options (determine which of these is closest to goal)
              -> PositionInBlocks   -- ^ The block closest to goal block
closestToGoal _ (p : []) = p
closestToGoal g (p:q:[]) | (findDistance p g) < (findDistance q g) = p
                           | otherwise                             = q
closestToGoal g (p:q:ps) | (findDistance p g) < (findDistance q g) = closestToGoal g (p:ps)
                         | otherwise                               = closestToGoal g (q:ps)

-- | Function to determine distance between two points
findDistance :: PositionInBlocks -- ^ A tuple of two Ints which represent the test block location
             -> PositionInBlocks -- ^ A tuple of two Ints which represent the goal block location
             -> Float            -- ^ The distance between these points
findDistance (x1, y1) (x2, y2) = sqrt (xSq + ySq)
  where
    xSq = xs * xs
    ySq = ys * ys
    xs  = (fromIntegral x2) - (fromIntegral x1)
    ys  = (fromIntegral y2) - (fromIntegral y1)

findPtDistance :: Pt    -- ^ Pt to represent point of object
               -> Pt    -- ^ Pt to represent point of object
               -> Float -- ^ The distance between these points
findPtDistance (Pt x1 y1) (Pt x2 y2) = sqrt (xSq + ySq)
  where
    xSq = xs * xs
    ySq = ys * ys
    xs  = x2 - x1
    ys  = y2 - y1

-- Wall-to-bool-check-function
wallToBool :: Wall -- ^ wall-object
           -> Bool -- ^ Return, whether it is GhostStart or not
wallToBool (GhostStart _) = True
wallToBool _              = False
