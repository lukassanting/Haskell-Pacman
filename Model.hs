module Model where

--------------------------------------------------------------------------------
-- fixed values

-- | FilePath to level 1
getPathToLvl1 :: [String]
getPathToLvl1 = ["Level","level_1.txt"]

-- | FilePath to level 2
getPathToLvl2 :: [String]
getPathToLvl2 = ["Level","level_2.txt"]

-- | FilePath to level 3
getPathToLvl3 :: [String]
getPathToLvl3 = ["Level","level_3.txt"]

-- | FilePath to level for Fre
getPathToLvlFre :: [String]
getPathToLvlFre = ["Level","level_fre.txt"]

-- | FilePath to the highscore-list
getPathToHiSc :: [String]
getPathToHiSc = ["Highscore","highscore.txt"]

-- |inital higscore-list
initHighscore :: String
initHighscore = "0 0 0 0 0"

-- | fixed sqauresize of the board-items
fixedSquareSize :: Float
fixedSquareSize = 25.0

-- | fixed timestep in seconds
fixedTimestep :: Float
fixedTimestep = 0.02

-- | fixed velocity for dynmaic objects moving normally
fixedVelocityNormal :: Float
fixedVelocityNormal = 75.0
-- | fixed velocity for dynamic objects moving faster
fixedVelocityFast :: Float
fixedVelocityFast = 2.0 * fixedVelocityNormal
-- | fixed velocity of the ghost, so that it is easier
fixedVelocityGhost :: Float
fixedVelocityGhost = fixedVelocityNormal - 12.5

-- | fixed time in seconds, in which the Ghost remains in a animation state frame
changeAnGhosts  ::  Float
changeAnGhosts = 1.0
-- | number of frames used for the ghost animation
noFramesGhost :: Frame
noFramesGhost = 4

-- | fixed time in seconds, in whihc Pacman in an animation state frame
changeAnPacman :: Float
changeAnPacman = 0.2
-- | fixed time in seconds, in which Pacman remains in an animation state frame
noFramesPacman :: Frame
noFramesPacman = 2

-- | fixed time in seconds, in which pacman remains in a special form after collecting a chest
changeToNormal  ::  Float
changeToNormal = 10.0
-- | fixed time in seconds for which an agent remains 'dead'
changeShort :: Float
changeShort = 5.0


-- | Increase of the score after eating a food object
increasingFoodFactor :: Int
increasingFoodFactor = 2

-- | Increase of the score after eating a food object
increasingChestFactor :: Int
increasingChestFactor = 5

-- | The Increase of the numbers of food-objects
increasingFoodNo  :: Int
increasingFoodNo = 10

-- | number of different powerups
noOfPUS   :: Int
noOfPUS = length [Killer ..]


-- | Number of frames to show per second.
fps :: Int  -- ^ frames/second
fps = 60


-- aliases and data types

-- | number, in which animation state the object is
type Frame = Int
-- | row of the board
type Row = [Wall]
-- | column of the board
type Column = [Wall]
-- | board described as a list of rows
type BoardInRows = [Row]
-- | board described as a list of colum
type BoardInCols = [Column]
-- | tupel, which describes a 2D-position in blocks
type PositionInBlocks = (Int, Int)
-- | tupel of two tupel, which represents the block position of the heading edge and the distance from the middle point of the pacman to the middle of the path
type EdgeAndDistDiff = (PositionInBlocks, Float)


-- | A data structure, which represents a point in 2D
data Pt = Pt
  { getX::Float -- ^ x-value of Point
  , getY::Float -- ^ y-value of Point
  }
        | PtBlocks
  { getXBlock::Int   -- ^ x-(int)-value of Point
  , getYBlock::Int   -- ^ y-(int)-value of Point
  }
  deriving (Show, Eq)

-- | A data structure, which represents a number between 0 and 9 or ? if out of range
data No = QuestionMark
        | Zero
        | One
        | Two
        | Three
        | Four
        | Five
        | Six
        | Seven
        | Eight
        | Nine
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | A data structure, which represents a Int in decimal-format and can expressed through No-objects
data DecInt = DecInt
  { getHundreds::No     -- ^ represents the 10 to the power of 2
  , getTens::No         -- ^ represents the 10 to the power of 1
  , getOnes::No         -- ^ represents the 10 to the power of 0
  }
  deriving (Eq, Ord, Show)

-- | A data structure, which represents the board in initial state (binary)
data InitBoard = InitBoard
  { binBoard::[Int]             -- ^ binary entry of the board in a list.
  , getInitBoardSize::BoardSize -- ^ represents the size information of the board
  } deriving(Show)


-- Container objects -------------------------

-- | A data sructure, which represents the state of the game
data GameState = Game
  { getInitBoard::InitBoard   -- ^ the initial Board
  , getWorldState::WorldState -- ^ represents the state of the world
  , getBoardState::BoardState -- ^ represents the state of the board
  , getTimeOutMode::TimeOut   -- ^ shows, if the game is on pause or not
  , getCurrentTimer::Timer    -- ^ shows the time passed
  }
               | GameOver
  { getEndScore::Score        -- ^ reresents the reached score
  , getHighscore::Highscore   -- ^ represnts the loaded scores
  , getInitBoard::InitBoard   -- ^ the initial Board
  }
  deriving(Show)

------------------
--- World data ---

-- | A data structure, which represents the state of the world and gloabl variables
data WorldState = World
  { getScoreObj::Score          -- ^ represents the current score
  , getLife::Life               -- ^ represents the current number of lives
  , getTotalFoodObj::TotalFood  -- ^ represents the current number of available foods
  } deriving(Show)

-- | A data structure, which represents the current score
data Score = Score
  { getScore::Int       -- ^ Current Score
  }
  deriving(Show, Eq, Ord)

-- | A data structure, which represents the current number of food-items
data TotalFood = TotalFood { getNoTotalFood::Int }
  deriving(Show, Eq, Ord)

-- | A data structure, which states the number of lives left
data Life = ZeroLives
          | OneLife
          | TwoLives
          | ThreeLives
  deriving(Show, Eq, Ord)


------------------
--- Board data ---

-- | A data structure, which represents the state of the board
data BoardState = Board
  { getStaticBoard::StaticBoard   -- ^ represents the static part of the board
  , getDynamicBoard::DynamicBoard -- ^ represents the dynamic part of the board
  } deriving(Show)


-- Static objects --

-- | A data structure, which represents the static parts of the board
data StaticBoard = Static
  { getWalls::[Wall]        -- ^ represents the walls and no-walls of the board
  , getFood::[Food]         -- ^ represents the food-objects
  , getPowerups::[Powerup]  -- ^ represents the powerups
  } deriving(Show)

-- | A data structure, which represents a wall on the board
data Wall = Wall
  { getPt::Pt}
          | Path
  { getPt::Pt}
          | NotAllowedForPM
  { getPt::Pt}
          | OutOfField
  { getPt::Pt}
          | GhostStart
  { getPt::Pt}
          | FreWall
  { getPt::Pt}
  deriving (Show, Eq)

-- | A data structure, which represents the food-objects
data Food = NoFd {getFLocation::Pt}
          | Fd {getFLocation::Pt}
  deriving(Show, Eq)

-- | A data structure, which represents the powerup-objects
data Powerup  = EmptyChest
  {
    getPULocation::Pt             -- ^ represents the position of the EmptyChest
  }
              | Chest
  {
    getPULocation::Pt             -- ^ represents the position of the Chest
  }
  deriving (Show, Eq)

-- | A data structure, which represents the kind of powerup
data PowerupState = Killer
                  | Runner
                  | Life
                  | MinusScore
                  | HalfScore
                  | DoubleScore
  deriving(Show, Eq, Ord, Enum)


-- Dynamic objects --

-- | A data structure, which represents the dynamic parts of the board
data DynamicBoard = Dynamic
  { getPacman::Pacman   -- ^ represents the pacman object
  , getGhosts::[Ghost]  -- ^ represents the ghost-objects
  } deriving(Show)

-- | A data structure, which represents the pacman object
data Pacman = Player
  { getPacmanState::PacmanState -- ^ represents the state of pacman
  , getPFrame::PacmanAnimation  -- ^ represents the current frame of pacman animation
  , getPMortality::Mortality    -- ^ represents whether pacman is mortal or not
  , getPVelocity::Velocity      -- ^ represents the velocity and direction of pacman
  , getPLocation::Pt         -- ^ represents pacman's location
  , getPSetTimer::SetTimer     -- ^ represents the countdown to set the object into its usual state
  , getPCollision::Collision     -- ^ represents whether pacman is waiting or not
  } deriving(Show)

-- | A data structure, which represents the state of pacman
data PacmanState  = ReallyStuffedMode
                  | StuffedMode
                  | StarvingMode
                  | FastMode
                  | KillerMode
                  | NormalMode
  deriving(Show, Eq)

-- | A data structure, which represents a Ghost
data Ghost = FixedPatternOne
  { getGMortality::Mortality    -- ^ represents whether the ghost is mortal or not
  , getGFrame::GhostAnimation   -- ^ represents the current frame of the ghost animation
  , getGVelocity::Velocity      -- ^ represents the velocity and direction of the ghost
  , getGLocation::Pt            -- ^ represents the ghost's location
  , getTargetList::[Pt]         -- ^ represents the list of targets for the fixed ghost
  , getWhereGhost::WhereIsGhost -- ^ represents whether the ghost is in base or not
  , getNextTarget::NextTarget   -- ^ represents the pt location of the next target
  , getGSetTimer::SetTimer      -- ^ represents the countdown to set the object into its usual state
  , getGCollision::Collision     -- ^ represents whether the ghost is waiting or not
  }
            |FollowTypeOne
  { getGMortality::Mortality    -- ^ represents whether the ghost is mortal or not
  , getGFrame::GhostAnimation   -- ^ represents the current frame of the ghost animation
  , getGVelocity::Velocity      -- ^ represents the velocity and direction of the ghost
  , getGLocation::Pt            -- ^ represents the ghost's location
  , getWhereGhost::WhereIsGhost -- ^ represensts whether the ghost is in base or not
  , getNextTarget::NextTarget   -- ^ represents the pt location of the next target
  , getGSetTimer::SetTimer      -- ^ represents the countdown to set the object into its usual state
  , getGCollision::Collision     -- ^ represents whether the ghost is waiting or not
  }
            |FollowTypeTwo
  { getGMortality::Mortality    -- ^ represents whether the ghost is mortal or not
  , getGFrame::GhostAnimation   -- ^ represents the current frame of the ghost animation
  , getGVelocity::Velocity      -- ^ represents the velocity and direction of the ghost
  , getGLocation::Pt            -- ^ represents the ghost's location
  , getWhereGhost::WhereIsGhost -- ^ represensts whether the ghost is in base or not
  , getNextTarget::NextTarget   -- ^ represents the pt location of the next target
  , getGSetTimer::SetTimer      -- ^ represents the countdown to set the object into its usual state
  , getGCollision::Collision     -- ^ represents whether the ghost is waiting or not
  }
            |FollowTypeThree
  { getGMortality::Mortality    -- ^ represents whether the ghost is mortal or not
  , getGFrame::GhostAnimation   -- ^ represents the current frame of the ghost animation
  , getGVelocity::Velocity      -- ^ represents the velocity and direction of the ghost
  , getGLocation::Pt            -- ^ represents the ghost's location
  , getWhereGhost::WhereIsGhost -- ^ represensts whether the ghost is in base or not
  , getNextTarget::NextTarget   -- ^ represents the pt location of the next target
  , getGSetTimer::SetTimer      -- ^ represents the countdown to set the object into its usual state
  , getGCollision::Collision     -- ^ represents whether the ghost is waiting or not
  }
  deriving(Show)

-- | A data structure, which represents the animation state of the ghosts
data GhostAnimation = GhostAnimation
  { getCurrGFrameNo::Frame  -- ^ number of the current frame
  , getCurrGTimer::SetTimer -- ^ time, in which the frame will remain
  }
  deriving(Show)

-- | A data structure, which represents the animation state of Pacman
data PacmanAnimation = PacmanAnimation
  { getCurrPFrameNo::Frame  -- ^ number of the current frame
  , getCurrPTimer::SetTimer -- ^ time, in which the frame will remain}
  }
                     | NoAnimation
  deriving(Show)

-- | A data structure, which states whether the ghost is in base or not
data WhereIsGhost = InBase
                  | Stand
                  | OnField
  deriving(Show, Eq)

-- | A data structure, which states if a target is collided with or not
data Collision = Collided
               | NotCollided
  deriving(Show)

-- | A data structure, which states the next target in blocks
data NextTarget = Destiny
  { getDestinyPt::Pt
  }
                | NoDestiny
  { getDestinyPt::Pt
  }
  deriving(Show, Eq)

-- | A data structure, which represents the direction and the velocity of the dynamic board objects
data Velocity = VerticalDirection
  { getVelocity::Float  -- ^ Gives the velocity (pixel/second) in y-direction. Positiv = in direction and Negativ = against the direction
  }
              | HorizontalDirection
  { getVelocity::Float  -- ^ Gives the velocity (pixel/second) in x-direction. Positiv = in direction and Negativ = against the direction
  } deriving(Show, Eq)

-- | A data structure, which states if the dynamic object should stop or keep running
data ShouldStop = Stop
                | KeepRunning
  deriving(Show, Eq)

-- | A data structure, which represents the mortality of the dynamic structure
data Mortality = Mortal
               | Immortal
  deriving(Show, Eq)

-- | A data structure, which represents a set timer
data SetTimer = SetTimer
  { getTimeLeft::Float  -- ^ returns the rest time
  }
              | NoTimer
  deriving(Show, Eq)
---------------
-- Boardsize --

-- | A data structure, which represents the size of the board
data BoardSize = BoardSize
  { getWidthBoard::Int   -- ^ board width in blocks
  , getHeightBoard::Int  -- ^ board height in blocks
  , getSizeSquare::Float -- ^ length of each saure in Float pixel
  } deriving(Show, Eq)

-------------
-- TimeOut --

-- | A data structure, which shows, whether the game is on pause or not
data TimeOut  = Pause   -- ^ represents the Pause-mode, in which the game pauses
              | Running -- ^ represents the Running-mode, in which the game runs
  deriving(Show)

-----------
-- Timer --

-- | A data structure, which represnts the time, which has passed
data Timer  = Timer
  {
    getTime::Float  -- ^ current time of the game as Float
  }
  deriving(Show, Eq, Ord)

---------------
-- Highscore --

-- | A data structure, which represents the loaded scores
data Highscore = NoLoadedScores
               | LoadScores
               | LoadedScores
  {
    getLoadedScores::[Score] -- ^ The list of Scores, which are the Highscores (Top 5)
  }
  deriving(Show, Eq)
