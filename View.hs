module View where

import Graphics.Gloss

-- import our modlues

import Model
import HelperFunctions(convPixel, convPixelInt, convertIntToNo, convertIntToDecInt)

-- type class to draw the board
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | new type class to draw the objects, which needs the Boardsize
class Drawable a where
  drawObj :: BoardSize  -- ^ BoardSize of the current board
          -> a          -- ^ object, which should be drawn
          -> Picture    -- ^ output-picture

----------------
-- BoardState --

-- | drawCont BoardState
--implementation of instance DrawableCont BoardState
instance Drawable BoardState where
  drawObj bS (Board stB dB@( Dynamic (Player pcS an _ _ _ _ _) _ ) ) = pictures [drawDynStatBoard pcS bS stB, drawObj bS dB]

-- Static objects --

-- type class to draw the StaticBoard
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | new type class to draw the StaticBoard-object, which needs PacmanState and BoardSize

class DrawDynStatBoard a where
  drawDynStatBoard  :: PacmanState  -- ^ represents the state of pacman
                    -> BoardSize    -- ^ represents the size of the board
                    -> a            -- ^ represent the state of the static object
                    -> Picture


-- | drawDynStatBoard StaticBoard
--implementation of instance DrawDynStatBoard StaticBoard
instance DrawDynStatBoard StaticBoard where
  drawDynStatBoard pcS bS (Static ws fs pUs) = pictures (map (drawObj bS) ws ++ map (drawDynStatBoard pcS bS) fs ++ map (drawObj bS) pUs)

-- | drawDynStatBoard Food
--implementation of instance drawDynStatBoard Food
instance DrawDynStatBoard Food where
  drawDynStatBoard _ (BoardSize w h s) (NoFd _) = blank
  drawDynStatBoard ReallyStuffedMode (BoardSize w h s) (Fd (Pt x y)) = translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ color chartreuse $ (polygon [(0.0, (-0.2)*s),(0.2*s, 0.0),(0.0, 0.2*s), ((-0.2)*s, 0.0)])
  drawDynStatBoard StuffedMode (BoardSize w h s) (Fd (Pt x y)) = translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ color red $ (polygon [(0.0, (-0.2)*s),(0.2*s, 0.0),(0.0, 0.2*s), ((-0.2)*s, 0.0)])
  drawDynStatBoard StarvingMode (BoardSize w h s) (Fd (Pt x y)) = translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ color blue $ (polygon [(0.0, (-0.2)*s),(0.2*s, 0.0),(0.0, 0.2*s), ((-0.2)*s, 0.0)])
  drawDynStatBoard _ (BoardSize w h s) (Fd (Pt x y)) = translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ color (light rose) $ (polygon [(0.0, (-0.2)*s),(0.2*s, 0.0),(0.0, 0.2*s), ((-0.2)*s, 0.0)])

-- | drawObj Powerup
--implementation of instance Drawable Powerup
instance Drawable Powerup where
  drawObj _ (EmptyChest _) = blank
  drawObj (BoardSize w h s) (Chest (Pt x y)) = translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ color (light aquamarine) $ (polygon [(0.0, (-0.45)*s),(0.45*s, (-0.2)*s),(0.45*s, 0.45*s), ( (-0.45)*s, 0.45*s), ((-0.45)*s, (-0.2)*s)])

-- | drawObj Wall
--implementation of instance Drawable Wall
instance Drawable Wall where
  drawObj (BoardSize w h s) (Wall (Pt x y)) = pictures
      [
        translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ color white $ rectangleSolid s s,
        translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ color black $ rectangleSolid (0.9*s) (0.9*s),
        translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ color (greyN 0.5) $ rectangleSolid (s * 0.8) (s * 0.8)
      ]
  drawObj (BoardSize w h s) (Path (Pt x y)) = translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ color white $ rectangleSolid s s
  drawObj (BoardSize w h s) (NotAllowedForPM (Pt x y)) = translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ color white $ rectangleSolid s s
  drawObj (BoardSize w h s) (OutOfField (Pt x y)) = translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ color black $ rectangleSolid s s
  drawObj (BoardSize w h s) (GhostStart (Pt x y)) = translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ color white $ rectangleSolid s s
  drawObj (BoardSize w h s) (FreWall (Pt x y)) = pictures
      [
        translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ color white $ rectangleSolid s s,
        translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ color black $ rectangleSolid (0.9*s) (0.9*s),
        translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ color (dark red) $ rectangleSolid (s * 0.8) (s * 0.8)
      ]

-- Dynamic objects --

-- | drawObj DynamicBoard
--implementation of instance Drawable DynamicBoard
instance Drawable DynamicBoard where
  drawObj bS (Dynamic p gs) = pictures (drawObj bS p : map (drawObj bS) gs)

-- | drawObj Pacman
--implementation of instance Drawable Pacman
instance Drawable Pacman where
  drawObj bs@(BoardSize w h s) (Player pcS an _ _ (Pt x y) _ _) = case pcS of
                                                          FastMode -> translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ color (dark yellow) $ circleSolid (0.3 * s)
                                                          KillerMode -> translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ color red $ circleSolid (0.4 * s)
                                                          _ -> translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ drawObj bs an

-- | drawObj Ghost
--implementation of instance Drawable Ghost
instance Drawable Ghost where
  drawObj bs@(BoardSize w h s) (FixedPatternOne Immortal an _ (Pt x y) _ _ _ _ _) = translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ pictures $
    [ color (red) $ (polygon [((-0.4)*s, (-0.4)*s),(0.4*s, (-0.4)*s),(0.0, 0.4*s)]), drawObj bs an]
  drawObj bs@(BoardSize w h s) (FollowTypeOne Immortal an _ (Pt x y) _ _ _ _) = translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ pictures $
    [ color (light green) $ (polygon [((-0.4)*s, (-0.4)*s),(0.4*s, (-0.4)*s),(0.0, 0.4*s)]), drawObj bs an]
  drawObj bs@(BoardSize w h s) (FollowTypeTwo Immortal an _ (Pt x y) _ _ _ _) = translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ pictures $
    [ color (dim cyan) $ (polygon [((-0.4)*s, (-0.4)*s),(0.4*s, (-0.4)*s),(0.0, 0.4*s)]), drawObj bs an]
  drawObj bs@(BoardSize w h s) (FollowTypeThree Immortal an _ (Pt x y) _ _ _ _) = translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ pictures $
    [ color (bright orange) $ (polygon [((-0.4)*s, (-0.4)*s),(0.4*s, (-0.4)*s),(0.0, 0.4*s)]), drawObj bs an]
  drawObj bs@(BoardSize w h s) (FixedPatternOne Mortal an _ (Pt x y) _ _ _ _ _) = translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ pictures $
    [ color blue $ (polygon [((-0.4)*s, (-0.4)*s),(0.4*s, (-0.4)*s),(0.0, 0.4*s)]), drawObj bs an]
  drawObj bs@(BoardSize w h s) (FollowTypeOne Mortal an _ (Pt x y) _ _ _ _) = translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ pictures $
    [ color blue $ (polygon [((-0.4)*s, (-0.4)*s),(0.4*s, (-0.4)*s),(0.0, 0.4*s)]), drawObj bs an]
  drawObj bs@(BoardSize w h s) (FollowTypeTwo Mortal an _ (Pt x y) _ _ _ _) = translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ pictures $
    [ color blue $ (polygon [((-0.4)*s, (-0.4)*s),(0.4*s, (-0.4)*s),(0.0, 0.4*s)]), drawObj bs an]
  drawObj bs@(BoardSize w h s) (FollowTypeThree Mortal an _ (Pt x y) _ _ _ _) = translate (x - convPixel s w * 0.5) (y - convPixel s h *0.5) $ pictures $
    [ color blue $ (polygon [((-0.4)*s, (-0.4)*s),(0.4*s, (-0.4)*s),(0.0, 0.4*s)]), drawObj bs an]

-- | drawObj GhostAnimation
--implementation of instance Drawable GhostAnimation
instance Drawable GhostAnimation where
  drawObj (BoardSize w h s) (GhostAnimation 0 _) = color (white) $ pictures
    [ translate (0.1 * s) (0.0 * s) $ circle (0.4*0.5*0.25*s)
    , translate ((-0.1) * s) (0.0 * s) $ circle (0.4*0.5*0.25*s)
    , translate (0.0 * s) ((-0.2) * s) $ rectangleSolid (0.4*s) (0.4*0.05*s)
    ]
  drawObj (BoardSize w h s) (GhostAnimation 1 _) = color (white) $ pictures
    [ translate (0.1 * s) (0.0 * s) $ rotate (-45.0) $ rectangleSolid (0.4*0.25*s) (s / s)
    , translate ((-0.1) * s) (0.0 * s) $ rotate (45.0) $ rectangleSolid (0.4*0.25*s) (s / s)
    , translate (0.0 * s) ((-0.2) * s) $ rectangleSolid (0.4*s) (0.4*0.05*s)
    ]
  drawObj (BoardSize w h s) (GhostAnimation 2 _) = color (white) $ pictures
    [ translate (0.1 * s) (0.0 * s) $ rotate (-45.0) $ rectangleSolid (0.4*0.25*s) (s / s)
    , translate ((-0.1) * s) (0.0 * s) $ rotate (45.0) $ rectangleSolid (0.4*0.25*s) (s / s)
    , translate (0.0 * s) ((-0.2) * s) $ circle (0.4*0.5*0.25*s)
    ]
  drawObj (BoardSize w h s) (GhostAnimation 3 _) = color (white) $ pictures
    [ translate (0.1 * s) (0.0 * s) $ rotate (-45.0) $ rectangleSolid (0.4*0.25*s) (s / s)
    , translate ((-0.1) * s) (0.0 * s) $ rotate (45.0) $ rectangleSolid (0.4*0.25*s) (s / s)
    , translate (0.0 * s) ((-0.2) * s) $ rectangleSolid (0.4*s) (0.4*0.05*s)
    ]

-- implementation of instance drawable for pacman animation
instance Drawable PacmanAnimation where
  drawObj (BoardSize w h s) NoAnimation = color yellow $ circleSolid (0.4 * s)
  drawObj (BoardSize w h s) (PacmanAnimation 0 _) = color yellow  $ circleSolid (0.4 * s)
  drawObj (BoardSize w h s) (PacmanAnimation 1 _) = blank

----------------
-- WorldState --

-- | drawObj WorldState
--implementation of instance Drawable WorldState
instance Drawable WorldState where
  drawObj  sB@(BoardSize w h s) (World score life totFood) = pictures  [
                                                                      drawObj sB life,
                                                                      translate (0.0) (0.5*s - convPixel s h *0.5) $ drawDecInt sB $ convertIntToDecInt (getScore score),
                                                                      pictures
                                                                        [
                                                                          translate (convPixel s w * 0.5 - 4.5*s) (0.5*s - convPixel s h *0.5) $ color (light rose) $ (polygon [(0.0, (-0.2)*s),(0.2*s, 0.0),(0.0, 0.2*s), ((-0.2)*s, 0.0)]),
                                                                          translate (convPixel s w * 0.5 - 2.5*s) (0.5*s - convPixel s h *0.5) $ drawDecInt sB $ convertIntToDecInt (getNoTotalFood totFood)
                                                                        ]
                                                                      ]

----------
-- drawObj of Score and TotalFood, which are represented by No- and DecInt-objects

-- | drawNo, which returns a picture-object of a No-object, scaled on the BoardSize
drawNo  :: BoardSize  -- ^ BoardSize of the current board
        -> No         -- ^ No-object, which represents an integer
        -> Picture    -- ^ output-picture
drawNo (BoardSize _ _ s) QuestionMark = pictures  [
                                                  translate (0.0) (0.1*s) $ color white $ rectangleSolid (0.1*s) (0.6*s),
                                                  translate (0.0) ((-0.35)*s) $ color white $ rectangleSolid (0.1*s) (0.1*s)
                                                  ]

drawNo (BoardSize _ _ s) Zero = pictures  [
                                          translate ((-0.15)*s) (0.0) $ color white $ rectangleSolid (0.1*s) (0.8*s),
                                          translate (0.15*s) (0.0) $ color white $ rectangleSolid (0.1*s) (0.8*s),
                                          translate (0.0) (0.35*s) $ color white $ rectangleSolid (0.4*s) (0.1*s),
                                          translate (0.0) ((-0.35)*s) $ color white $ rectangleSolid (0.4*s) (0.1*s)
                                          ]

drawNo (BoardSize _ _ s) One = pictures [
                                        translate (0.0) (0.0) $ color white $ rectangleSolid (0.1*s) (0.8*s)
                                        ]

drawNo (BoardSize _ _ s) Two = pictures   [
                                          translate (0.15*s) (0.2*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                          translate ((-0.15)*s) ((-0.2)*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                          translate (0.0) (0.35*s) $ color white $ rectangleSolid (0.4*s) (0.1*s),
                                          translate (0.0) (0.0) $ color white $ rectangleSolid (0.4*s) (0.1*s),
                                          translate (0.0) ((-0.35)*s) $ color white $ rectangleSolid (0.4*s) (0.1*s)
                                          ]

drawNo (BoardSize _ _ s) Three = pictures [
                                          translate (0.15*s) (0.2*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                          translate ((0.15)*s) ((-0.2)*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                          translate (0.0) (0.35*s) $ color white $ rectangleSolid (0.4*s) (0.1*s),
                                          translate (0.0) (0.0) $ color white $ rectangleSolid (0.4*s) (0.1*s),
                                          translate (0.0) ((-0.35)*s) $ color white $ rectangleSolid (0.4*s) (0.1*s)
                                          ]

drawNo (BoardSize _ _ s) Four = pictures [
                                          translate ((-0.15)*s) (0.2*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                          translate ((0.15)*s) (0.0) $ color white $ rectangleSolid (0.1*s) (0.8*s),
                                          translate (0.0) (0.0) $ color white $ rectangleSolid (0.4*s) (0.1*s)
                                          ]

drawNo (BoardSize _ _ s) Five = pictures [
                                          translate ((-0.15)*s) (0.2*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                          translate ((0.15)*s) ((-0.2)*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                          translate (0.0) (0.35*s) $ color white $ rectangleSolid (0.4*s) (0.1*s),
                                          translate (0.0) (0.0) $ color white $ rectangleSolid (0.4*s) (0.1*s),
                                          translate (0.0) ((-0.35)*s) $ color white $ rectangleSolid (0.4*s) (0.1*s)
                                          ]

drawNo (BoardSize _ _ s) Six = pictures   [
                                          translate ((-0.15)*s) (0.2*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                          translate ((-0.15)*s) ((-0.2)*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                          translate ((0.15)*s) ((-0.2)*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                          translate (0.0) (0.35*s) $ color white $ rectangleSolid (0.4*s) (0.1*s),
                                          translate (0.0) (0.0) $ color white $ rectangleSolid (0.4*s) (0.1*s),
                                          translate (0.0) ((-0.35)*s) $ color white $ rectangleSolid (0.4*s) (0.1*s)
                                          ]

drawNo (BoardSize _ _ s) Seven = pictures [
                                          translate (0.15*s) (0.2*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                          translate ((0.15)*s) ((-0.2)*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                          translate (0.0) (0.35*s) $ color white $ rectangleSolid (0.4*s) (0.1*s)
                                          ]

drawNo (BoardSize _ _ s) Eight = pictures  [
                                          translate ((-0.15)*s) (0.0) $ color white $ rectangleSolid (0.1*s) (0.8*s),
                                          translate (0.15*s) (0.0) $ color white $ rectangleSolid (0.1*s) (0.8*s),
                                          translate (0.0) (0.35*s) $ color white $ rectangleSolid (0.4*s) (0.1*s),
                                          translate (0.0) (0.0) $ color white $ rectangleSolid (0.4*s) (0.1*s),
                                          translate (0.0) ((-0.35)*s) $ color white $ rectangleSolid (0.4*s) (0.1*s)
                                          ]

drawNo (BoardSize _ _ s) Nine = pictures [
                                          translate ((-0.15)*s) (0.2*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                          translate ((0.15)*s) (0.0) $ color white $ rectangleSolid (0.1*s) (0.8*s),
                                          translate (0.0) (0.0) $ color white $ rectangleSolid (0.4*s) (0.1*s),
                                          translate (0.0) (0.35*s) $ color white $ rectangleSolid (0.4*s) (0.1*s)
                                          ]


-- | drawDecInt, which returns a picture-object of a DecInt-object, scaled on the BoardSize
drawDecInt  :: BoardSize  -- ^ BoardSize of the current board
            -> DecInt     -- ^ DecInt-object, which represents an integer
            -> Picture    -- ^ output-picture
drawDecInt bS@(BoardSize _ _ s) (DecInt h t o) = pictures [
                                                          translate ((-1.0)*s) (0.0) $ color black $ rectangleSolid s s,
                                                          translate ((0.0)*s) (0.0) $ color black $ rectangleSolid s s,
                                                          translate ((1.0)*s) (0.0) $ color black $ rectangleSolid s s,
                                                          translate ((-1.0)*s) (0.0) $ drawNo bS h,
                                                          translate (0.0) (0.0) $ drawNo bS t,
                                                          translate ((1.0)*s) (0.0) $ drawNo bS o
                                                          ]


-- | drawObj Life
--implementation of instance Drawable Life
instance Drawable Life where
  drawObj sB@(BoardSize w h s) ZeroLives = blank
  drawObj sB@(BoardSize w h s) OneLife = translate (1.5*s - convPixel s w * 0.5) (0.5*s - convPixel s h *0.5) $ color yellow $ circleSolid (0.4 * s)
  drawObj sB@(BoardSize w h s) TwoLives = pictures [
                                            translate (1.5*s - convPixel s w * 0.5) (0.5*s - convPixel s h *0.5) $ color yellow $ circleSolid (0.4 * s),
                                            translate (2.5*s - convPixel s w * 0.5) (0.5*s - convPixel s h *0.5) $ color yellow $ circleSolid (0.4 * s)
                                            ]
  drawObj sB@(BoardSize w h s) ThreeLives = pictures [
                                              translate (1.5*s - convPixel s w * 0.5) (0.5*s - convPixel s h *0.5) $ color yellow $ circleSolid (0.4 * s),
                                              translate (2.5*s - convPixel s w * 0.5) (0.5*s - convPixel s h *0.5) $ color yellow $ circleSolid (0.4 * s),
                                              translate (3.5*s - convPixel s w * 0.5) (0.5*s - convPixel s h *0.5) $ color yellow $ circleSolid (0.4 * s)
                                              ]

--------------------------------------------------------------------------------

-- | new type class to draw the objects, which need no Boardsize (data-container)
class DrawableCont a where
  drawCont  :: a        -- ^ object, which should be drawn
            -> Picture  -- ^ output-picture


-- | drawCont GameState
--implementation of instance DrawableCont GameState
instance DrawableCont GameState where
  -- Draw your own score
  drawCont (GameOver (Score sc) NoLoadedScores (InitBoard _ sB)) = translate (0.0) (0.0) $ scale 3.0 3.0 $ color (light blue) $ drawDecInt sB $ convertIntToDecInt sc
  -- Don't draw something, because the Highscore is loading
  drawCont (GameOver _ LoadScores _) = blank
  -- Draw your score at the top with a increased size, on the bottom of the field are the Top 5- scores with your score, if big enough, inside
  drawCont (GameOver (Score sc) (LoadedScores scs) (InitBoard _ sB@(BoardSize _ _ s))) =  let scPicList = [ scale 2.0 2.0 $ (drawDecInt sB) $ (convertIntToDecInt sc)] ++ (((drawDecInt sB) . convertIntToDecInt . getScore ) <$> scs)
                                                                                              yCoords = [fromIntegral(2*n) * s | n <- [(- ceiling(fromIntegral(length scPicList) / 2.0)) .. (floor(fromIntegral(length scPicList) / 2.0) - 1) ] ]
                                                                                              transList = map (translate 0.0) yCoords
                                                                                          in pictures $ (zipWith ($) (reverse transList)  (scPicList) )
  -- Draw the Pause-Menu
  drawCont (Game (InitBoard _ sB@(BoardSize w h s)) _ _ Pause _) = let  pPic = pictures   [
                                                                                          translate (0.0) (0.0) $ color black $ rectangleSolid s s,
                                                                                          translate ((-0.15)*s) (0.2*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                                                                          translate ((-0.15)*s) ((-0.2)*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                                                                          translate ((0.15)*s) ((0.2)*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                                                                          translate (0.0) (0.35*s) $ color white $ rectangleSolid (0.4*s) (0.1*s),
                                                                                          translate (0.0) (0.0) $ color white $ rectangleSolid (0.4*s) (0.1*s)
                                                                                          ]
                                                                        aPic = pictures   [
                                                                                          translate (0.0) (0.0) $ color black $ rectangleSolid s s,
                                                                                          translate ((-0.15)*s) (0.0) $ color white $ rectangleSolid (0.1*s) (0.8*s),
                                                                                          translate (0.15*s) (0.0) $ color white $ rectangleSolid (0.1*s) (0.8*s),
                                                                                          translate (0.0) (0.35*s) $ color white $ rectangleSolid (0.4*s) (0.1*s),
                                                                                          translate (0.0) (0.0) $ color white $ rectangleSolid (0.4*s) (0.1*s)
                                                                                          ]
                                                                        uPic = pictures   [
                                                                                          translate (0.0) (0.0) $ color black $ rectangleSolid s s,
                                                                                          translate ((-0.15)*s) (0.0) $ color white $ rectangleSolid (0.1*s) (0.8*s),
                                                                                          translate (0.15*s) (0.0) $ color white $ rectangleSolid (0.1*s) (0.8*s),
                                                                                          translate (0.0) ((-0.35)*s) $ color white $ rectangleSolid (0.4*s) (0.1*s)
                                                                                          ]
                                                                        sPic = pictures   [
                                                                                          translate (0.0) (0.0) $ color black $ rectangleSolid s s,
                                                                                          translate ((-0.15)*s) (0.2*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                                                                          translate ((0.15)*s) ((-0.2)*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                                                                          translate (0.0) (0.35*s) $ color white $ rectangleSolid (0.4*s) (0.1*s),
                                                                                          translate (0.0) (0.0) $ color white $ rectangleSolid (0.4*s) (0.1*s),
                                                                                          translate (0.0) ((-0.35)*s) $ color white $ rectangleSolid (0.4*s) (0.1*s)
                                                                                          ]
                                                                        ePic = pictures   [
                                                                                          translate (0.0) (0.0) $ color black $ rectangleSolid s s,
                                                                                          translate ((-0.15)*s) (0.2*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                                                                          translate ((-0.15)*s) ((-0.2)*s) $ color white $ rectangleSolid (0.1*s) (0.4*s),
                                                                                          translate (0.0) (0.35*s) $ color white $ rectangleSolid (0.4*s) (0.1*s),
                                                                                          translate (0.0) (0.0) $ color white $ rectangleSolid (0.4*s) (0.1*s),
                                                                                          translate (0.0) ((-0.35)*s) $ color white $ rectangleSolid (0.4*s) (0.1*s)
                                                                                          ]
                                                                        pPause = pictures [
                                                                                          translate ((-2.0)*s) (0.0) $ pPic,
                                                                                          translate ((-1.0)*s) (0.0) $ aPic,
                                                                                          translate (0.0) (0.0) $ uPic,
                                                                                          translate ((1.0)*s) (0.0) $ sPic,
                                                                                          translate ((2.0)*s) (0.0) $ ePic
                                                                                          ]
                                                                  in scale 3.0 3.0 $ pPause

  -- Draw the running Game Board
  drawCont (Game iG ws bs Running ti) = pictures [drawObj (getInitBoardSize iG) bs, drawObj (getInitBoardSize iG) ws]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
