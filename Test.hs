{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
import Control.Monad
import Data.IORef
import Control.Arrow
import Graphics.UI.GLUT
       (keyboardMouseCallback, joystickCallback, idleCallback, Key(..),
        KeyState(..), JoystickButtons(..), JoystickPosition(..),
        getArgsAndInitialize, SpecialKey(..), PerWindowKeyRepeat(..),
        perWindowKeyRepeat, postRedisplay, mainLoop, ($=), get,
        elapsedTime)
import Data.Vec hiding (map,get)
--import Data.Vec.Nat
import Graphics.GPipe
import Graphics.GPipe.Format
import FRP.Yampa hiding (normalize)
import FRP.Yampa.Vector2
import FRP.Yampa.Integration
import FRP.Yampa.VectorSpace hiding (normalize)
import Data.Time.Clock.POSIX
import Control.Lens
import Data.Monoid
import Text.Printf
import Control.Applicative
import Data.Array.Storable
import Foreign.Storable
import Foreign.Ptr

circPoints (fromIntegral -> n) = map (\v -> (sin (v*2*pi/n), cos (v*2*pi/n))) [1..n]

data Input
  = Key Key KeyState
  | Joystick JoystickButtons JoystickPosition
  deriving (Eq,Show)

type State = (Double,Double,Double)

main :: IO ()
main =
  do getArgsAndInitialize

     state <- newIORef (0,0,0)
     oldTime <- newIORef 0

     rh <- reactInit (return $ Key (Char 'a') Up)
                     (\_ _ st ->
                        do writeIORef state st
                           return False)
                     ctl

     let doReact st = 
           do t <- get oldTime
              t' <- get elapsedTime
              oldTime $= t'
              let td = fromIntegral (t' - t) / 1000
              printf "Feeding %s at %d\n" (show st) (t' - t)
              void $ react rh (td, st)
         initWindow win = return ()
           -- do keyboardMouseCallback $=
           --           Just (\key kstate _ _ -> doReact (Just (Key key kstate)))

           --    joystickCallback $=
           --           Just (\buttons pos -> doReact (Just (Joystick buttons pos)), 10)

           --    idleCallback $= Just (postRedisplay Nothing)

     newWindow "test" (100:.100:.()) (800:.600:.()) (display state (doReact Nothing)) initWindow

     perWindowKeyRepeat $= PerWindowKeyRepeatOff

     let keyval dir = if dir == Up then 1 else 0

     mainLoop

display :: IORef State -> IO () -> Vec2 Int -> IO (FrameBuffer RGBFormat () ())
display state step size = 
  do step
     st <- readIORef state
     return (frameBuffer (redFrag (trans (cube TriangleFan))))

cube :: Primitive p => p -> PrimitiveStream p (Vec3 (Vertex Float))
cube p = mconcat (map ((toGPUStream p).(`map` ps)) [\(x,y) -> x:.y:.0:.()
                                                   ,\(x,y) -> y:.x:.1:.()
                                                   ])
  where ps = [(0,0),(1,0),(1,1),(0,1)]

tri :: PrimitiveStream Triangle (Vec3 (Vertex Float))
tri = toGPUStream TriangleStrip [1:.0:.0:.(), 0:.1:.0:.(), 0:.0:.0:.()]

trans :: Primitive p => PrimitiveStream p (Vec3 (Vertex Float)) -> PrimitiveStream p (Vec4 (Vertex Float))
trans = fmap trans'
  where trans' pos = translation ((-0.5):.(-0.5):.0:.()) `multmv` homPoint pos

redFrag :: PrimitiveStream p (Vec4 (Vertex Float)) -> FragmentStream (Color RGBFormat (Fragment Float))
redFrag vs = const (RGB (1:.0:.0:.())) <$> rasterizeFront ((,()) <$> vs)

frameBuffer :: FragmentStream (Color RGBFormat (Fragment Float)) -> FrameBuffer RGBFormat () ()
frameBuffer fs = paintSolid fs emptyFrameBuffer

paintSolid = paintColor NoBlending (RGB (vec True))
emptyFrameBuffer = newFrameBufferColor (RGB 0)

keyIsDown :: Key -> SF Input Bool
keyIsDown k = loopPre False
                      (arr $ \case 
                               (Key k' Down,_) | k == k' -> (True,True)
                               (Key k' Up, _) | k == k' -> (False,False)
                               (_,last) -> (last,last))

joystickEvent (Joystick btns pos) = Event (btns,pos)
joystickEvent _ = NoEvent

clampN l h v | v > h = h
             | v < l = l
             | otherwise = v

clampVec lx hx ly hy v = vector2 (clampN lx hx (vector2X v))
                                 (clampN ly hy (vector2Y v))

ctl :: SF Input State
ctl = 
  proc input -> do
        up <- keyNum (SpecialKey KeyUp) -< input
        down <- keyNum (SpecialKey KeyDown) -< input
        left <- keyNum (SpecialKey KeyLeft) -< input
        right <- keyNum (SpecialKey KeyRight) -< input
        (btns,JoystickPosition jx jy jr) <- hold (undefined,JoystickPosition 0 0 0) -< joystickEvent input

        rec
          angle <- integral -< -rot * 2
          wallX <- edge -< abs (vector2X pos) > 1
          wallY <- edge -< abs (vector2Y pos) > 1
          let wallX' = wallX `tag` vector2 (-2 * vector2X vel0) 0
              wallY' = wallY `tag` vector2 0 (-2 * vector2Y vel0)
              acc = up - down + (fromIntegral jy / 1000)
              rot = right - left + (fromIntegral jx / 1000)
          vel <- impulseIntegral -< (vector2Polar acc angle, mergeBy (^+^) wallX' wallY')
          vel0 <- iPre (vector2 0 0) -< vel
        
          wall' <- iPre NoEvent -< wallX `merge` wallY
          let bound = clampVec (-1) 1 (-1) 1 pos0
          pos <- impulseIntegral -< (vel, Event (bound ^-^ pos0))
          pos0 <- iPre (vector2 0 0) -< pos

        returnA -< (vector2X pos, vector2Y pos, angle)
  where boolN True = 1
        boolN False = 0
        keyNum k = boolN ^<< keyIsDown k
