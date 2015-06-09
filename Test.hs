{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
import Control.Monad
import Graphics.UI.GLUT
import Data.IORef
import Control.Arrow
import FRP.Yampa
import FRP.Yampa.Vector2
import FRP.Yampa.Integration
import FRP.Yampa.VectorSpace
import Data.Time.Clock.POSIX
import Control.Lens
import Text.Printf

circPoints (fromIntegral -> n) = map (\v -> (sin (v*2*pi/n), cos (v*2*pi/n))) [1..n]

data Input
  = Key Key KeyState
  | Joystick JoystickButtons JoystickPosition
  deriving (Eq,Show)
type State = (Double,Double,Double)

main :: IO ()
main =
  do (prog,args) <- getArgsAndInitialize
     (initialDisplayMode $=) =<< (DoubleBuffered :) <$> get initialDisplayMode
     win <- createWindow "hello"
     
     state <- newIORef (0,0,0)
     oldTime <- newIORef 0
     
     rh <- reactInit (return $ Key (Char 'a') Up)
                     (\_ _ st ->
                        do writeIORef state st
                           return False)
                     ctl

     let doReact st = do t <- get oldTime
                         t' <- get elapsedTime
                         oldTime $= t'
                         let td = fromIntegral (t' - t) / 1000
                         printf "Feeding %s at %d\n" (show st) (t'-t)
                         void $ react rh (td, st)

     displayCallback $= (display state (doReact Nothing))
     perWindowKeyRepeat $= PerWindowKeyRepeatOff

     let keyval dir = if dir == Up then 1 else 0
     keyboardMouseCallback $=
       Just (\key kstate _ _ -> doReact (Just (Key key kstate)))
       
     joystickCallback $=
       Just (\buttons pos -> doReact (Just (Joystick buttons pos)), 10)

     idleCallback $=
       Just (postRedisplay (Just win))
     
     mainLoop

display :: IORef State -> IO () -> DisplayCallback
display state step = 
  do clear [ ColorBuffer ]
     loadIdentity
     
     step
     (x,y,r) <- get state

     color $ Color3 1 1 (1::GLfloat)
     translate $ Vector3 (realToFrac x) (realToFrac y) (0::GLfloat)
     rotate ((realToFrac r)*180/pi) $ Vector3 0 0 (1::GLfloat)
                                              
     cube 0.2
     translate $ Vector3 0.15 0 (0::GLfloat)
     color $ Color3 1 0 (0::GLfloat)
     cube 0.1
     -- forM_ (circPoints 7) $ \(x,y) -> 
     --   preservingMatrix $ do 
     --     color $ Color3 ((x+1)/2) ((y+1)/2) (0::GLfloat)
     --     translate $ Vector3 x y 0
     --     cube 0.1
     swapBuffers

keyIsDown :: Key -> SF Input Bool
keyIsDown k = loopPre False
                      (arr $ \case 
                               (Key ((==k) -> True) Down,_) -> (True,True)
                               (Key ((==k) -> True) Up, _) -> (False,False)
                               (_,last) -> (last,last))
                               
joystickEvent (Joystick btns pos) = Event (btns,pos)
joystickEvent _ = NoEvent

clamp l h v | v > h = h
            | v < l = l
            | otherwise = v

clampVec lx hx ly hy v = vector2 (clamp lx hx (vector2X v))
                                 (clamp ly hy (vector2Y v))

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

cube :: GLfloat -> IO ()
cube w = renderPrimitive Quads $ 
           do mapM_ (\(a,b) -> vertex $ Vertex3 w a b) verts
              mapM_ (\(a,b) -> vertex $ Vertex3 (-w) a b) verts
              mapM_ (\(a,b) -> vertex $ Vertex3 a w b) verts
              mapM_ (\(a,b) -> vertex $ Vertex3 a (-w) b) verts
              mapM_ (\(a,b) -> vertex $ Vertex3 a b w) verts
              mapM_ (\(a,b) -> vertex $ Vertex3 a b (-w)) verts
  where verts = [(w,w),(w,-w),(-w,-w),(-w,w)]
