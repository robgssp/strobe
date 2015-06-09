import Graphics.UI.GLUT
import Data.IORef
import Data.Fixed
import Debug.Trace
import System.Random

points :: [(GLfloat,GLfloat)]
points = [(-1,-1),(1,-1),(1,1),(-1,1)]

hsvToRgb (Color3 h s v) = Color3 (r + m) (g + m) (b + m)
  where h1 = h / 60
        c = v * s
        x = c * (1 - (abs (((h / 60) `mod'` 2) - 1)))
        m = v - c
        (r,g,b) = case floor h1 of
                    0 -> (c,x,0)
                    1 -> (x,c,0)
                    2 -> (0,c,x)
                    3 -> (0,x,c)
                    4 -> (x,0,c)
                    5 -> (c,0,x)

main :: IO ()
main = do
  (prog, args) <- getArgsAndInitialize
  (initialDisplayMode $=) =<< (DoubleBuffered:) <$> get initialDisplayMode
  win <- createWindow "hello"
  lastC <- newIORef (Color3 1 0 0)
  idleCallback $= (Just (postRedisplay Nothing))
  print =<< get initialDisplayMode
  displayCallback $= (display lastC)
  mainLoop
  
display :: IORef (Color3 GLfloat) -> DisplayCallback
display lastC = do
  clear [ ColorBuffer ]
  oldC <- readIORef lastC
  newC <- if oldC == Color3 0 0 0 
          then do h <- randomRIO (0,360)
                  return $ hsvToRgb (Color3 h 1 1)
          else return $ Color3 0 0 0 
  writeIORef lastC $! newC
  print newC
  color newC
  renderPrimitive Polygon $ 
    mapM_ (\(x,y) -> vertex $ Vertex2 x y) points
  swapBuffers
