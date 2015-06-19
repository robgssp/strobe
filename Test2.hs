{-# LANGUAGE FlexibleContexts #-}
module Main where

import Graphics.GPipe
import qualified Data.Vec as Vec
import Data.Vec.Nat
import Data.Monoid
import Data.IORef
import Graphics.UI.GLUT
    (Window,
    mainLoop,
    postRedisplay,
    idleCallback,
    getArgsAndInitialize,
    ($=))
    
main :: IO ()
main = do
    getArgsAndInitialize
    angleRef <- newIORef 0.0
    newWindow "Spinning box" (100:.100:.()) (800:.600:.()) (renderFrame angleRef) initWindow
    mainLoop       

renderFrame :: IORef Float -> Vec2 Int -> IO (FrameBuffer RGBFormat () ())
renderFrame angleRef size = do
    angle <- readIORef angleRef
    writeIORef angleRef ((angle + 0.005) `mod'` (2*pi))
    return $ cubeFrameBuffer angle size

initWindow :: Window -> IO ()
initWindow win = idleCallback $= Just (postRedisplay (Just win))

cube :: PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
cube = mconcat [sidePosX, sideNegX, sidePosY, sideNegY, sidePosZ, sideNegZ]

sidePosX = toGPUStream TriangleStrip $ zip3 [1:.0:.0:.(), 1:.1:.0:.(), 1:.0:.1:.(), 1:.1:.1:.()] (repeat (1:.0:.0:.()))    uvCoords
sideNegX = toGPUStream TriangleStrip $ zip3 [0:.0:.1:.(), 0:.1:.1:.(), 0:.0:.0:.(), 0:.1:.0:.()] (repeat ((-1):.0:.0:.())) uvCoords
sidePosY = toGPUStream TriangleStrip $ zip3 [0:.1:.1:.(), 1:.1:.1:.(), 0:.1:.0:.(), 1:.1:.0:.()] (repeat (0:.1:.0:.()))    uvCoords
sideNegY = toGPUStream TriangleStrip $ zip3 [0:.0:.0:.(), 1:.0:.0:.(), 0:.0:.1:.(), 1:.0:.1:.()] (repeat (0:.(-1):.0:.())) uvCoords
sidePosZ = toGPUStream TriangleStrip $ zip3 [1:.0:.1:.(), 1:.1:.1:.(), 0:.0:.1:.(), 0:.1:.1:.()] (repeat (0:.0:.1:.()))    uvCoords
sideNegZ = toGPUStream TriangleStrip $ zip3 [0:.0:.0:.(), 0:.1:.0:.(), 1:.0:.0:.(), 1:.1:.0:.()] (repeat (0:.0:.(-1):.())) uvCoords

uvCoords = [0:.0:.(), 0:.1:.(), 1:.0:.(), 1:.1:.()]

transformedCube :: Float -> Vec2 Int -> PrimitiveStream Triangle (Vec4 (Vertex Float), (Vec3 (Vertex Float), Vec2 (Vertex Float)))
transformedCube angle size = fmap (transform angle size) cube

transform :: Float
          -> Vec2 Int
          -> (Vec3 (Vertex Float),Vec3 (Vertex Float),Vec2 (Vertex Float))
          -> (Vec4 (Vertex Float),(Vec3 (Vertex Float),Vec2 (Vertex Float)))
transform angle (width:.height:.()) (pos, norm, uv) = (transformedPos, (transformedNorm, uv))
    where
        modelMat = rotationVec (normalize (1:.0.5:.0.3:.())) angle `multmm` translation (-0.5)
        viewMat = translation (-(0:.0:.2:.())) 
        projMat = perspective 1 100 (pi/3) (fromIntegral width / fromIntegral height)
        viewProjMat = projMat `multmm` viewMat
        transformedPos = toGPU (viewProjMat `multmm` modelMat) `multmv` (homPoint pos :: Vec4 (Vertex Float))
        transformedNorm = toGPU (Vec.map (Vec.take n3) $ Vec.take n3 modelMat) `multmv` norm

rasterizedCube :: Float -> Vec2 Int -> FragmentStream (Vec3 (Fragment Float), Vec2 (Fragment Float))
rasterizedCube angle size = rasterizeFront $ transformedCube angle size

litCube :: Float -> Vec2 Int -> FragmentStream (Color RGBFormat (Fragment Float))
litCube angle size = fmap enlight $ rasterizedCube angle size

enlight :: (Vec3 (Fragment Float), Vec2 (Fragment Float)) -> Color RGBFormat (Fragment Float)
enlight (norm, uv) = RGB (0:.2:.0:. ())

cubeFrameBuffer :: Float -> Vec2 Int -> FrameBuffer RGBFormat () ()
cubeFrameBuffer angle size = paintSolid (litCube angle size) emptyFrameBuffer

paintSolid = paintColor NoBlending (RGB $ Vec.vec True)
emptyFrameBuffer = newFrameBufferColor (RGB 0)
