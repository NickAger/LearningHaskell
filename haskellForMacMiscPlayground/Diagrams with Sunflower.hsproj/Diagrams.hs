-- Some examples from the Diagrams package

  -- JuicyPixels
import Codec.Picture

  -- Diagrams
import Diagrams.Prelude
import Diagrams.Backend.Rasterific

import BrewerSet

-- Two diagram examples

alignedCircles :: Diagram B R2
alignedCircles = hrule (2 * sum sizes) === circles # centerX
  where circles = hcat . map alignT . zipWith scale sizes
                $ repeat (circle 1)
        sizes   = [2,5,4,7,1,3]

node :: Int -> Diagram B R2
{- no label for the moment until font issues fixed
node n = text (show n) # fontSizeN 0.1 # fc white 
                       # translateY (-0.21)
         <> circle 0.2 # fc green # named n -}
node n = circle 0.2 # fc green # named n

arrowOpts = with & gaps  .~ small
                 & headLength .~ Global 0.2

tournament :: Int -> Diagram B R2
tournament n = decorateTrail (regPoly n 1) (map node [1..n])
  # applyAll [connectOutside' arrowOpts j k 
             | j <- [1 .. n-1], k <- [j+1 .. n]]

exampleDiagram = tournament 6

--example :: Diagram B
--example = mconcat (map (place pt) points) <> mconcat ellipses
--   where
--     ell = circle 1 # scaleX 2
--     pt  = circle 0.05 # fc blue # lw none
--     ellipses = [ ell # rotateBy r # translateX (2*r) | r <- [0, 1/12 .. 5/12] ]
--     points = allPairs ellipses >>= uncurry (intersectPointsP' 1e-8)
--     allPairs [] = []
--     allPairs (x:xs) = map (x,) xs ++ allPairs xs


mkCoords :: Int -> [P2]
mkCoords n =[coord (fromIntegral i) | i <- [1..n]]
  where
  coord m = p2 $ fromPolar (sqrt m) (2.4 * m)
  fromPolar r theta = (r * cos theta, r * sin theta)
  
floret :: Double -> Diagram B R2
floret r = circle 0.6 # lw none # fc (colors !! n)
  where
    n = floor (1.4 * sqrt r) `mod` 10
    colors = black : (reverse $ brewerSet YlOrBr 9)
    

sunflower :: Int ->  Diagram B R2
sunflower n = frame 4 $ position $ zip (mkCoords n) (florets n)
  where
    florets m = [floret (sqrt (fromIntegral i)) | i <- [1..m]]
    
composition1 ::  Diagram B R2
composition1 = circle 1 <> square (sqrt 2) 
