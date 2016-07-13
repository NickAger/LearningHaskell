
module DiagramsArrows where

-- Some examples from the Diagrams package

  -- JuicyPixels
import Codec.Picture

  -- Diagrams
import Diagrams.Prelude
import Diagrams.Backend.Rasterific

text' d s = (strokeP $ textSVG' (TextOpts lin2 INSIDE_H KERN False d d) s)
           # lw none # fc black

stateLabel = text' 6
arrowLabel txt size = text' size txt

state  = circle 4 # fc silver
fState = circle 3.7 # fc lightblue <> state

points = map p2 [ (0, 12), (12, 16), (24, 12), (24, 21), (36, 16), (48, 12)
                 , (48, 21), (12, 0), (7, 7), (24, 4), (36, 0), (46, 0)]

ds = [ (stateLabel "1" <> state)  # named "1"
        ,  arrowLabel "0-9" 4
        , (stateLabel "2" <> state)  # named "2"
        ,  arrowLabel "0-9" 4
        ,  arrowLabel "." 8
        , (stateLabel "3" <> fState) # named "3"
        ,  arrowLabel "0-9" 4
        , (stateLabel "4" <> state)  # named "4"
        ,  arrowLabel "." 8
        ,  arrowLabel "0-9" 4
        , (stateLabel "5" <> fState) # named "5"
        ,  arrowLabel "0-9" 4]

states = position (zip points ds)

shaft  = arc xDir (-1/6 @@ turn)
shaft' = arc xDir (-2.7/5 @@ turn)
line = trailFromOffsets [unitX]

arrowStyle1 = (with  & arrowHead  .~ spike & headLength .~ normal
                      & arrowShaft .~ shaft)

arrowStyle2 = (with  & arrowHead   .~ spike
                      & arrowShaft  .~ shaft' & arrowTail .~ lineTail
                      & tailTexture .~ solid black & lengths .~ normal)

arrowStyle3 = (with  & arrowHead  .~ spike  & headLength .~ normal
                      & arrowShaft .~ line)

example = states
   # connectOutside' arrowStyle1 "1" "2"
   # connectOutside' arrowStyle3 "1" "4"
   # connectPerim'   arrowStyle2 "2" "2" (4/12 @@ turn) (2/12 @@ turn)
   # connectOutside' arrowStyle1 "2" "3"
   # connectPerim'   arrowStyle2 "3" "3" (4/12 @@ turn) (2/12 @@ turn)
   # connectOutside' arrowStyle1 "4" "5"
   # connectPerim'   arrowStyle2 "5" "5" (1/12 @@ turn) (-1/12 @@ turn)

testCircle :: Diagram B R2
testCircle = circle 1

boundingTest :: Diagram B R2
boundingTest =  let items = ((square 1 ||| circle 1) # frame 0.5) ::  Diagram B R2
                  in items <> boundingRect items # dashingG [0.1,0.1] 0
                  
boundingTestWithPadding :: Diagram B R2
boundingTestWithPadding =  boundingTest # pad 1.1