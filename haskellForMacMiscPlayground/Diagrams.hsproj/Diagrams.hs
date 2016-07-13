-- Some examples from the Diagrams package

  -- JuicyPixels
import Codec.Picture

  -- Diagrams
import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Diagrams.TwoD.Combinators
import Diagrams.TwoD.Vector

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

sqNewEnv :: Diagram B R2
sqNewEnv =
    c 
    <>
    square 4 # withEnvelope (c :: Diagram B R2) # bg blue
    where
      c = circle 1

withEnvelopeEx :: Diagram B R2
withEnvelopeEx = sqNewEnv # centerXY # pad 1.1

testCircle :: Diagram B R2
testCircle = circle 1

boundingTest :: Diagram B R2
boundingTest =  let items = ((square 1 ||| circle 1) # frame 0.5) ::  Diagram B R2
                  in items <> boundingRect items # dashingG [0.1,0.1] 0
                  
boundingTestWithPadding :: Diagram B R2
boundingTestWithPadding =  boundingTest # pad 1.1

beside1 :: Diagram B R2
beside1 =  beside(r2 (1,1)) (circle 1)(square 2)

beside2 :: Diagram B R2
beside2 =  (beside(r2 (1,-2)) (circle 1 # alignTL # showOrigin)(square 2 # showOrigin))

d1, d2 :: Diagram B R2
d1 = circle 1
d2 = (pentagon 1 === roundedRect 1.9 0.7 0.3)


-- see 2.5 Envelopes and local vector space
--example = hsep 1
--   [ (d1 ||| d2)          # showEnvelope' (with & ePoints .~ 360) # showOrigin
--   , (d1 ||| d2) # center # showEnvelope' (with & ePoints .~ 360) # showOrigin
--   ]

vector1 :: Diagram B R2
vector1 = fromOffsets [unitX, unitY, 2 *^ unit_X, unit_Y, unitX] # centerXY


vector2 :: Diagram B R2
vector2 = fromOffsets . map r2 $ [(1,1), (0,3), (-2,1), (-1,-4)]

vector3 :: Diagram B R2
vector3 = fromOffsets [1 ^& 1, 0 ^& 3, (-2) ^& 1, (-1) ^& (-4)]

vector4 :: Diagram B R2
vector4 = lwG 0.05 . mconcat . map fromOffsets
         $ [ [r *^ e (r @@ rad)]
           | r <- [33 * tau/32, 34 * tau/32 .. 2 * tau]
           ]