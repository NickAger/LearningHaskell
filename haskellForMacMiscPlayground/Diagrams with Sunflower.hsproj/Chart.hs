-- Some examples from the Chart package

module Chart where

import Data.Map (fromList)

  -- JuicyPixels
import Codec.Picture

 -- SVGFonts
import qualified Graphics.SVGFonts.ReadFont as F

  -- Diagrams
import Diagrams.Prelude
import Diagrams.Backend.Rasterific

  -- Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Backend.Types

import ExampleStocks

-- Some of the Chart example from the package docs

-- Amplitude Modulation

signal :: [Double] -> [(Double, Double)]
signal xs 
  = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) 
    | x <- xs ]
    
amplitudeModulation
  = do
      layout_title .= "Amplitude Modulation"
      plot (line "am" [signal [0,(0.5)..400]])
      plot (points "am points" (signal [0,7..400]))

values :: [(String,Double,Bool)]
values = [ ("Mexico City",19.2,False)
         , ("Mumbai",12.9,False)
         , ("Sydney",4.3,False)
         , ("London",8.3,False)
         , ("New York",8.2,True) 
         ]

-- Relative Population

pitem (s,v,o) = pitem_value .~ v
              $ pitem_label .~ s
              $ pitem_offset .~ (if o then 25 else 0)
              $ def

relativePopulation
  = do
      pie_title .= "Relative Population"
      pie_plot . pie_data .= map pitem values

-- MSFT vs APPL

lineStyle n colour = line_width .~ n
                   $ line_color .~ opaque colour
                   $ def

fbetween label color vals = liftEC $ do
  plot_fillbetween_style .= solidFillStyle (withOpacity color 0.4)
  plot_fillbetween_values .= [ (d, (lo,hi)) | (d,(lo,op,cl,hi)) <- vals]
  plot_fillbetween_title .= label

cline label color vals = liftEC $ do
  plot_lines_style .= lineStyle 2 color
  plot_lines_values .= [ [ (d, cl) | (d,(lo,op,cl,hi)) <- vals] ]
  plot_lines_title  .= label

candle label color vals = liftEC $ do
  plot_candle_line_style  .= lineStyle 1 color
  plot_candle_fill .= True
  plot_candle_rise_fill_style .= solidFillStyle (opaque white)
  plot_candle_fall_fill_style .= solidFillStyle (opaque color)
  plot_candle_tick_length .= 0
  plot_candle_width .= 2
  plot_candle_values .= [ Candle d lo op 0 cl hi | (d,(lo,op,cl,hi)) <- vals]
  plot_candle_title .= label

msftVSaapl
  = do
      layoutlr_title .= "Stock Prices"
      layoutlr_left_axis . laxis_override .= axisGridHide
      layoutlr_right_axis . laxis_override .= axisGridHide

      plotLeft (fbetween "AAPL spread" green pricesAAPL)
      plotLeft (cline "AAPL closing" green pricesAAPL)
      plotLeft (candle "AAPL candle" blue pricesAAPL)

      plotRight (fbetween "MSFT spread" purple pricesMSFT)
      plotRight (cline "MSFT closing" purple pricesMSFT)
      plotRight (candle "MSFT candle" red pricesMSFT)

-- Electric Charges

r' x y z        = sqrt $ x^2 + y^2 + z^2
efield sign x y = ( sign*x/r,sign*y/r) where r = r' x y 10
bfield sign x y = (-sign*y/r^2,sign*x/r^2) where r = r' x y 10
square' a s     = [(x,y) | x <- range, y <- range] 
  where range = [-a,-a+s..a] :: [Double]

add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

ef (x,y) = efield 1 (x-20) y `add` efield (-1) (x+20) y
bf (x,y) = bfield 1 (x-20) y `add` bfield (-1) (x+20) y
grid = square' 30 3

vectorField title f grid = fmap plotVectorField $ liftEC $ do
    c <- takeColor  
    plot_vectors_mapf .= f
    plot_vectors_grid .= grid
    plot_vectors_style . vector_line_style . line_color .= c
    plot_vectors_style . vector_head_style . point_color .= c
    plot_vectors_title .= title

electricCharges
  = do
      setColors [opaque black, opaque blue]

      layout_title .= "Positive and Negative Charges"
      plot $ vectorField "Electric Field" ef grid
      plot $ vectorField "B-field" bf grid


-- Support code
-- --  

renderChart :: (Default r, ToRenderable r) 
       => DEnv -> EC r () -> Image PixelRGBA8
renderChart env ec
  = renderDia Rasterific (RasterificOptions (Width width)) $
      fst $ runBackendR env (toRenderable (execEC ec))
  where
    (width, _) = envOutputSize env

-- We need to use a local font.
chartEnv :: IO DEnv
chartEnv = customFontEnv vectorAlignmentFns 640 400 $ fromList
           [ (("sans-serif", FontSlantNormal , FontWeightNormal), "SourceSansPro_R.svg")
           , (("sans-serif", FontSlantNormal , FontWeightBold  ), "SourceSansPro_RB.svg")
           ]
