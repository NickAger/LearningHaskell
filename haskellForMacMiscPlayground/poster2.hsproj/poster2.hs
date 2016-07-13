{-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE TypeFamilies #-}

-- an attempt to try to distill to the simplest form the diagrams
-- simply shows a single box with the doctor login web-services

-- to generate the poster:
-- ghc --make doctorlogin.hs
-- ./doctorlogin. -o doctorlogin..pdf -w 1685 or
-- ./doctorlogin. -o doctorlogin..png -w 10000    (to generate a png with width 10000)
--
-- Then try:
-- ./doctorlogin. -o doctorlogin..pdf -w 500 --loop
-- to understand the code, go to http://projects.haskell.org/diagrams/manual/diagrams-manual.html
--
-- expected format: Din A1: 594 x  841
-- or               Din A0: 841 x 1189 mm  (sqrt 2 / 1)


import Diagrams.Prelude
-- import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.Rasterific
import Data.Word
import Graphics.SVGFonts
import qualified Diagrams.TwoD.Size as Size
import Data.Tree
import Data.Colour hiding (atop)
import Data.Maybe



main = do
  folderImg <- getDataFileName "folder.png"
  filesImg <- getDataFileName "files.png"
  pdp7 <- getDataFileName "pdp7_3.png"
  let images = [folderImg, filesImg, pdp7]
  defaultMain (tlcPoster images)

-- other stuff
getDataFileName :: FilePath -> IO FilePath
getDataFileName name = return ("/Users/nickager/Downloads/unixPoster-master/img" ++ "/" ++ name)


--------------------------------------------------
-- basic building blocks of the diagram
--------------------------------------------------

tlcPoster [folderImg, filesImg, pdp7] = strutY 3 ===
             header # centerXY
             === strutY 4 ===
             poster_portrait # centerXY
             

folderPic f = image f -- 10 10 # centerXY

header = (textLin "Doctor login" 10 black) # alignBR

poster_portrait = (strutX 3 ||| doctorLogin ||| strutX 3 ||| doctorMessaging ||| strutX 3 )

doctorLogin = textBoxWithHeader
  "Doctor login web-services"
  ["vpWebAPI/",
  "      Device/Register",
  "      Api/Version",
  "      Device/CheckVersion",
  "      Authenticate/Limited",
  "      Log/Errors",
  "      Device/Register",
  "      AuthenticateDoctor",
  "      Doctor/SynchroniseDoctorMessages",
  "      Device/SyncLocation/true",
  "      Trust/GetConsultantsWithSpecialities",
  "      PatientLists/GetPatientLists",
  "      Doctor/GetOnDutyLists",
  "      Ward/GetPatients/EMERALD/false",
  "      Patients/HighlightedPatientsForPersion/1272",
  "      Diagnosis/GetDiagnosisSummaryForPatientsInList",
  "      Authenticate/Limited",
  "      Log/Errors",
  "      Log/Errors",
  "      Doctor/SynchroniseDoctorMessages"] 2 white indianred # alignTL

doctorMessaging = textBoxWithHeader
    "Doctor Messaging"
    ["Doctor/",
    "      SynchroniseDoctorMessages ReadMessages, DeletedMessages",
    "      Doctors/RespondToEscalation"] 2 white indianred # alignTL

textLin t h c | null t = mempty
              | otherwise = textSVG_ (TextOpts t lin INSIDE_H HADV False 1 h) # fc c # lc c # fillRule EvenOdd

textBit t h c | null t = mempty
              | otherwise = textSVG_ (TextOpts t bit INSIDE_H HADV False 1 h) # fc c # lc c # fillRule EvenOdd


textBoxWithHeader line lines h c0 c1 = (border 0.3 w hh placedTextLines) `atop` roundedBox
  where
    -- placing textLines below each other
    placedTextLines = ((textBit line h black # centerXY) === (vcat' (with & sep .~ h/5) textLines # centerXY)) # alignTL
    textLines = map (\t -> (textBit t h c0) # alignTL) lines
    roundedBox = fancyRoundedBox w h hh c1
    w = Size.width  (placedTextLines :: D R2)
    hh = Size.height (placedTextLines :: D R2)

-- reduce the size of an object by len in all directions
border hrel w h obj | w > len && h > len = obj # scaleX ((w-len)/w)
                                               # scaleY ((h-len)/h)
                                               # translateX (len/2)
                                               # translateY (-len/2)
                    | otherwise           = obj
  where len = hrel*h

fancyRoundedBox w h hh c = ( (roundedRect (w-0.4-size*0.04) (hh-0.4-size*0.04) (h/5)) # centerXY # lwL 0 # fc (blend 0.5 c white)
                           `atop`
                             (roundedRect w hh (h/5))          # centerXY # lwL 0 # fc (blend 0.5 c black) ) # alignTL # opacity 0.7
  where size | w > hh    = hh
             | otherwise = w
             
