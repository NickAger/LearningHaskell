module StateMonadExample where
  
import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  :: (Bool, StdGen)
        (secondCoin, newGen') = random newGen  :: (Bool, StdGen)
        (thirdCoin, newGen'') = random newGen'  :: (Bool, StdGen)
    in  (firstCoin, secondCoin, thirdCoin)  