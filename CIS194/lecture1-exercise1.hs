--  Double the value of every second digit beginning from the right. 
-- That is, the last digit is unchanged; the second-to-last digit is 
-- doubled; the third-to-last digit is unchanged; and so on. 
-- For example, [1,3,8,6] becomes [2,3,16,6].

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) $ cycle [2,1] 


-- Add the digits of the doubled values and the undoubled digits
-- from the original number. 
-- For example, [2,3,16,6] becomes 2+3+1+6+6 = 18.
-- Calculate the remainder when the sum is divided by 10. 
-- For the above example, the remainder would be 8.
-- If the result equals 0, then the number is valid.
-- Example: isValidCardNumber 4012888888881881 = True 
-- Example: isValidCardNumber 4012888888881882 = False
isValidCardNumber :: Integer -> Bool
isValidCardNumber a =
    let as = doubleEveryOther $ toDigits  a 
        digits = concatMap toDigits as 
        cardNumberSum =  sum  digits
    in cardNumberSum `mod` 10 == 0

-- We need to first find the digits of a number. Define the
-- functions
--  toDigits    :: Integer -> [Integer]
--  toDigitsRev :: Integer -> [Integer]
-- toDigits should convert positive Integers to a list of digits. 
-- (For 0 or negative inputs, toDigits should return the empty list.) 
-- toDigitsRev should do the same, but with the digits reversed.
-- Example: toDigits 1234 == [1,2,3,4] 
-- Example: toDigitsRev 1234 == [4,3,2,1] 
-- Example: toDigits 0 == []
-- Example: toDigits (-17) == []

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = if n < 0 
    then [] 
    else n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev


