module PhoneExercise where
  

data PhoneButton = PhoneButton { button :: Char    , chars :: [Char] }


data DaPhone = DaPhone {buttons :: [PhoneButton]}

keypad = DaPhone [PhoneButton '1' "", PhoneButton '2' "ABC", PhoneButton '3' "DEF", PhoneButton '4' "GHI", PhoneButton '5' "JKL", PhoneButton '6' "MNO", PhoneButton '7' "PQRS", PhoneButton '8' "TUV", PhoneButton '9' "WXYZ", PhoneButton '*' "^", PhoneButton '0' "+ ", PhoneButton '#' ".," ]


