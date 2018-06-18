getMiddle :: String -> String
getMiddle s 
  |mod  (length s) 2 == 0 = [s !! (a-1)]++[s !! a] 
  |mod  (length s) 2 == 1 = [s !! a]
     where a = div (length s) 2