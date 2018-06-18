disemvowel :: String -> String
disemvowel =   filter ( `notElem ` "aeiouAEIOU") 