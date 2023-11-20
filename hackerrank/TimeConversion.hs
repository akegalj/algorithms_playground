solve :: String -> String
solve [h1, h2, ':', m1, m2, ':', s1, s2, t1, t2] = hh ++ ":" ++ mm ++ ":" ++ ss
  where
    isAM = [t1, t2] == "AM"
    isPM = not isAM
    hh = convert $ read [h1, h2]
    mm = [m1, m2]
    ss = [s1, s2]
    padd x
      | x < 10 = "0" ++ show x
      | otherwise = show x
    convert x
      | isAM && x == 12 = "00"
      | isAM = padd x
      | isPM && x == 12 = padd x
      | isPM = padd $ x + 12

main = getLine >>= putStr . solve