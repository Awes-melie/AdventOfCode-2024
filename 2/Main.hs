module Main where
    main :: IO ()
    main = do fileContents <- readFile "numbers.txt"
              let safe = (countSafe . convertToLists) fileContents
              print safe

    main2 :: IO ()
    main2 = do fileContents <- readFile "numbers.txt"
               let safe = (countSafeDampened . convertToLists) fileContents
               print safe

    -- so much more concise than day one oh my godd, idk what i was cooking but i'm locked back in
    convertToLists :: String -> [[Int]]
    convertToLists list = numbers
        where
            lineList = lines list
            wordList = map words lineList
            numbers = map (map (\y -> read y ::Int)) wordList

    countSafe :: [[Int]] -> Int
    countSafe = length . filter isSafe

    countSafeDampened :: [[Int]] -> Int
    countSafeDampened = length . filter isSafeDampened

    isSafeDampened :: [Int] -> Bool
    isSafeDampened list = any isSafe [ removeFrom i list | i <- [0 .. length list] ]
        where
            removeFrom :: Int -> [Int] -> [Int]
            removeFrom i (s:sx)
                | i == 0 = sx
                | otherwise = s : removeFrom (i - 1) sx

    isSafe :: [Int] -> Bool
    isSafe (a:b:sx)
        | isSteadilyChanging (>) a b = isAlwaysDecreasing (b:sx)
        | isSteadilyChanging (<) a b = isAlwaysIncreasing (b:sx)
        | otherwise = False

    isAlwaysDecreasing = isDirectionConsistent (>)
    isAlwaysIncreasing = isDirectionConsistent (<)

    isDirectionConsistent :: (Int -> Int -> Bool) -> [Int] -> Bool
    isDirectionConsistent _ [] = False
    isDirectionConsistent _ [_] = True
    isDirectionConsistent p [higher,lower] = isSteadilyChanging p higher lower
    isDirectionConsistent p (higher:lower:rest)
        = isSteadilyChanging p higher lower && isDirectionConsistent p (lower:rest)

    isSteadilyChanging :: (Int -> Int -> Bool) -> Int -> Int -> Bool
    isSteadilyChanging p x y = p x y && abs (x - y) <=3 && x /= y