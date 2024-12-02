module Main where
    main :: IO ()
    main = do fileContents <- readFile "numbers.txt"
              let distance = (findDistance . convertToLists) fileContents
              print distance

    main2 :: IO ()
    main2 = do fileContents <- readFile "numbers.txt"
               let distance = (findSimilarity . convertToLists) fileContents
               print distance

    convertToLists :: String -> ([Int],[Int])
    convertToLists list = zipMap pairs
        where
            lines = wordsWhen (=='\n') list
            pairs = map (splitStringPair ' ') lines
            zipMap :: [(String, String)] -> ([Int], [Int])
            zipMap [] = ([0], [0])
            zipMap list = (readLeft list, readRight list)
                where
                    readLeft :: [(String, String)] -> [Int]
                    readLeft [] = []
                    readLeft (x:xs) = (read (fst x) :: Int) : readLeft xs

                    readRight :: [(String, String)] -> [Int]
                    readRight [] = []
                    readRight (x:xs) = (read (snd x) :: Int) : readRight xs

    splitString :: Char -> String -> [String]
    splitString _ "" = []
    splitString char str
        | current_char == char = [] : rest
        | otherwise            = (current_char:[r]):[rx]
        where
            current_char = head str
            rest@([r]:[rx]) = splitString char (tail str)

    splitStringPair :: Char -> String -> (String, String)
    splitStringPair _ "" = ("","")
    splitStringPair char str = splitString' char ("", str)
        where
            splitString' :: Char -> (String, String) -> (String,String)
            splitString' char (pre, []) = (pre, [])
            splitString' char (pre, x:c:xs)
                | c == char = (pre ++ [x], xs)
                | otherwise = splitString' char (pre ++ [x], c:xs)

    findDistance :: ([Int],[Int]) -> Int
    findDistance list = sum (diffZip sorted)
        where
            sorted = (sort (fst list), sort (snd list))
            diffZip :: ([Int],[Int]) -> [Int]
            diffZip (listA, listB) = [abs (x - y) | (x,y) <- zip listA listB]

    -- breaks if theres a 0 loll but im not doing allthat
    findSimilarity :: ([Int],[Int]) -> Int
    findSimilarity ([], _) = 0
    findSimilarity (l:ls,right) = countLs l right + findSimilarity (ls, right)
        
    countLs :: Int -> [Int] -> Int
    countLs _ [] = 0
    countLs i (x:xs)
        | x == i = i + countLs i xs
        | otherwise = countLs i xs

--- PRELUDE, not working for some reason --
    sort = sortBy compare
    sortBy cmp = mergeAll . sequences
        where
            sequences (a:b:xs)
                | a `cmp` b == GT = descending b [a]  xs
                | otherwise       = ascending  b (a:) xs
            sequences xs = [xs]

            descending a as (b:bs)
                | a `cmp` b == GT = descending b (a:as) bs
            descending a as bs  = (a:as): sequences bs

            ascending a as (b:bs)
                | a `cmp` b /= GT = ascending b (\ys -> as (a:ys)) bs
            ascending a as bs   = let !x = as [a]
                                in x : sequences bs

            mergeAll [x] = x
            mergeAll xs  = mergeAll (mergePairs xs)

            mergePairs (a:b:xs) = let !x = merge a b
                                in x : mergePairs xs
            mergePairs xs       = xs

            merge as@(a:as') bs@(b:bs')
                | a `cmp` b == GT = b:merge as  bs'
                | otherwise       = a:merge as' bs
            merge [] bs         = bs
            merge as []         = as

    wordsWhen :: (Char -> Bool) -> String -> [String]
    wordsWhen p s =
        case dropWhile p s of
            "" -> []
            s' -> w : wordsWhen p s''
                where (w, s'') = break p s'

