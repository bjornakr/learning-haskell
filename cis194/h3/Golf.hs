module Golf where
    import Data.List



    -- Exercise 1 Hopscotch
    skips :: [a] -> [[a]]
    skips xs = zipWith ($) (map takeEvery [1..]) (replicate (length xs) xs)
        where 
            takeEvery :: Int -> [a] -> [a]
            takeEvery n = map snd . filter (\x -> mod (fst x) n == 0) . zip [1..]



    -- Exercise 2 Local maxima
    localMaxima :: [Integer] -> [Integer]
    localMaxima (x:y:z:zs)
        | y > x && y > z = (reverse . sort) (y:(localMaxima (y:z:zs)))
        | otherwise = (localMaxima (y:z:zs))
    localMaxima _ = []


    
    -- Exercise 3 Histogram
    histogram :: [Integer] -> String
    histogram xs = ((unlines . reverse . printx . frequencies) xs) ++ ("==========\n0123456789")

        where
            frequencies :: [Integer] -> [Integer]
            frequencies xs = map toInteger (reduceBy 1 (map length ((group . sort) (xs ++ [0..9]))))

            printx :: [Integer] -> [String]
            printx xs
                | maximum xs > 0 = (printLine xs):(printx (reduceBy 1 xs))
                | otherwise = []

            printLine :: [Integer] -> String
            printLine = map (\x -> if x > 0 then '*' else ' ')

            reduceBy n xs = map (\x -> x - n) xs


    main = do
        putStrLn $ show $ skips "ABCD"
        putStrLn $ show $ skips "hello!"
        putStrLn $ show $ skips [1]
        putStrLn $ show $ skips [True, False]
