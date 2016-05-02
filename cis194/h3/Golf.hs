module Golf where
    import Data.List

    skips :: [a] -> [[a]]
    skips xs = loop 1 xs
        where
            loop :: Int -> [a] -> [[a]]
            loop _ [] = []
            loop skp (x:xs) = (takeEvery skp (x:xs)):(loop (skp+1) xs)

            takeEvery :: Int -> [a] -> [a]
            takeEvery _ [] = []
            takeEvery n xs@(x:_) = x:(takeEvery n (drop n (xs)))

    
    localMaxima :: [Integer] -> [Integer]
    localMaxima xs = loop (-1) xs
        where
            loop :: Integer -> [Integer] -> [Integer]
            loop _ [] = []
            loop lst (x:[]) = []
            loop lst (x:y:ys)
                | x > lst && x > y = x:(loop x (y:ys))
                | otherwise = (loop x (y:ys))

    histogram :: [Integer] -> String
    histogram xs = ((unlines . reverse . printx . frequencies) xs) ++ ("==========\n0123456789")

        where
            frequencies :: [Integer] -> [Integer]
            frequencies xs = map toInteger (reduceByOne (map length ((group . sort) (xs ++ [0..9]))))

            printx :: [Integer] -> [String]
            printx xs
                | maximum xs <= 0 = []
                | otherwise = (printLine xs):(printx (reduceByOne xs))

            printLine :: [Integer] -> String
            printLine [] = []
            printLine (x:xs)                
                | x > 0 = '*':(printLine xs)
                | otherwise = ' ':(printLine xs)

            reduceByOne xs = map (\x -> x -1) xs


    main = do
        putStrLn $ show $ skips "ABCD"
        putStrLn $ show $ skips "hello!"
        putStrLn $ show $ skips [1]
        putStrLn $ show $ skips [True, False]
        --putStrLn $ show $ skips []