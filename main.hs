import Text.Read (readMaybe)

calculateDotProduct :: [Double] -> [Double] -> Double
calculateDotProduct vector1 vector2 = sum (zipWith (*) vector1 vector2)

populateVector :: [String] -> IO [String]
populateVector vector = do
    putStrLn "Enter a value. Type \"d\" when you are finished populating the vector."
    value <- getLine

    if value == "d"
        then return vector
    else case readMaybe value :: Maybe Double of
        Just _ -> populateVector (vector ++ [value])
        Nothing -> do
            putStrLn "Sorry, the input you provided is invalid. Please try again."
            populateVector vector

main :: IO ()
main = do
    putStrLn "Populate the first vector."
    vector1 <- populateVector []
    putStrLn "Populate the second vector."
    vector2 <- populateVector []

    let convertedVector1 = map read vector1 :: [Double]
    let convertedVector2 = map read vector2 :: [Double]
    let dotProduct = calculateDotProduct convertedVector1 convertedVector2
    putStrLn $ "The dot product of vectors " ++ show vector1 ++ " and " ++ show vector2 ++ " is " ++ show dotProduct ++ "." 
