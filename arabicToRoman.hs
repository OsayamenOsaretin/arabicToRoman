fetchValue :: [Char] -> Int -> String
fetchValue values 4 = (values !! 0) : (values !! 1) : [] 
fetchValue values 9 = (values !! 0) : (values !! 2) : []
fetchValue values 5 = values !! 1 : []
fetchValue values x
  | x < 5 = replicate x (values !! 0)
  | x > 5 = (values !! 1) : (replicate (x-5) (values !! 0))
  | otherwise = ""


convert :: Int -> Int -> String
convert 1000 = (\x -> replicate x 'M') 
convert 1 = fetchValue ['I', 'V', 'X']
convert 10 = fetchValue ['X', 'L', 'C'] 
convert 100 = fetchValue ['C', 'D', 'M']


converter :: Int -> Int -> String
converter _ 0 = "" 
converter divisor number = (convert divisor quotient) ++ (converter (quot divisor 10) remainder)
    where (quotient, remainder) = quotRem number divisor


main = do 
    putStrLn "Enter a number to convert to Arabic"
    number <- getLine
    putStrLn (converter 1000 $ read number)

