-- Dummy Solutions to COMP2209 Coursework 1 Exercises
-- Please take these dummy functions and implement them as specified
-- To test this with the supplied test harness, rename to Exercises.hs
-- Submit your code using your tested version of this file
--
-- NOTE THAT NO EXTERNAL MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION SIGNATURES NOR TYPE DEFINITIONS

-- This module statement makes public only the specified functions and types
-- please do not change these lines either
module Exercises (splitSort, longestCommonSubList,
    ModuleResult (ModuleResult), canProgress, DegreeClass (First, UpperSecond,
    LowerSecond, Third), classify, hillClimb, nearestRoot, Instruction (Duplicate,
    Pop, Add, Multiply), executeInstructionSequence, optimalSequence,
    findBusyBeavers, Rectangle (Rectangle), simplifyRectangleList, drawEllipse,
    extractMessage, differentStream, unPairAndApply, isShellTreeSum) where

-- Exercise 1
-- split a given list into sub-lists
-- each of these must be strictly ascending, descending, or equal
splitSort:: Ord a => [a]->[[a]]
splitSort []         = []
splitSort [x]        = [[x]]
splitSort (x:y:xs)   = (x:y:map snd current) : splitSort (map snd rest)
  where
    (current,rest) = span ((==operator) . uncurry compare) (zip (y:xs) xs)
    operator       = compare x y -- find the comparison operator between two elements

-- Exercise 2
-- longest common sub-list of a finite list of finite list
longestCommonSubList :: Eq a => [[a]] -> [a]
longestCommonSubList []       = []
longestCommonSubList [x]      = x
longestCommonSubList (x:y:xs) = longestCommonSubList (compareLists x y : xs)

-- Finds the longest common sublist of two lists
compareLists :: Eq a => [a] -> [a] -> [a]
compareLists [] _ = []
compareLists _ [] = []
compareLists (x:xs) (y:ys) | x == y    = x : compareLists xs ys
                           | otherwise = longest (compareLists (x:xs) ys) (compareLists xs (y:ys))
                           where
                             longest a b | length a > length b = a
                                         | otherwise = b

-- Exercise 3
-- check whether the given results are sufficient to pass the year
-- and progress using the University of Southampton Calendar regulations
data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show
data ModuleOutcome = Pass | Qualify | Fail deriving (Eq, Show)
canProgress :: [ModuleResult] -> Bool
canProgress xs | passCredits >= 60 && qualifyCredits == 0 && failCredits == 0 = True -- Pass
               | average >= 40 && passCredits + qualifyCredits >= 60
                  && qualifyCredits <= 15 && failCredits == 0 = True -- Pass by compensation
               | otherwise = False
               where
  results x = [markCheck m | m <- x]
  average = yearAverage xs
  passCredits = sumResult Pass (results xs)
  qualifyCredits = sumResult Qualify (results xs)
  failCredits = sumResult Fail (results xs)

markCheck :: ModuleResult -> (Float, ModuleOutcome)
markCheck mr | mark mr >= 40 = (credit mr, Pass)
             | mark mr >= 25 = (credit mr, Qualify)
             | mark mr < 25 = (credit mr, Fail)
             | otherwise = error "No module result given"

sumResult :: ModuleOutcome -> [(Float, ModuleOutcome)] -> Float
sumResult _ [] = 0
sumResult r (x:xs) | snd x == r = fst x + sumResult r xs
                   | otherwise = sumResult r xs

-- Exercise 4
-- compute the degree classification associate with 3 or 4 year's worth of results
-- using the regulations given in the University of Southampton Calendar
data DegreeClass = First | UpperSecond | LowerSecond | Third deriving (Eq, Show)
classify :: [[ModuleResult]] -> DegreeClass
classify ms | length ms == 4 = getClass (checkUpgrade ((yearAverage (ms!!1) * 0.2) + (yearAverage (ms!!2) * 0.4) + (yearAverage (ms!!3) * 0.4)) ms)
            | length ms == 3 = getClass (checkUpgrade ((yearAverage (ms!!1) * 0.25) + (yearAverage (ms!!2) * 0.75)) ms)
            | otherwise = error "Invalid results provided"

getClass :: Int -> DegreeClass
getClass x | x >= 70 = First
           | x >= 60 = UpperSecond
           | x >= 50 = LowerSecond
           | x >= 40 = Third
           | otherwise = error "Unable to classify, the student has failed"

yearAverage :: [ModuleResult] -> Float
yearAverage mr = numerator mr / denominator mr where
  numerator = foldr (\ x -> (+) (fromIntegral (mark x) * credit x)) 0
  denominator = foldr ((+) . credit) 0

-- Check if the classification can be upgraded (it is within 2 marks of the higher classification and 50% of the credit points,
-- weighted by Part, are derived from Module Marks in the higher class or above)
checkUpgrade :: Float -> [[ModuleResult]] -> Int
checkUpgrade x ms | boundary > 0 && overallPercentFromHigher boundary ms > 50 = round boundary
                  | otherwise = round x
  where
    boundary = upgradeBoundary x

upgradeBoundary :: Float -> Float
upgradeBoundary x | x >= 38 && x < 40 = 40
                  | x >= 48 && x < 50 = 50
                  | x >= 58 && x < 60 = 60
                  | x >= 68 && x < 70 = 70
                  | otherwise = 0

overallPercentFromHigher :: Float -> [[ModuleResult]] -> Float
overallPercentFromHigher x ms | length ms == 4 = (percentFromHigher x (ms!!1) * 0.2) + (percentFromHigher x (ms!!2) * 0.4) + (percentFromHigher x (ms!!3) * 0.4)
                              | length ms == 3 = (percentFromHigher x (ms!!1) * 0.25) + (percentFromHigher x (ms!!2) * 0.75)
                              | otherwise = error "Invalid results or mark boundary provided"

percentFromHigher :: Float -> [ModuleResult] -> Float
percentFromHigher x ys = (creditsAbove x ys / allCredits ys) * 100
  where
    allCredits = foldr ((+) . credit) 0

creditsAbove :: Float -> [ModuleResult] -> Float
creditsAbove _ [] = 0
creditsAbove x (y:ys) | score >= x = credit y + creditsAbove x ys
                      | otherwise = creditsAbove x ys
                      where
                        score = fromIntegral (mark y)

-- Exercise 5
-- search for the local maximum of f nearest x using an
-- approximation margin delta and initial step value s
hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb f a b eps | abs (c - d) > eps && (f c > f d)   = hillClimb f a d eps
                    | abs (c - d) > eps && (f c < f d)   = hillClimb f c b eps
                    | otherwise                          = (b + a) / 2
                    where
                      gr = (sqrt 5 + 1) / 2
                      c = b - (b - a) / gr
                      d = a + (b - a) / gr

-- Exercise 6
nearestRoot :: [Float] -> Float -> Float -> Float -> Float
nearestRoot xs = hillClimb (\x -> -(sum (zipWith (\j k -> j*x**k) xs [0..])^2))

-- Exercise 7
data Instruction = Add | Subtract | Multiply | Duplicate | Pop deriving (Eq, Show)
executeInstructionSequence :: [Int] -> [Instruction] -> [Int]
executeInstructionSequence ns []  = ns
executeInstructionSequence ns (x:xs)
  | x == Add = executeInstructionSequence (sum (take 2 ns) : drop 2 ns) xs
  | x == Multiply = executeInstructionSequence (product (take 2 ns) : drop 2 ns) xs
  | x == Duplicate = executeInstructionSequence (head ns : ns) xs
  | x == Pop = executeInstructionSequence (tail ns) xs
  | otherwise = error "Invalid instructions or stack configuration given"

-- Exercise 8
optimalSequence :: Int -> [Instruction]
optimalSequence n | n == 1 = []
                  | n == 2 = [Duplicate, Multiply]
                  | mod n 3 == 0 = [Duplicate, Duplicate, Multiply, Multiply] ++ optimalSequence (n `div` 3)
                  | even n = [Duplicate, Multiply] ++ optimalSequence (n `div` 2)
                  | odd n = [Duplicate] ++ optimalSequence (n - 1) ++ [Multiply]
                  | otherwise = error "Cannot produce instruction pattern"

-- Exercise 9
findBusyBeavers :: [Int] -> [[Instruction]]
findBusyBeavers s = bestSequences
  where
    sequences = allValues s (allSequences (length s - 1))
    bestValue = foldl (\ acc x -> acc `max` snd x) 0 sequences
    bestSequences = [ fst x | x <- sequences, snd x == bestValue ]

allValues :: [Int] -> [[Instruction]] -> [([Instruction], Int)]
allValues s xs = [ (x, y) | x <- xs, y <- executeInstructionSequence s x ]

allSequences :: Int -> [[Instruction]]
allSequences n = mapM (const [Pop, Add, Multiply]) [1..n]

-- Exercise 10
data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)
simplifyRectangleList :: [Rectangle] -> [Rectangle]
simplifyRectangleList xs = lineCheck (overlapCheck (insideCheck (removeNonRectangles xs)))

-- Removes any rectangles with invalid coordinates (factors for lines)
removeNonRectangles :: [Rectangle] -> [Rectangle]
removeNonRectangles [] = []
removeNonRectangles (x:xs) | validRectangle x = x : removeNonRectangles xs
                           | otherwise = removeNonRectangles xs
                          where
                            validRectangle :: Rectangle -> Bool
                            validRectangle (Rectangle (a,b) (c,d)) | (a <= c) && (b <= d) = True
                                                                   | otherwise = False

-- Returns every combination of rectangles for comparison
allRectanglePairs :: [Rectangle] -> [(Rectangle, Rectangle)]
allRectanglePairs xs = [ (x, y) | x <- xs, y <- xs, x /= y]

----------------- INSIDE TEST -----------------
-- Removes any rectanlges that are inside others
insideCheck :: [Rectangle] -> [Rectangle]
insideCheck xs = [ x | x <- xs, x `notElem` firstElemList (insideCompare (allRectanglePairs xs))]
  where
    firstElemList []         = []
    firstElemList ((x,_):ys) = x : firstElemList ys

-- Returns pairs of rectangles where a is inside b
insideCompare :: [(Rectangle, Rectangle)] -> [(Rectangle, Rectangle)]
insideCompare [] = []
insideCompare (x:xs) | a `inside` b = x : insideCompare xs
                     | otherwise = insideCompare xs
  where
    a = allPoints (fst x)
    b = allPoints (snd x)

-- True if all the points of shape a are inside shape b
inside :: [(Int, Int)] -> [(Int, Int)] -> Bool
inside a b | matchingPoints a b == a = True
           | otherwise = False
           where
             matchingPoints :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
             matchingPoints xs ys = [x | x <- xs, x `elem` ys]

-- Generate all points inside a rectangle
allPoints :: Rectangle -> [(Int, Int)]
allPoints (Rectangle (a,b) (c,d)) = [ (x, y) | x <- [a..c], y <- [b..d] ]

----------------- SHARED LINE AND OVERLAP TEST -----------------
-- Loop until there are no more rectangles we can merge with lines touching
lineCheck :: [Rectangle] -> [Rectangle]
lineCheck xs | not (null matchingPair) = lineCheck (removeAndMerge (head matchingPair) xs)
             | otherwise = xs
             where
               matchingPair = lineCompare(allRectanglePairs xs)

-- Loop until there are no more rectangles we can merge that are overlapping
overlapCheck :: [Rectangle] -> [Rectangle]
overlapCheck xs | not (null overlapPair) = overlapCheck (removeAndMerge (head overlapPair) xs)
                | otherwise = xs
                where
                  overlapPair = overlapCompare(allRectanglePairs xs)

-- Remove the two rectangles from the list and append the new merged rectangle
removeAndMerge :: (Rectangle, Rectangle) -> [Rectangle] -> [Rectangle]
removeAndMerge (a,b) xs = [ y | y <- xs, y /= a, y /= b ] ++ [merged]
  where
    merged = mergeRectangle (a,b)

-- Return the resulting rectangle from merging the two rectangles passed
mergeRectangle :: (Rectangle, Rectangle) -> Rectangle
mergeRectangle (Rectangle (a,b) (c,d), Rectangle (e,f) (g,h)) = Rectangle (x1,y1) (x2,y2)
  where
    x1 = minimum [a, c, e, g]
    y1 = minimum [b, d, f, h]
    x2 = maximum [a, c, e, g]
    y2 = maximum [b, d, f, h]

-- Return a list of the rectangles that are overlapping
overlapCompare :: [(Rectangle, Rectangle)] -> [(Rectangle, Rectangle)]
overlapCompare xs = [ y | y <- xs, overlapping y ]

-- Returns true if a rectangle is overlapping another in a way that can be merged
overlapping :: (Rectangle, Rectangle) -> Bool
overlapping (Rectangle (a,b) (c,d), Rectangle (e,f) (g,h)) | d == h && b == f && e < c = True
                                                           | e == a && g == c && b < h = True
                                                           | otherwise = False

-- Return a list of rectanlges with matching edges
lineCompare :: [(Rectangle, Rectangle)] -> [(Rectangle, Rectangle)]
lineCompare xs = [ y | y <- xs, shapeTouching y ]

-- Returns true if a rectangle is up against another (can be merged)
shapeTouching :: (Rectangle, Rectangle) -> Bool
shapeTouching (x,y) | not (null (matchingLines a b)) = True
                    | otherwise = False
  where
    a = allLines x
    b = allLines y
    matchingLines :: [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
    matchingLines xs ys = [z | z <- xs, z `elem` ys]

-- Returns an array of pairs of tuples representing the edges of a rectangle, or a signle line if the rectangle is flat
allLines :: Rectangle -> [((Int, Int), (Int, Int))]
allLines (Rectangle (a,b) (c,d)) | a == c || b == d = [((a,b),(c,d))]
                                 | otherwise = [((a,b),(a,d)), ((a,b),(c,b)), ((c,d),(a,d)), ((c,d),(c,b))]



-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image
drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
drawEllipse xC yC a b = simplifyRectangleList (generateAllRectangles xC yC a b (allEllipsePoints xC yC a b))

-- Generates all the possible rectangles contained within the ellipse
generateAllRectangles :: Float -> Float -> Float -> Float -> [(Int, Int)] -> [Rectangle]
generateAllRectangles xC yC a b xs = checkInEllipse xC yC a b (removeNonRectangles [ Rectangle x y | x <- xs, y <- xs ])

-- Checks the entire rectangle is inside the ellipse
checkInEllipse :: Float -> Float -> Float -> Float -> [Rectangle] -> [Rectangle]
checkInEllipse _ _ _ _ [] = []
checkInEllipse xC yC a b (x:xs) | isInside xC yC a b x = x : checkInEllipse xC yC a b xs
                                | otherwise = checkInEllipse xC yC a b xs

-- Returns true if the rectangle is fully contained within the ellipse
isInside :: Float -> Float -> Float -> Float -> Rectangle -> Bool
isInside xC yC a b (Rectangle (c,d) (e,f)) | ((fromIntegral c - xC)^2 / a^2) + ((fromIntegral f - yC)^2 / b^2) <= 1 && ((fromIntegral e - xC)^2 / a^2) + ((fromIntegral d - yC)^2 / b^2) <= 1 = True
                                           | otherwise = False

-- Generates all the integer points inside the ellipse (including the centre)
allEllipsePoints :: Float -> Float -> Float -> Float -> [(Int, Int)]
allEllipsePoints xC yC a b | a < 1 || b < 1 = points ++ centre
                           | otherwise = points
  where
    points = [ (round x, round y) | x <- [(xC-a)..(xC+a)], y <- [(yC-b)..(yC+b)], ((x - xC)^2 / a^2) + ((y - yC)^2 / b^2) <= 1 ]
    centre = [(round xC, round yC)]



-- Exercise 12
-- extract a message hidden using a simple steganography technique
extractMessage :: String -> String
extractMessage xs = decode [x | x <- xs, x `elem` "01"] where
  decode :: String -> String
  decode [] = []
  decode [_] = error "String is too short to contain message"
  decode (x:y:xs') | x:[y] == ['0', '0'] = "a" ++ decode xs'
                   | x:[y] == ['0', '1'] = "b" ++ decode xs'
                   | x:[y] == ['1', '0'] = "c" ++ decode xs'
                   | x:[y] == ['1', '1'] = "d" ++ decode xs'
                   | otherwise = error "Unrecognised pattern"

-- Exercise 13
-- return a stream which is different from all streams of the given stream
-- you may choose to use Cantor's diagonal method
differentStream :: [[Int]] -> [Int]
differentStream = cantors 0

cantors :: Int -> [[Int]] -> [Int]
cantors _ [] = []
cantors n xs | element == 0 = 1 : cantors (n + 1) (tail xs)
             | otherwise = 0 : cantors (n + 1) (tail xs)
  where
    element = head xs !! n

-- Exercise 14
-- extract both components from a square shell pair and apply the (curried) function
unPairAndApply :: Int -> (Int -> Int -> a) -> a
unPairAndApply n f = uncurry f (unpair n)

unpair :: Int -> (Int, Int)
unpair z | z - m^2 < m = (z-m^2, m)
           | otherwise = (m, m^2 + 2 * m - z)
            where
              m = floor (sqrt (fromIntegral z))

-- Exercise 15
isShellTreeSum :: Int -> Bool
isShellTreeSum n = snd unpaired == sumTree (fst unpaired)
  where
    unpaired = unpair n

sumTree :: Int -> Int
sumTree 0 = 0
sumTree 1 = 1
sumTree n = fst unpaired + sumTree (snd unpaired)
  where
    unpaired = unpair n
