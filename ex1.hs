import Data.List

add a b = a + b

lastButOne :: [a] -> Maybe a
lastButOne xs = if (null xs || length(xs) < 2)
                then Nothing 
                else if (length(xs) == 2) then Just(head(xs)) else lastButOne(tail xs)

myLen :: [a] -> Int
myLen xs = foldr (\ _ n -> n+1) 0 xs

myDrop n xs = if n <=0 || null xs
              then xs
              else myDrop (n-1) (tail xs)

myMean :: [Int] -> Float
myMean xs = fromIntegral(foldr (+) 0 xs) / fromIntegral(myLen xs)

makePalindrome :: [a] -> [a]
makePalindrome xs = xs ++ reverse(xs)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = if null(xs)
                  then True
                  else head(xs) == last(xs) && isPalindrome(tail(init(xs)))

mySort :: [[a]] -> [[a]]
mySort xs = sortBy(\a b -> (compare (length a) (length b))) xs

-- simpelest solution
myIntersperse :: a -> [[a]] -> [a]
myIntersperse d xs = if null xs || length(xs) == 0 
                     then []
                     else if length(xs) == 1
                          then head(xs)
                          else head(xs) ++ [d] ++ myIntersperse d (tail xs)

-- use pattern matching
myIntersperseVOne :: a -> [[a]] -> [a] -- type
myIntersperseVOne _ [] = [] -- term
myIntersperseVOne _ (x:[]) = x
myIntersperseVOne sep (x:xs) = x ++ [sep] ++ myIntersperse sep xs

-- binary tree
data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show)

treeHight Empty = 0
treeHight (Node _ left right) = if treeHight(left) > treeHight(right)
                                then 1 + treeHight(left)
                                else 1 + treeHight(right)

data Direction = DLeft 
               | DRight 
               | DLine 
               deriving (Show)
data Point = Pt {pointx,pointy :: Float }
getDirection :: Point -> Point -> Point -> Direction
getDirection p1 p2 p3 = if n == 0
                        then DLine
                        else if n > 0
                             then DRight
                             else DLeft 
           where a = atan2 (pointy(p3) - pointy(p1)) (pointx(p3) - pointx(p1))
                 b = atan2 (pointy(p2) - pointy(p1)) (pointx(p2) - pointx(p1))
                 n = a - b

getDirections :: [Point] -> [Direction]
getDirections (a:b:c:xs) = (getDirection a b c):(getDirections (b:c:xs))
getDirections _ = []
