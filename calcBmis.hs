calcBmis::[(Double, Double)]->[Double]
calcBmis xs = [bmi w h|(w, h)<-xs]
  where bmi weight height = weight/height ^ 2

calcBmis'::[(Double, Double)]->[Double]
calcBmis' xs = [bmi|(w, h)<-xs, let bmi = w / h ^ 2, bmi > 25.0]

cylinder::Double-> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

head'::[a] -> a
head' [] = error "No head for empty lists!"
head' (x:_) = x

--head'::[a] -> a
--head' xs = case xs of [] -> error "No head for empty lists!"
--   (x:_) -> x

describeList::[a]->String
describeList ls = "The list is "
             ++ case ls of [] -> "empty."
                           [x]-> "a singleton list."
                           xs -> "a longer list."

describeList'::[a]->String
describeList' ls = "The list is " ++ what ls
  where what [] = "empty."
        what [x] = "a singleton list."
        what xs = "a longer list."

maximum'::(Ord a) => [a]->a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate'::Int->a->[a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate'(n - 1) x

take'::Int->[a]->[a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n - 1) xs

reverse'::[a]->[a]
reverse'[] = []
reverse'(x:xs) = reverse' xs ++ [x]

zip'::[a]->[b]->[(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

rzip'::[a]->[b]->[(b,a)]
rzip' _ [] = []
rzip' [] _ = []
rzip' (x:xs) (y:ys) = (y,x):rzip' xs ys

elem'::(Eq a)=>a->[a]->Bool
elem' a [] = False
elem' a (x:xs)
     | a == x  = True
     | otherwise = a `elem'` xs

quicksort::(Ord a) => [a]->[a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerOrEqual = [a|a <- xs, a <= x]
      larger = [a|a<-xs, a > x]
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger


applyTwice::(a->a)->a->a
applyTwice f x = f(f x)

map'::(a -> b) -> [a] -> [b]
map' _ [] = []
map' f(x:xs) = f x : map' f xs

