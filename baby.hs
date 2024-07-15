doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y 

doubleSmallNumber x = if x > 100 then x else x*2

boomBang xs = [if x < 10 then "Boom" else "Bang" | x <- xs, odd x]

factorial n = if n > 1 then n * factorial (n - 1) else 1

mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

myMax :: (Ord a) => [a] -> a
myMax [] = error "maximum of an empty list is undefined"
myMax [x] = x
myMax (x:xs) 
    | x > currentMax = x 
    | x <= currentMax = currentMax 
    where currentMax =  myMax xs

myMin x = -(myMax [-t | t <- x])

cylinder_surf_area r h = 
    let area = pi * r^2
        curve = 2 * pi * r * h
    in 2 * area + curve


myRev :: [a] -> [a]
myRev [] = []
myRev (x:xs) = (myRev xs) ++ [x]



quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let ls = [l | l <- xs, l < x]
        rs = [r | r <- xs, r >= x]
    in quicksort ls ++ [x] ++ quicksort rs





