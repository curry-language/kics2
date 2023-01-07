-- Concatenating two lists:
-- (predefined as `++' in the standard prelude)

append :: [t] -> [t] -> [t]

append []     x  = x
append (x:xs) ys = x : append xs ys


-- Reverse the order of elements in a list:

rev :: [t] -> [t]

rev []     = []
rev (x:xs) = append (rev xs) [x]


goal1 :: [Int]
goal1 = append [1,2] [3,4]

goal2 :: [Int]
goal2 = rev [1,2,3,4]

goal3 :: [Int]
goal3 = rev [1,2,3,4,5,6,7,8,9,10]


-- end of program
