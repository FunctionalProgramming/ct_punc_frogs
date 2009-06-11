module Chart (makeChart) where

import Data.List


extractCities = sort . nub . listCities
    where
    listCities zs = [x | (x,_,_)<-zs] ++ [y | (_,y,_)<-zs] 


makeIndexes cities = makeRows 1 [ (x,y) | x<-cities, y<-cities, x > y ]
    where
    makeRows n [] = []
    makeRows n xs = take n xs : makeRows (n+1) (drop n xs)


makeChart :: [ (String,String,Int) ] -> ([String],[[Int]])
makeChart xs = (cities, distances)
    where
    cities = extractCities xs
    distances = (mapDistance . makeIndexes) cities
    
    mapDistance indexes = [ [lookup (x,y) | (x,y)<-row] | row<-indexes ]
    lookup (x,y) = head [d | (c1,c2,d)<-xs, (x,y) == (c1,c2) || (x,y) == (c2,c1)]
     
{-
makeChart [("O", "W", 40), ("M", "O", 10), ("M", "W", 30), 
            ("L", "M", 100), ("L", "W", 70), ("L", "O", 110)]
~~>   
    (["L","M","O","W"], [[100],
                         [110,10],
                         [70,30,40]])

-}
