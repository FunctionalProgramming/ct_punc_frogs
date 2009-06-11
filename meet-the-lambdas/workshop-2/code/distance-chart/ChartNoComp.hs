--
-- This module is functionally equivalent to the Chart module, but it has
-- been re-implemented without list comprehensions.
-- In fact, each comprehension in the Chart module has been replaced with
-- a suitable combination of mapping and filtering functions to obtain an
-- equivalent definition.  Note that in the case of multiple generators,
-- we have to throw concat in the mix as well.  (See makeIndexes.)
--
module ChartNoComp (makeChart) where

import Data.List



extractCities :: [(String,String,Int)] -> [String]
extractCities = sort . nub . listCities
    where
    listCities zs = mapFst zs ++ mapSnd zs 

mapFst    []  = []
mapFst (x:xs) = fst x : mapFst xs
    where
    fst (x,_,_) = x
    
mapSnd    []  = []
mapSnd (x:xs) = snd x : mapSnd xs
    where
    snd (_,y,_) = y



makeIndexes :: [String] -> [[(String,String)]]
makeIndexes cities = makeRows 1 flatIndexes
    where
    
    makeRows n [] = []
    makeRows n xs = take n xs : makeRows (n+1) (drop n xs)
    
    flatIndexes = concat (mapIndex cities)
    
    mapIndex    []  = []
    mapIndex (x:xs) = index x : mapIndex xs
    
    index x = mapPair x (filterGt x cities)
    
mapPair x    []  = []
mapPair x (y:ys) = (x,y) : mapPair x ys

filterGt x    []  = []
filterGt x (y:ys)
    | x > y       = y : filterGt x ys
    | otherwise   =     filterGt x ys



makeChart :: [ (String,String,Int) ] -> ([String],[[Int]])
makeChart xs = (cities, distances)
    where
    cities = extractCities xs
    distances = (mapDistance . makeIndexes) cities
    
    mapRow    []  = []
    mapRow (p:ps) = lookup' p xs : mapRow ps                -- mapping lookup'

    mapDistance    []  = []
    mapDistance (r:rs) = mapRow r : mapDistance rs          -- mapping mapRow 

lookup' (x,y) distanceData = (head . mapTrd . filterSameRoute) distanceData
    where
          
    sameRoute (c1,c2,_) = (x,y) == (c1,c2) || (x,y) == (c2,c1)
    
    filterSameRoute    []  = []
    filterSameRoute (t:ts) 
        | sameRoute t      = t : filterSameRoute ts
        | otherwise        =     filterSameRoute ts
        
mapTrd    []  = []
mapTrd (t:ts) = trd t : mapTrd ts
    where
    trd (_,_,d) = d

 

{-
makeChart [("O", "W", 40), ("M", "O", 10), ("M", "W", 30), 
            ("L", "M", 100), ("L", "W", 70), ("L", "O", 110)]
~~>   
    (["L","M","O","W"], [[100],
                         [110,10],
                         [70,30,40]])

-}
