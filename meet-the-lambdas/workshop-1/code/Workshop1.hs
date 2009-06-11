--
-- Source code for the program discussed in Workshop I.
-- Short comments are given below, for further details refer to the workshop
-- notes.  To run the program, open a command shell and cd to the directory
-- containing this file, then run the following command: 'runghc Workshop1.hs'.
-- Obviously, this assumes you have already installed GHC and runghc is in
-- your path.
--
module Workshop1 where

--
-- Enumerations to represent the Person and Color data our functions will
-- operate on.
--
data Person = Joe | Tom | Sally
data Color  = Black | Green | Orange | Yellow | Blue | White

-- 
-- Tells which Person likes functional programming by enumerating all the
-- associations.
--
likesFP :: Person -> Bool
likesFP Joe   = True
likesFP Tom   = False
likesFP Sally = True

-- 
-- Associates an eye Color to each Person.
--
eyeColor :: Person -> Color
eyeColor Joe   = Black
eyeColor Tom   = Green
eyeColor Sally = Green

--
-- Is the Color at hand Green?  The underscore character is used as a wildcard
-- to match any Color other than Green.
--
isGreen :: Color -> Bool
isGreen Green = True
isGreen   _   = False

--
-- Applies eyeColor to each Person in the input list to figure out their eye
-- Color.  Input values are processed recursively by means of successive 
-- rewrites.
--
mapEyeColor :: [Person] -> [Color]
mapEyeColor [ ]    = [ ]
mapEyeColor (x:xs) = eyeColor x : mapEyeColor xs
    
--
-- Applies isGreen to each Color in the input list to figure out whether or
-- not the Color is Green.  Input values are processed recursively by means 
-- of successive rewrites.
--
mapIsGreen :: [Color] -> [Bool]
mapIsGreen [ ]    = [ ]
mapIsGreen (x:xs) = isGreen x : mapIsGreen xs

--
-- Folds the || operator into the input list so that all values are reduced
-- to a single Bool, which is True if and only if the input list contains at
-- least one True value.  Input values are processed recursively by means 
-- of successive rewrites.
--
foldOr [ ]    = False
foldOr (x:xs) = x || foldOr xs

--
-- Function pipeline which given a Person list tells you whether at least one
-- person has green eyes.  Functions are chained together by means of function
-- composition.
--
someoneGreenEyed :: [Person] -> Bool
someoneGreenEyed = foldOr . mapIsGreen . mapEyeColor

--
-- Filters out of the input list those Person's who don't like functional
-- programming, according to the likesFP function.  Input values are processed
-- recursively by means of successive rewrites and a guard condition is used
-- to decide which route to take at each rewrite step.
--
filterLikesFP [ ]    = [ ]
filterLikesFP (x:xs)
    | likesFP x = x : filterLikesFP xs 
    | otherwise =     filterLikesFP xs


--
-- Trying out all the above on a list of Person values.
-- We want to find out whether someone in the list likes functional programming
-- and is green eyed.  We get the answer by chaining together (using function
-- composition) the filterLikesFP and someoneGreenEyed functions.  The main
-- function is the program entry point and prints the answer to stdout, after
-- converting it to a string.
--
ppl = [Joe, Tom, Sally]
answer = (someoneGreenEyed . filterLikesFP) ppl
main = putStr (show answer ++ "\n")
