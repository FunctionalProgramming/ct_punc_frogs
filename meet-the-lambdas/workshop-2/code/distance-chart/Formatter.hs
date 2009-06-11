--
-- This module provides the functionality to turn the chart data into a 
-- printable string.
-- Note that we handle "labels" and "data rows", i.e. this module is ignorant
-- of the actual meaning of its inputs (i.e. cities and distances);  all it 
-- does is pretty-printing the inputs using a triangular chart format. 
-- (Information Hiding.)
--
module Formatter (formatChart) where

--
-- Produces a printable string out of the chart data.
-- This is done in three steps.  First, format all the data rows into lines.
-- Second, add an empty line to the front of the lines list and then append, 
-- in turn, each label to each line string to produce the chart lines.
-- Third, concatenate all the chart lines into a single string.   
--
formatChart :: ([String],[[Int]]) -> String
formatChart (labels, dataRows) = "\n" ++ concat chartLines ++ "\n"
    where
    chartLines = zipWithLine labels ([] : format dataRows) 

--
-- Builds a list by applying the line function, in turn, to each label and data
-- line in the input lists.  You can picture this as a zip fastener, where you
-- slide the tab (f=line function) to interlock (produce a new list) the two 
-- parallel rows of metal teeth (the two input lists).  
--
-- [x1,    x2,    x3, ...
--     \      \      \
-- [f x1 y1, f x2 y2, f x3 y3, ...
--     /      /      /
-- [y1,    y2,    y3, ...
--
-- The line function (see below) just joins a label and data line string, 
-- appending a newline at the end.
--
zipWithLine (x:labels) (y:dataLines) = line x y : zipWithLine labels dataLines
zipWithLine   _          _           = []

line label dataLine = dataLine ++ label ++ "\n"

--
-- Converts each input data row into a string.
-- Conversion happens as follows:
--  * convert each data row into a padded row by converting each data item into
--    a string and adding some padding to it;  
--  * convert each padded row into a line string by concatenating the contained
--    padded items.
--
-- The amount of padding added to each data item in the conversion process is
-- computed such that all padded items (across all rows) have the same length
-- and data is aligned to the right.  This is accomplished by making each item
-- as wide as the number of digits in the greater input data item plus 4 space 
-- characters to pad, 2 on the left and 2 on the right.   
--
format dataRows = [ concat row | row<-paddedRows ]
    where
    maxDigits  = (length . show . foldMax . concat) dataRows
    paddedRows = [ [padCell x | x<-row] | row<-dataRows ]
    
    padCell x = leftPadding ++ content ++ rightPadding
        where
        content      = show x
        rightPadding = pad 2
        leftPadding  = pad (2 + (maxDigits - length content)) 
        pad size     = take size (repeat ' ')

--
-- Utility function to find the maximum of a list of integers.  It just folds
-- the max built-in function into the list:
-- 
--      foldMax [1,3,2] = 1 `max` 3 `max` 2 `max` 0
-- 
foldMax    []  = 0
foldMax (x:xs) = x `max` (foldMax xs)


{-
formatChart (["L","M","O","W"], [[100],
                                 [110,10],
                                 [70,30,40]])
  ~~>
    "\nL\n  100  M\n  110     10  O\n   70     30     40  W\n\n"                                 
-}
