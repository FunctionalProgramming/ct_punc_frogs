module Main where

import Analyzer
import Chart
import Formatter


parse xs = read xs :: [(String,String,Int)]

main = interact (formatChart . makeChart . findDistances . parse)
