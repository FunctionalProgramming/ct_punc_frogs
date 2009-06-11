--
-- Implementation of the supermarket billing exercise in section 6.4 of the
-- Craft.  We follow the directions given in the book and supply the needed
-- functions.
--
module SupermarketBilling where

type Name     = String
type Price    = Int
type BarCode  = Int
type Database = [(BarCode, Name, Price)]
type TillType = [BarCode]
type BillType = [(Name, Price)]


codeIndex :: Database
codeIndex = [
    (4719, "Fish Fingers", 121),
    (5643, "Nappies", 1010),
    (3814, "Orange Jelly", 56),
    (1111, "Hula Hops", 21),
    (1112, "Hula Hops (Giant)", 133),
    (1234, "Dry Sherry, 1lt", 540) ]

lineLength :: Int
lineLength = 30



produceBill :: TillType -> String
produceBill = formatBill . makeBill

makeBill :: TillType -> BillType
makeBill []     = []
makeBill (c:cs) = look codeIndex c : makeBill cs 

look :: Database -> BarCode -> (Name, Price)
look              []  code = ("Unknown Item", 0)
look ((c, n, p) : ds) code 
    | code == c            = (n, p)
    | otherwise            = look ds code

makeTotal :: BillType -> Price
makeTotal           []  = 0    
makeTotal ((n, p) : xs) = p + makeTotal xs 


formatBill :: BillType -> String
formatBill xs = "\n" ++ formatLines xs ++ 
                "\n" ++ formatTotal (makeTotal xs) ++ 
                "\n"

formatLines :: BillType -> String
formatLines             []  = []
formatLines ((n, p) : rest) = formatLine (n, p) ++ formatLines rest

formatLine :: (Name, Price) -> String
formatLine (n, p) = name ++ fill ++ price ++ "\n"
    where
    ps       = formatPence p
    price    = take priceLen ps
    name     = take nameLen n
    fill     = take fillLen (repeat '.')
    availLen = lineLength - 2
    priceLen = min availLen (length ps)
    nameLen  = min (availLen - priceLen) (length n)
    fillLen  = lineLength - priceLen - nameLen 

formatPence :: Price -> String
formatPence p = show pounds ++ separator ++ show pence
    where
    pounds    = p `div` 100
    pence     = p `mod` 100
    separator = if pence < 10 then ".0" else "." 
    
formatTotal :: Price -> String
formatTotal p = formatLine ("Total", p)

printBill :: TillType -> IO ()
printBill = putStr . produceBill

{-
printBill [1234, 4719, 3814, 1112, 1113, 1234]      ~~>

Dry Sherry, 1lt...........5.40
Fish Fingers..............1.21
Orange Jelly..............0.56
Hula Hops (Giant).........1.33
Unknown Item..............0.00
Dry Sherry, 1lt...........5.40

Total....................13.90

-}
