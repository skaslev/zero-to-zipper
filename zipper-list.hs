import Data.Maybe (fromJust)

-- L(x) = 1/(1-x)
-- L'(x) = 1/(1-x)^2 = L(x)^2
-- Z_L(x) = x L(x)^2

data ZList a = Focus a [a] [a]

toZipper :: [a] -> Maybe (ZList a)
toZipper [] = Nothing
toZipper (x:xs) = Just $ Focus x xs []

fromZipper :: ZList a -> [a]
fromZipper   (Focus x r []) = x:r
fromZipper z@(Focus x r (y:p)) = fromZipper $ left z

set :: ZList a -> a -> ZList a
set (Focus x r p) y = Focus y r p

left :: ZList a -> ZList a
left z@(Focus x r []) = z
left   (Focus x r (y:p)) = Focus y (x:r) p

right :: ZList a -> ZList a
right z@(Focus x [] p) = z
right   (Focus x (y:r) p) = Focus y r (x:p)

main = do
  let z  = fromJust $ toZipper [1,2,3,4,5]
      z1 = set (right $ right z) 42
      z2 = set (left z1) 0
  print $ fromZipper z2
