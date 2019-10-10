import Data.Maybe (fromJust)

-- B(x) = 1 + x B(x)^2
-- B'(x) = B(x)^2 + 2 x B(x) B'(x)
-- B'(x) = B(x)^2 / (1 - 2 x B(x))
-- Z_B(x) = x B(x)^2 / (1 - 2 x B(x))

data BinTree a = Leaf | Branch a (BinTree a) (BinTree a)
  deriving Show

data Segment a = SLeft a (BinTree a) | SRight a (BinTree a)
data ZBinTree a = Focus a (BinTree a) (BinTree a) [Segment a]

toZipper :: BinTree a -> Maybe (ZBinTree a)
toZipper Leaf = Nothing
toZipper (Branch x l r) = Just $ Focus x l r []

fromZipper :: ZBinTree a -> BinTree a
fromZipper   (Focus x l r []) = Branch x l r
fromZipper z@(Focus x l r (s:p)) = fromZipper $ up z

set :: ZBinTree a -> a -> ZBinTree a
set (Focus x l r p) y = Focus y l r p

left :: ZBinTree a -> ZBinTree a
left z@(Focus x Leaf r p) = z
left   (Focus x (Branch y ll lr) r p) = Focus y ll lr (SLeft x r:p)

right :: ZBinTree a -> ZBinTree a
right z@(Focus x l Leaf p) = z
right   (Focus x l (Branch y rl rr) p) = Focus y rl rr (SRight x l:p)

up :: ZBinTree a -> ZBinTree a
up z@(Focus x l r []) = z
up   (Focus x l r (SLeft y ur:p))  = Focus y (Branch x l r) ur p
up   (Focus x l r (SRight y ul:p)) = Focus y ul (Branch x l r) p

t :: BinTree Int
t = Branch 1 (Branch 2 Leaf (Branch 3 Leaf (Branch 4 Leaf Leaf)))
             (Branch 5 Leaf Leaf)

main = do
  let z  = fromJust $ toZipper t
      z1 = set (right $ left z) 42
      z2 = set (up z1) 0
  print $ fromZipper z2
