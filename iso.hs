data Iso a b = Iso { f :: a -> b, g :: b -> a }
-- Should should satisfy the following laws:
--   ∀ x : a, g (f x) = x
--   ∀ x : b, f (g x) = x

inv :: Iso a b -> Iso b a
inv (Iso f g) = Iso g f

comp :: Iso a b -> Iso b c -> Iso a c
comp (Iso f1 g1) (Iso f2 g2) = Iso (f2 . f1) (g1 . g2)

main = do
  print "compiles"
