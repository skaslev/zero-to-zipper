structure iso (a b : Type) :=
(f : a → b) (g : b → a) (gf : Π x, g (f x) = x) (fg : Π x, f (g x) = x)

def inv {a b} : iso a b → iso b a
| ⟨f, g, gf, fg⟩ := ⟨_, _, _, _⟩

def comp {a b c} : iso a b →  iso b c → iso a c
| ⟨f₁, g₁, g₁f₁, f₁g₁⟩ ⟨f₂, g₂, g₂f₂, f₂g₂⟩ := ⟨_, _, _, _⟩
