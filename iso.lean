structure iso (a b : Type) :=
(f : a → b) (g : b → a) (gf : Π x, g (f x) = x) (fg : Π x, f (g x) = x)

def inv {a b} : iso a b → iso b a
| ⟨f, g, gf, fg⟩ := ⟨g, f, fg, gf⟩

def comp {a b c} : iso a b →  iso b c → iso a c
| ⟨f₁, g₁, g₁f₁, f₁g₁⟩ ⟨f₂, g₂, g₂f₂, f₂g₂⟩ := ⟨f₂ ∘ f₁, g₁ ∘ g₂, by simp [g₂f₂, g₁f₁], by simp [f₁g₁, f₂g₂]⟩
