lines ← •file.Lines "./day08.txt"
grid ← >{𝕩 - '0'}¨lines
dim ← ≠grid
outer ← 4 × dim - 1

visible ← { e ← (⟨𝕨 ,𝕩 ⟩⊑ grid)
          ⋄ z ← e ≤(𝕨 ⊏ grid)
          ⋄ s ← e ≤ (𝕩 ⊏ ⍉ grid)
          ⋄ t ← ∨´𝕨 ↑s
          ⋄ b ← ∨´(𝕨 +1)↓s
          ⋄ l ← ∨´𝕩 ↑z
          ⋄ r ← ∨´(𝕩 +1)↓z
          ⋄ ∨´¬⟨t,b,l,r⟩
          }⌜˜1 + ↕(dim-2)

inner ← +´∾˝visible

visidx ← ({¬(1=≠∘⥊)¨𝕩}⊸/)˜(∾˝{ (⟨𝕨 ,𝕩 ⟩⊑ visible) ⊑ 0‿⟨𝕨 + 1,𝕩 + 1⟩}⌜˜↕(dim-2))

score ← { x ← 0⊑𝕩
        ⋄ y ← 1⊑𝕩
        ⋄ e ← ⟨x,y⟩⊑ grid
        ⋄ z ← e ≤(x ⊏ grid)
        ⋄ s ← e ≤ (y ⊏ ⍉ grid)
        ⋄ t ← (+´∧`)¬ ⌽ x↑s
        ⋄ st ← ((≠x↑s) =t) ⊑ (t+1)‿t
        ⋄ b ← (+´∧`)¬ (x+1)↓s
        ⋄ sb ← ((≠(x+1)↓s)=b) ⊑ (b+1)‿b
        ⋄ l ← (+´∧`) ¬ ⌽ y↑z
        ⋄ sl ← ((≠y↑z) =l) ⊑ (l+1)‿l
        ⋄ r ← (+´∧`)¬ (y+1)↓z
        ⋄ sr ← ((≠(y+1)↓z)=r) ⊑ (r+1)‿r
        ⋄ ×´⟨st,sb,sl,sr⟩
        }¨visidx

•Show "Puzzle 1: "‿(inner + outer)
•Show "Puzzle 2: "‿(⌈´score)
