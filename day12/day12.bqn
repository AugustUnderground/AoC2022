lines ← •file.Lines "./day12ex.txt"
map ← > {27 ⌊ | 48 - 𝕩×𝕩 ≥ 49}(-⟜'0') ¨ lines
idx ← /○⥊⟜(↕≢) (27 = map)
s ← 0 ⊑ idx
e ← 1 ⊑ idx
m ← 0˙⌾(s⊸⊑) map
⟨r,c⟩ ← ≢ m

g ← m ≍ 0 ¨ m

X ← { north ← 𝕩 - ⟨1,0⟩
    ⋄ south ← 𝕩 + ⟨1,0⟩
    ⋄ west ← 𝕩 - ⟨0,1⟩
    ⋄ east ← 𝕩 + ⟨0,1⟩
    ⋄ ⍷ (⟨(r-1),(c-1)⟩⊸⌊)¨(⟨0,0⟩⊸⌈)¨north‿south‿west‿east
    }

S ← { wf ← 𝕨
    ⋄ mp ← 0 ⊏ 𝕩
    ⋄ gd ← 1 ⊏ 𝕩
    ⋄ i ←  wf ⊑ mp
    ⋄ p ← C x
    ⋄ f ← /⟜p˜{¬∨´𝕩⊸ ≡¨a}¨p
    ⋄ l ← f /˜{((i+1)=𝕩)∨(i=𝕩)}¨f ⊑ m
    ⋄ ng ← (i˙⌾(s⊸⊑)) gd{(i+1)˙⌾(𝕨⊸⊑) 𝕩}´l
    ⋄ nm ← mp ≍ ng
    ⋄ (0 ≥ ≠ l /˜ e⊸≡¨l) ? (𝕊  )∾ 𝕩 ; ⊑ ≢ 𝕩
    }

wf ← ⋈ s # 1 ↓ X s # 
mp ← 0 ⊏ g
gd ← 1 ⊏ g
t ← 1 + wf ⊑ mp
wv ← 1 + ⌊´⌊´˘gd

p ← X¨wf

ns ← /⟜p˜{¬∨´𝕩⊸ ≡¨wf}¨p # Not self
tl ← ns /˜
{((t+1)=𝕩)∨(t=𝕩)}¨ns ⊑ m

nw ← (i˙⌾(s⊸⊑)) gd{(i+1)˙⌾(𝕨⊸⊑) 𝕩}´l

p ← ⊑ X¨wf
f ← /⟜p˜{¬∨´𝕩⊸ ≡¨a}¨p
l ← f /˜{((i+1)=𝕩)∨(i=𝕩)}¨f ⊑ m
ng ← (i˙⌾(s⊸⊑)) gd{(i+1)˙⌾(𝕨⊸⊑) 𝕩}´l
nm ← mp ≍ ng
(0 ≥ ≠ l /˜ e⊸≡¨l) ? (𝕊  )∾ 𝕩 ; ⊑ ≢ 𝕩







s S g

p ← Cross s
a ← (⋈s)
v ← 0
foo ← ⟨⟨0,1⟩,⟨3,2⟩,⟨1,2⟩,⟨2,5⟩⟩
0 ≥ ≠ foo /˜ e⊸≡¨foo

f ← /⟜p{¬∨´𝕩⊸ ≡¨a}¨p
i ← s ⊑ m
l ← f /˜{((i+1)=𝕩)∨(i=𝕩)}¨f ⊑ m

ng ← (i˙⌾(s⊸⊑)) (1⊏g){(i+1)˙⌾(𝕨⊸⊑) 𝕩}´l

((1 ⊏ g)⊸{𝕨 + (v+1)˙⌾(𝕩⊸⊑) 𝕨})´l

p ← Step ⋈ e 

•Out "Puzzle 1: "∾(•Fmt p)
