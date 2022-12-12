lines ← •file.Lines "./day12ex.txt"
map ← > {27 ⌊ | 48 - 𝕩×𝕩 ≥ 49}(-⟜'0') ¨ lines
idx ← /○⥊⟜(↕≢) (27 = map)
s ← 0 ⊑ idx
e ← 1 ⊑ idx
m ← 0˙⌾(s⊸⊑) map
⟨r,c⟩ ← ≢ m

Cross ← { u ← 0     ⌈ 𝕩 - ⟨1,0⟩ ⋄ d ← (r-1) ⌊ 𝕩 + ⟨1,0⟩ ⋄ l ← 0     ⌈ 𝕩 - ⟨0,1⟩ ⋄ r ← (c-1) ⌊ 𝕩 + ⟨0,1⟩ ⋄ ⍷ u‿d‿l‿r }

Step ← { •Show 𝕩 ⋄ a ← 𝕩 ⋄ x ← ⊑ 𝕩 ⋄ i ← x ⊑ m ⋄ p ← Cross x ⋄ •Show i ⋄ d ← /⟜p˜{¬∨´𝕩⊸ ≡¨a}¨p ⋄ •Show (d ⊑ m) ⋄ n ← (⊑ ⟜ d) ˜ ⊑ / {((i-1)=𝕩)∨(i=𝕩)}¨d ⊑ m ⋄ •Show n ⋄ (10 > ⊑ ≢ 𝕩) ? 𝕊 (⋈ n)∾ 𝕩 ; 𝕩} 

 n ← (⊑ ⟜ d) ˜ ⊑ / i ≤ d ⊑ m ⋄ •Show 𝕩 

(Cross ⟨ 1,6 ⟩ ) ⊑ m
⟨ 1,6 ⟩ ⊑ m
⊑ m
p ← ⟨ ⟨ 0,0 ⟩,⟨ 0,1 ⟩,⟨ 1,0 ⟩ ⟩

{F q: ¬∨´q⊸ ≡¨x}¨p

Step ⋈ e 

x ← ⟨ ⟨ 1,4 ⟩,⟨ 1,3 ⟩, ⟨ 0,3 ⟩,⟨ 0,2 ⟩,⟨ 0,1 ⟩,⟨ 0,0 ⟩ ⟩
c ← ⟨ ⟨ 0,0 ⟩,⟨ 1,4 ⟩,⟨ 2,4 ⟩ ⟩


d ← ⊑ /⟜c˜{¬∨´𝕩⊸ ≡¨x}¨c
      

      ⟨1,4⟩⊸≡¨c



Move ← {n ← Step x ⋄ ( 10 > ⊑ ≢ 𝕩) ? 𝕊 (⋈ n)∾ 𝕩 ; 𝕩 }

(¬ n ≡ e) ? Move p ; (1 + ≢ 𝕩) }

⋄ •Show (≢ 𝕩) ⋄ •Show (5 < ≢ 𝕩)}
⋄ (5 < ≢ 𝕩) ⊑ 'b'‿(Step ((⋈ n)∾𝕩))}

⊑ (𝕊 ((⋈ n)∾𝕩)))‿"exit"}

⋄ (n ≡ e) ⊑ (𝕊 ((⋈ n)∾𝕩))‿(1 + ≢ 𝕩) }


       ⋄ •Show "n: "‿n ⋄ n }
       ⋄ •Show "c: "‿c
       ⋄ •Show "n: "‿n
       }


       ⋄ y ← ⊑ x < i ⊑ m
        ⋄ f ← ∨´∧´¨e⊸=¨cs

⍷ 
⊑∘/∘∊ 



(⌊´⊐)

0 ⌈ ⟨3,¯3⟩
outer ← 4 × dim - 1
