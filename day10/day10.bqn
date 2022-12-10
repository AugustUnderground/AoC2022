ops ← •file.Lines "./day10.txt"

cyc  ← {𝕩 ⊑ (𝕨 + 1)‿(𝕨 + 2) }`{(2×⊑ 𝕩) ∾ 1↓𝕩}˜"addx"⊸(≤○≠◶0‿(⊣≡≠⊸↑))¨ops
max  ← 220
get  ← 40(⊢/˜0=|⟜(↕≠))(↕20)«↕(1+max)
idx  ← {(+´∧`)(𝕩⊸>)¨cyc}¨get
sig  ← +´get{𝕨×1+´{•BQN (5↓𝕩)}¨{(¬"noop"⊸≡¨𝕨) / 𝕩}˜(↕𝕩) ⊏ ops}¨idx
ds   ← |-˝⍉2↕0∾cyc
inst ← (|1-≢ds) ↑ ops
pos  ← ∾ds{𝕨(∾˝/⟜≍)(⋈𝕩)}¨((⋈↕3) ∾{(↕3)+𝕩}¨+`{•BQN 𝕩}¨{("noop" ≡ 𝕩) ⊑ (5↓𝕩)‿"0"}¨inst)
vis  ← (((≢pos)↑ ∾6(×⟜≠⥊ ⊢)(⋈↕40)){∨´(𝕨⍷𝕩)}¨pos)
crt  ← ∾¨(40(⌊∘÷˜⟜(↕≠)⊔⊢){𝕩 ⊑ "."‿"#" }¨vis)


•Out "Puzzle 1: " ∾ (•Fmt sig)
•Out "Puzzle 2: "
•Out¨crt
