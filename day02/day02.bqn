strategy ← •file.Lines "./day02.txt"
strategyT ← {<˘⍉>𝕩} (" "⊸ ((⊢-˜¬×·+`»⊸<)∘∊˜⊔⊢))¨strategy
shifted ← (-⟜23) ¨ 1 ⊑  strategyT
strat ← (⊑ strategyT)‿shifted

score ← ⊑{<˘⍉>𝕩}-(16⊸-)¨((1⊑ strat) - '0')
draw ← (3⊸×) ∾ =´strat
win ← (6⊸×)⊑{<˘⍉>𝕩} {((𝕨="A")∧ (𝕩="B"))∨((𝕨 ="B")∧ (𝕩="C"))∨((𝕨="C")∧ (𝕩="A"))}¨´strat
total1 ← +´+´ ⟨score, draw, win⟩

•Show "Puzzle 1: "‿total1

opponent ← ⊑ strategyT
outcome ← 1 ⊑ strategyT
t ←{<˘⍉>𝕩} strategyT

#Test ← {fn←{Cond‿Act 𝕊 else: Cond◶Else‿Act}´𝕩 ⋄ Fn@}
Test ← { "A"‿"X": "C"; "A"‿"Y": "A"; "A"‿"Z": "B"
       ; "B"‿"X": "A"; "B"‿"Y": "B"; "B"‿"Z": "C"
       ; "C"‿"X": "B"; "C"‿"Y": "C"; "C"‿"Z": "A" } 

w ← (6⊸×)⊑{<˘⍉>𝕩} "Z"⊸= ¨outcome
d ← (3⊸×)⊑{<˘⍉>𝕩} "Y"⊸= ¨outcome
s ← ⊑{<˘⍉>𝕩}-(16⊸-)¨((Test¨t) - '0')

total2 ← +´+´ ⟨s, d, w⟩

•Show "Puzzle 2: "‿total2
