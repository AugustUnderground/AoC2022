file ← "./day01.txt"
lines ← •file.Lines file
groups ← {•BQN¨𝕩} ¨ ((0=≠∘⥊)¨lines) (⊢-˜+`×¬)⊸⊔ lines
top1 ← +´1 ↑ ∨ +´¨groups
top3 ← +´3 ↑ ∨ +´¨groups

•Show "Puzzle 1: "‿top1
•Show "Puzzle 2: "‿top3
