# q01.ua

SR! ← ⍜(⊜∘|≡⋅^0)⊸⦷

PartOne ← (
  # ⍜⊜∘ ≡⋅"0" ⊸⦷ "A"
  SR!"0" "A"
  SR!"1" "B"
  SR!"3" "C"
  -@0
  /+
)

PartTwo ← (
  SR!"0" "x"
  SR!"1" "A"
  SR!"2" "B"
  SR!"4" "C"
  SR!"6" "D"
  -@0
  SR!¯1 0
  ◫[[2]]
  ≡/+
  ↥0
  /+
)

PartThree ← (
  SR!"0" "x"
  SR!"2" "A"
  SR!"3" "B"
  SR!"5" "C"
  SR!"7" "D"
  -@0
  SR!¯2 0
  ◫[[3]]
  ≡/+
  ↥0
  /+
)

&fras "2024/data/q01_p1.txt"
PartOne

&fras "2024/data/q01_p2.txt"
PartTwo

&fras "2024/data/q01_p3.txt"
PartThree

# The End
