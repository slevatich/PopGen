# PopGen
A PTGG and Euterpea (Donya Quick, Paul Hudak) based pop song generator in Haskell, modeled on popular four chord structures. A final project in CPSC 431 at Yale University for Sam Levatich

# Dependancies
Haskell and Euterpea: See http://haskell.cs.yale.edu/euterpea/download/ for installation instructions

PTGG.lhs and MusicGrammars.lhs: Included in repository

# How to Run
After loading PopGen into ghci, run the following, where i is a random integer seed and n is the absPitch of the key (Middle C is 48, mid range recommended):
_play $ songGenerator i n_