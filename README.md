# PopGen
A PTGG and Euterpea (Donya Quick, Paul Hudak) based pop song generator in Haskell, modeled on popular four chord structures. A final project in CPSC 431 at Yale University for Sam Levatich

# Dependancies
See http://haskell.cs.yale.edu/euterpea/download/ for Haskell and Euterpea installation instructions. PTGG.lhs and MusicalGrammars.lhs by Donya Quick and Paul Hudak are included in the repository.

# How to Run
After loading PopGen into ghci, run the following, where i is a random integer seed and n is the absPitch of the key (Middle C is 48, mid range recommended):

_play $ songGenerator i n_

# Examples
If you're too afraid to try a random root note and seed, why not try these?

* i = 7, n = 53 (Key of F)
* i = 43, n = 50 (Key of D)