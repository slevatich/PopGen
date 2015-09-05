> module PopGen where 
> import Euterpea hiding (key, Dur, dur, major, Major, Minor, qn, mode, MP, e)
> import PTGG
> import System.Random
> import Data.List hiding (transpose)
> import MusicGrammars hiding (rGen, rGenShow) 

Name: Sam Levatich
NetID: sml73

Potential Band Name: Lambda and the Monads

Final Structure:
Mel:  ---  - Verse - Chorus - Verse - Chorus - Bridge (V) - Chorus - Mod Chorus (ritardando)
Har: Verse - Verse - Chorus - Verse - Chorus - Bridge (V) - Chorus - Mod Chorus (ritardando)
Bas: Verse - Verse - Chorus - Verse - Chorus - Bridge (V) - Chorus - Mod Chorus (ritardando)

> songGenerator :: Int -> Int -> Music Pitch
> songGenerator i n = 
>   let 
>     s = instrument ElectricGrandPiano (ptggToChord (i) (n) qn harRules) :=: -- intro requires seperate generation in order to be able to tack on the melody
>         instrument SynthBass1 (ptggToNote (i*7) (n-12) (qn/2) bassRules)
>     v = s :=: instrument Harpsichord (ptggToNote (i*3) (n) (qn/4) melRules)
>     c = chorusGenerator (i*2) n qn
>     b = transpose 5 $ verseGenerator (i*3) n qn
>     modC = transpose 2 $ modChorusGenerator (i*2) n qn
>     tick = instrument Percussion $ note qn (pitch 48) :+: note qn (pitch 48) :+: note qn (pitch 48) :+: note qn (pitch 48)
>   in tempo (2/3) $ s :+: s :+: v :+: v :+: tick :+: c :+: c :+: v :+: v :+: tick :+:
>                    c :+: c :+: b :+: b :+: c :+: c :+: modC :+: tempo (4/5) modC

> verseGenerator :: Int -> Int -> Dur -> Music Pitch
> verseGenerator i n d = instrument Harpsichord        (ptggToNote  (i*3) (n+12) (d/4) melRules) :=:
>                        instrument ElectricGrandPiano (ptggToChord (i)   (n)    (d)   harRules) :=: 
>                        instrument SynthBass1         (ptggToNote  (i*7) (n-12) (d/2) bassRules)

> chorusGenerator :: Int -> Int -> Dur -> Music Pitch
> chorusGenerator i n d = instrument OverdrivenGuitar (ptggToNote (i*3) (n+12) (d/4) melChorRules) :=:
>                         instrument Violin           (ptggToArp  (i)   (n)    (d)   harRules) :=: 
>                         instrument SynthBass1       (ptggToNote (i*7) (n-12) (d/2) bassRules)

> modChorusGenerator :: Int -> Int -> Dur -> Music Pitch
> modChorusGenerator i n d = instrument VoiceOohs (ptggToNote  (i*3) (n+12) (d/4) melRules) :=:
>                            instrument VoiceOohs (ptggToChord (i)   (n)    (d)   harRules) :=: 
>                            instrument VoiceOohs (ptggToNote  (i*7) (n-12) (d/2) bassRules)

PTGG->Music Makers!

> ptggToNote :: Int -> Int -> Dur -> (Dur -> [Rule CType MP]) -> Music Pitch
> ptggToNote i n d r =
>	let 
>     a = rGen i d r
>     f :: [(CType, MP)] -> Music Pitch
>     f [] = rest 0
>     f ((c,ms@MP{dur = d, mode = m, key = k}):xs) = 
>	    let 
>         root = fromEnum(c)
>         dur = d
>         struct = [0]
>         key = k
>         b = chord $ map (note dur) (toPitches $ map (n+root+key+) struct)
>       in  b :+: f xs
>   in  f a

> ptggToChord :: Int -> Int -> Dur -> (Dur -> [Rule CType MP]) -> Music Pitch
> ptggToChord i n d r =
>	let 
>     a = rGen i d r
>     f :: [(CType, MP)] -> Music Pitch
>     f [] = rest 0
>     f ((c,ms@MP{dur = d, mode = m, key = k}):xs) = 
>	    let 
>         root = fromEnum(c)
>         dur = d
>         struct = if isMaj ms then [0,4,7] else [0,3,7]
>         key = k
>         b = chord $ map (note dur) (toPitches $ map (n+root+key+) struct)
>       in  b :+: f xs
>   in  f a

> ptggToArp :: Int -> Int -> Dur -> (Dur -> [Rule CType MP]) -> Music Pitch
> ptggToArp i n d r =
>	let 
>     a = rGen i d r
>     f :: [(CType, MP)] -> Music Pitch
>     f [] = rest 0
>     f ((c,ms@MP{dur = d, mode = m, key = k}):xs) = 
>	    let 
>         root = fromEnum(c)
>         dur = d
>         struct = if isMaj ms then [0,4,7,0,4,7,0,4] else [0,3,7,0,3,7,0,3]
>         key = k
>         b = line $ map (note (dur/8)) (toPitches $ map (n+root+key+) struct)
>       in  b :+: f xs
>   in  f a

> toPitches :: [Euterpea.AbsPitch] -> [Pitch] 
> toPitches [] = []
> toPitches (a:as) = pitch a : toPitches as

Generator!

> rGen :: Int -> Dur -> (Dur -> [Rule CType MP]) -> [(CType,MP)]
> rGen s d r = toPairs $ snd $ gen (r d) (mkStdGen s, [NT (I,MP 2 Major 0)]) !! 5

RuleSets!

> harRules :: Dur -> [Rule CType MP]
> harRules minDur = normalize $ map (toRelDur2 (<minDur)) [
>     -- Rules for I --
>     (I, 0.50) :-> \p -> [i  (q p), v  (q p), vi (q p), iv (q p)],
>     (I, 0.50) :-> \p -> [i p],
>     -- Rules for II --
>     (II, 0.40) :-> \p -> if isMaj p then [ii p] else [iv p],
>     (II, 0.40) :-> \p -> if isMaj p then (if dur p > qn then [ii p] else [i (m2 p)]) else [ii p],
>     (II, 0.20) :-> \p -> map ($ h p) $ if isMaj p then [vi, ii] else [vi, iv],
>     -- Rules for III--
>     (III, 0.90) :-> \p -> [iii p],
>     (III, 0.10) :-> \p -> [i $ m3 p],
>     -- Rules for IV -- 
>     (IV, 0.90) :-> \p -> [iv p],
>     (IV, 0.10) :-> \p -> [i $ m4 p],
>     -- Rules for V --
>     (V, 0.2) :-> \p -> [v p],
>     (V, 0.2) :-> \p -> [iii p],
>     (V, 0.2) :-> \p -> [i $ m3 p],
>     (V, 0.2) :-> \p -> [ii p],
>     (V, 0.2) :-> \p -> [iv p],
>     -- Rules for VI --
>     (VI, 0.70) :-> \p -> [vi p],
>     (VI, 0.30) :-> \p -> [i $ m6 p]
>     ]

> bassRules :: Dur -> [Rule CType MP]
> bassRules minDur = normalize $ map (toRelDur2 (<minDur)) [
>     -- Rules for I  --
>     (I, 0.90) :-> \p -> [i  (q p), v  (q p), vi (q p), iv (q p)],
>     (I, 0.01) :-> \p -> [i p],
>     (I, 0.09) :-> \p -> map ($ h p) [i, i],
>     -- Rules for II --
>     -- Rules for III--
>     -- Rules for IV -- 
>     (IV, 0.20) :-> \p -> [iv p],
>     (IV, 0.50) :-> \p -> map ($ h p) [iv, vi],
>     (IV, 0.30) :-> \p -> map ($ h p) [iv, iv],
>     -- Rules for V --
>     (V, 0.40) :-> \p -> [v p],
>     (V, 0.10) :-> \p -> [vi p],
>     (V, 0.10) :-> \p -> [ii p],
>     (V, 0.30) :-> \p -> map ($ h p) [vi, v],
>     (V, 0.10) :-> \p -> map ($ h p) [v, v]
>     -- Rules for VI --
>     -- Rules for VII--
>     ]

> melRules :: Dur -> [Rule CType MP]
> melRules minDur = normalize $ map (toRelDur2 (<minDur)) [
>     -- Rules for I --
>     (I, 0.45) :-> \p -> [i (q p), ii (q p), iii  (q p), i (q p)],
>     (I, 0.50) :-> \p -> [i  (q p), v  (q p), vi (q p), iv (q p)],
>     (I, 0.05) :-> \p -> [i (q p), i (h p), i (q p)],
>     -- Rules for II --
>     (II, 0.50) :-> \p -> [ii (q p), iii (h p), ii (q p)],
>     (II, 0.50) :-> \p -> [ii (q p), iii (h p), vi (q p)],
>     -- Rules for III--
>     (III, 0.50) :-> \p -> [iii (q p), i (q p), iii (q p), v (q p)],
>     (III, 0.50) :-> \p -> [iii (h p), iv (q p), v (q p)],
>     -- Rules for IV -- 
>     (IV, 0.50) :-> \p -> map ($ q p) [iv, v, vi, vii],
>     (IV, 0.50) :-> \p -> [iv (q p), iii (q p), ii (h p)],
>     -- Rules for V --
>     (V, 0.4) :-> \p -> map ($ q p) [v, v, vi, v],
>     (V, 0.4) :-> \p -> map ($ q p) [v, v, vii, vii],
>     (V, 0.2) :-> \p -> [v (h p), i (q p), v (q p)],
>     -- Rules for VI --
>     (VI, 0.70) :-> \p -> [vi (q p), vii (q p), vi (q p), v (q p)],
>     (VI, 0.30) :-> \p -> [vi (q p), iv (q p), vi (q p), iv (q p)],
>     -- Rules for VII--
>     (VII, 1.00) :-> \p -> [vii (q p), vii (q p), vii (h p)]
>     ]

> melChorRules :: Dur -> [Rule CType MP]
> melChorRules minDur = normalize $ map (toRelDur2 (<minDur)) [
>     -- Rules for I --
>     (I, 0.50) :-> \p -> [v (q p), ii (q p), v  (q p), i (q p)],
>     (I, 0.50) :-> \p -> [v  (q p), vi  (q p), vii (q p), i (q p)],
>     -- (I, 0.05) :-> \p -> [i (q p), i (h p), i (q p)],
>     -- Rules for II --
>     (II, 0.50) :-> \p -> [ii (q p), iii (h p), ii (q p)],
>     (II, 0.50) :-> \p -> [ii (q p), iii (h p), vi (q p)],
>     -- Rules for III--
>     (III, 0.50) :-> \p -> [iii (q p), i (q p), iii (q p), v (q p)],
>     (III, 0.50) :-> \p -> [iii (h p), iv (q p), v (q p)],
>     -- Rules for IV -- 
>     (IV, 0.50) :-> \p -> map ($ q p) [iv, v, vi, vii],
>     (IV, 0.50) :-> \p -> [iv (q p), iii (q p), ii (h p)],
>     -- Rules for V --
>     (V, 0.4) :-> \p -> map ($ q p) [v, v, vi, v],
>     (V, 0.4) :-> \p -> map ($ q p) [v, v, vii, vii],
>     (V, 0.2) :-> \p -> [v (h p), i (q p), v (q p)],
>     -- Rules for VI --
>     (VI, 0.70) :-> \p -> [vi (q p), vii (q p), vi (q p), v (q p)],
>     (VI, 0.30) :-> \p -> [vi (q p), iv (q p), vi (q p), iv (q p)],
>     -- Rules for VII--
>     (VII, 1.00) :-> \p -> [vii (q p), vii (q p), vii (h p)]
>     ]


