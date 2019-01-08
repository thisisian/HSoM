import Euterpea

-- Chapter 4 --


addDur :: Dur -> [Dur -> Music a] -> Music a
addDur d ns = let f n = n d
              in line (map f ns)

graceNote :: Int -> Music Pitch -> Music Pitch
graceNote n (Prim (Note d p)) = note (d/8) (trans n p) :+: note (7 * d / 8) p
graceNote n _ = error "Can only add a grace note to a note"

-- ex 4.1
-- TODO

prefixes :: [a] -> [[a]] 
prefixes [] = []
prefixes (x:xs) = let f pf = x : pf 
                  in  [x] : map f (prefixes xs)

mel1 = [c 5 en, e 5 sn, g 5 en, b 5 en, a 5 en, f 5 sn, d 5 en, b 4 sn, c 5 en]
mel2 = [c 5 sn, e 5 sn, g 5 sn, b 5 sn, a 5 sn, f 5 sn, d 5 sn, b 4 sn, c 5 sn]

test1 = playDev 2 $ line (mel1)
test2 = playDev 2 $ line (mel2)

prefix :: [Music a] -> Music a
prefix mel = let m1 = line (concat (prefixes mel))
                 m2 = transpose 12 (line (concat (prefixes (reverse mel))))
                 m  = instrument AcousticGuitarNylon m1 :=: instrument AcousticGrandPiano m2
             in  m :+: transpose 5 m :+: m

mymel1 = [c 5 dqn, e 5 en, f 5 qn, c 5 dqn, b 4 en, a 4 qn] 
mymel2 = [c 5 qn, c 5 qn, g 5 en, f 5 en, a 5 en, g 5 en]

myprefix :: [Music a] -> Music a
myprefix mel = let m1 = line (concat (prefixes mel))
                   m2 = transpose 12 (line (concat (prefixes (reverse mel))))
                   m3 = tempo 2 m2 
                   m  = instrument AcousticGrandPiano m1 :=: instrument AcousticGuitarNylon (m3 :+: m3)
               in  m :+: transpose 5 m :+: m

-- Chapter 5

-- ex 5.1
twice :: (a -> a) -> a -> a
twice f = f.f

-- twice twice will apply the function four times
-- twice twice twice will apply the function sixteen times 
-- twice (twice twice) will apply the function eight times

-- ex 5.2

power :: (a -> a) -> Int -> a -> a
power f 0 = id
power f n = power f (n-1) .f

ex2 = power (:+: c 4 qn) 3 (d 5 qn)

-- ex 5.3

fix :: (a -> a) -> a
fix f = f (fix f)

remainder :: Integer -> Integer -> Integer
remainder a b = if a < b then a
                else remainder (a-b) b

-- rewrite to use fix in place of recursive remainder call
remainder2 :: Integer -> Integer -> Integer
remainder2 a b = fix (\remainder a b -> if (a < b) then a else remainder (a-b) b) a b

-- ex 5.4
-- apPairs aps1 aps2 is a list of all combinations of the absolute pitches in aps1 and aps2. 
-- Furthermore, for each pair (ap1, ap2) in the result, the absolute value of ap1 − ap2 must be greater than two and less than eight.
apPairs :: [AbsPitch] -> [AbsPitch] -> [(AbsPitch, AbsPitch)]
apPairs aps1 aps2 = [(x, y) | x <- aps1, y <- aps2, (x - y) > 2 && (x - y) < 8 ]

--Finally, write a function to turn the result of apPairs into a Music Pitch value by playing 
--each pair of pitches in parallel and stringing them all together sequentially. 
--Try varying the rhythm by, for example, using an eighth note when the first absolute pitch 
--is odd and a sixteenth note when it is even, or some other criterion

pairsToMusic :: [(AbsPitch, AbsPitch)] -> Music Pitch
pairsToMusic = let f (ap1, ap2) = let d = if ap1 `mod` 2 == 0 then en else qn
                                  in  note d (pitch ap1) :=: note d (pitch ap2)
                  in line . map (f)

-- ex 5.5
hNote :: Dur -> Pitch -> Music Pitch
hNote d p = note d p :=: note d (trans (-3) p)

hList :: Dur -> [Pitch] -> Music Pitch
hList d = line . map (hNote d)

--TODO
-- rewrite hList to eta reduce the d argument
--hList2 :: Dur -> [Pitch] -> Music Pitch
--hList2 d = line . map hNote . zip d

-- ex 5.6
addDur2 :: Dur -> [Dur -> Music a] -> Music a
addDur2 d = line . map (\n -> n d)

-- ex 5.7
ex5_7 = map ((/2).(+1))

-- ex 5.8 

ex5_8 :: (b -> c) -> (a -> b) -> [a] -> [c]
ex5_8 f g = map (f.g)
ex5_8_2 = map (/2) . map (+1)

-- ex 5.9 
-- TODO

-- ex 5.10
-- fill in missing functions f1 and f2 so that
-- f1 (f2 (*) [1,2,3,4]) 5 => [5, 10, 15, 20]
f1 :: (Integer -> Integer -> Integer) -> [Integer] -> [(Integer -> Integer)]
f1 = map

f2 :: [Integer -> Integer] -> Integer -> [Integer]
f2 ops x = map (flip ($) x) ops

-- Chapter 6

m = line [b 4 qn, f 4 qn, g 4 qn, a 4 qn]
mel3 = m :+: retro m :+: invert m :+: invertRetro m :+: retroInvert m

-- ex 6.1 
--Show retro.retro, invert.invert and retroInvert.invertRetro are identities

-- retro.retro
-- => (line.reverse.lineToList).(line.reverse.lineToList) l
-- => (line.reverse.lineToList).reverse l
-- => reverse (reverse l)
-- => l

-- invert.invert
