import Euterpea

-- Chapter 1 stuff -- 

p1 = (A, 4)
p2 = (B, 4)
p3 = (D, 4)

hNote :: Dur -> Int -> Pitch -> Music Pitch
hNote d t p = note d p :=: note d (trans t p)

hList :: Dur -> Int -> [Pitch] -> Music Pitch
hList d t [] = rest 0
hList d t (p:ps) = hNote d t p :+: hList d t ps

melody :: Music Pitch
melody = note qn p1 :+: note qn (trans 2 p1) :+: note qn p2

-- Chapter 2 stuff --

t251 :: Music Pitch 
t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
       in dMinor :+: gMajor :+: cMajor

-- ex 1

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne p d = let two = (trans 2 p)
                     five = (trans 7 p)
                     minor :: Pitch -> Dur -> Music Pitch
                     minor p d = note d p :=: note d (trans 3 p) :=: note d (trans 7 p)
                     major :: Pitch -> Dur -> Music Pitch
                     major p d = note d p :=: note d (trans 4 p) :=: note d (trans 7 p)
                 in  minor two d :+: major five d :+: major p (2*d)

data BluesPitchClass = Ro | MT | Fo | Fi | MS

-- ex 2

type BluesPitch = (BluesPitchClass, Pitch)

ro p d = note d (Ro, p)
mt p d = note d (MT, p)
fo p d = note d (Fo, p)
fi p d = note d (Fi, p)
ms p d = note d (MS, p)

fromBlues :: Music BluesPitch -> Music Pitch
fromBlues (Prim (Note d p)) = let fromBluesNote :: BluesPitch -> Pitch
                                  fromBluesNote (Ro, p) = p          -- root
                                  fromBluesNote (MT, p) = trans 3 p  -- minor third
                                  fromBluesNote (Fo, p) = trans 5 p  -- fourth 
                                  fromBluesNote (Fi, p) = trans 7 p  -- fifth
                                  fromBluesNote (MS, p) = trans 10 p -- minor seventh
                              in (Prim (Note d (fromBluesNote p)))
fromBlues (Prim (Rest d)) = (Prim (Rest d))
fromBlues (m1 :+: m2) = fromBlues m1 :+: fromBlues m2
fromBlues (m1 :=: m2) = fromBlues m1 :=: fromBlues m2
fromBlues (Modify c m) = Modify c (fromBlues m)


ch2ex2 = let k :: Pitch
             k = (C, 4) 
             melody = ro k qn :+: mt k qn :+: fo k qn :+: fi k qn :+: ms k qn
         in playDev 10 (fromBlues melody)

-- ex3

transM :: AbsPitch -> Music Pitch -> Music Pitch
transM ap (Prim (Note d p)) = (Prim (Note d (trans ap p)))
transM ap (Prim (Rest d))   = (Prim (Rest d))
transM ap (m1 :+: m2)       = transM ap m1 :+: transM ap m2
transM ap (m1 :=: m2)       = transM ap m1 :=: transM ap m2
transM ap (Modify c m)      = Modify c (transM ap m)

ch2ex3 = let melody = c 4 qn :+: note qn (trans 2 (C,4)) :+: transM (absPitch (D,0)) (c 4 qn :+: d 4 qn)
         in playDev 10 melody

-- Chapter 3 stuff --

wts :: Pitch -> [Music Pitch]
wts p = let f ap = note qn (pitch (absPitch p+ap))
        in map f [0,2,4,6,8]

ch3wts = print $ wts (C,4)

-- ex1
-- Transposes each pitch in second argument by amount in first argument
ch3f1 :: Int -> [Pitch] -> [Pitch]
ch3f1 t p = map (trans t) p

ch3ex1 = print $ ch3f1 2 ([(C,4), (D,4), (E,4)])

--ex2
-- Turns a list of durations into a list of rests with corresponding duration
ch3f2 :: [Dur] -> [Music a]
ch3f2 d = map rest d

ch3ex2 = print $ (ch3f2 [1,2,3,4] :: [Music Pitch])

--ex3
-- Takes a list of music values, half each duration and add a rest of the same value after
ch3f3 :: [Music Pitch] -> [Music Pitch]
ch3f3 m = let f :: Music Pitch -> Music Pitch
              f (Prim(Note d p)) = note (0.5*d) p :+: rest (0.5*d)
          in map f m

ch3ex3 = print $ (ch3f3 [c 4 qn, d 4 qn, e 4 qn])

--ex4

simple a b c = a * (b + c)

applyEach :: [(a->b)] -> a -> [b]
--applyEach (x:xs) v = x v : applyEach xs v
applyEach x v = let f a = a v
                in  map (f) x

ch3ex4 = applyEach [simple 2 2, (+3)] 5

--ex5
--Define a function applyAll that, given a list of functions [f1, f2, ..., fn] and a value v, 
-- returns the result f1 (f2 (...(fn v)...)). 

applyAll fs v = let f :: [(a->a)] -> (a->a)
                    f x = foldr (.) id x
                in  f fs v


--ex6
-- In append operation, the right most argument is not evaluated. By flipping the arguments, 
-- the left hand argument is not evaluated. Thus, we want this situation:
-- (xs ++ ys) ++ zs
-- with a computation time: len (ys) ++ len(zs)
-- instead of:
-- xs ++ (ys ++ zs)
-- with a computation time: len(ys) + len(ys++zs)
-- and foldl should be used

--ex7
length2 :: [a] -> Int
length2 xs = let one a = 1
             in foldl (+) 0 (map one xs)

--ex8
doubleEach xs = map (*2) xs

pairAndOne xs = let f a = (a, succ a)
                in  map f xs

addEachPair xs = let f (a,b) = a + b
                 in map f xs

addPairPointwise :: Num a => [(a, a)] -> (a, a)
addPairPointwise xs = let f (a,b) (c,d) = (a+c, b+d)
                      in foldr f (0,0) xs 

--ex9
fuse :: [Dur] -> [Dur -> Music a] -> [Music a]
fuse (d:ds) (n:ns) = n d : fuse ds ns
fuse [] [] = []
fuse _ _ = error "List lengths do not match"

--ex10 

maxAbsPitch :: [AbsPitch] -> AbsPitch
maxAbsPitch [] = error "Empty list"
maxAbsPitch aps = let max a b = if a > b then a else b 
                  in  foldl max 0 aps 

minAbsPitch :: [AbsPitch] -> AbsPitch
minAbsPitch [] = error "Empty list"
minAbsPitch (a:aps) = foldl min a aps 

--ex10
chrom :: Pitch -> Pitch -> Music Pitch
chrom p1 p2 = let f p1 p2 
                      | p1 == p2 = rest 0
                      | p1 < p2  = note qn pUp :+: f pUp p2
                      | p1 > p2  = note qn pDown :+: f pDown p2
                      where pUp = trans 1 p1
                            pDown = trans (-1) p1
              in note qn p1 :+: f p1 p2

--ex12

mkScale :: Pitch -> [Int] -> Music Pitch
mkScale p xs = note qn p :+: line (map (note qn . flip trans p) (scanl1 (+) xs))

--ex13
data MajModes = Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian

genScale Main.Ionian     = [2, 2, 1, 2, 2, 2, 1]
genScale Main.Dorian     = [2, 1, 2, 2, 2, 1, 2]
genScale Main.Phrygian   = [1, 2, 2, 2, 1, 2, 2]
genScale Main.Lydian     = [2, 2, 2, 1, 2, 2, 1]
genScale Main.Mixolydian = [2, 2, 1, 2, 2, 1, 2]
genScale Main.Aeolian    = [2, 1, 2, 2, 1, 2, 2]
genScale Main.Locrian    = [1, 2, 2, 1, 2, 2, 2]

--ex14

ch3ex14 = let 
            firstPart = line [c 4 qn, d 4 qn, e 4 qn, c 4 qn]
            secondPart = line [e 4 qn, f 4 qn, g 4 hn]
            thirdPart = line [g 4 en, a 4 en, g 4 en, f 4 en, e 4 qn, c 4 qn]
            lastPart = line [c 4 qn, g 3 qn, c 4 hn]
            melody = firstPart :+: firstPart :+: secondPart :+: secondPart
                     :+: thirdPart :+: thirdPart :+: lastPart :+: lastPart
          in
            melody :=: 
            (instrument AcousticGuitarNylon ((rest (2*wn)) :+: melody)) :=: 
            (instrument Tuba (rest (4*wn) :+: melody))
                  
--ex15

encrypt :: [Char] -> [Char]
encrypt cs = map (toEnum.(flip mod 256).(+1).fromEnum) cs

decrypt :: [Char] -> [Char] 
decrypt cs = map (toEnum.(flip mod 256).(+255).fromEnum) cs
