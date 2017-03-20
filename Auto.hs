module Auto (
    Auto,
    emptyA,
    epsA,
    symA,
    fromLists,
    toLists,
    leftA,
    sumA,
    thenA,
    accepts
) where

import Data.List (nub)

data Auto a q = A {
    states      :: [q],
    initStates  :: [q],
    isAccepting :: q -> Bool,
    transition  :: q -> a -> [q]
}

instance (Show a, Enum a, Bounded a, Show q) => Show (Auto a q) where
    show aut = "fromLists " ++ (show (toLists aut))

emptyA :: Auto a ()
emptyA = A { 
        states = [], 
        initStates = [],
        isAccepting = false, 
        transition = emptyTransitions
    }

epsA :: Auto a ()
epsA = A {
        states = [()],
        initStates = [()],
        isAccepting = true,
        transition = emptyTransitions
    }

false :: a -> Bool
false _ = False

true :: a -> Bool
true _ = True

emptyTransitions :: q -> a -> [q]
emptyTransitions _ _ = []

symA :: Eq a => a -> Auto a Bool
symA c = A {
        states = [True, False],
        initStates = [False],
        isAccepting = id,
        transition = trans
    }
    where trans q a = if (not q) && (a == c) then [True] else []

fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q,a,[q])] -> Auto a q
fromLists s is acc tr = A {
        states = s,
        initStates = is,
        isAccepting = \q -> elem q acc,
        transition = \q -> \a -> concat [l | (q',a',l) <- tr, q' == q, a' == a]
    }

toLists :: (Enum a,Bounded a) => Auto a q -> ([q],[q],[q],[(q,a,[q])])
toLists aut = (
        states aut,
        initStates aut,
        filter (isAccepting aut) (states aut),
        genTransitions aut
    )

genTransitions :: (Enum a,Bounded a) => Auto a q -> [(q,a,[q])]
genTransitions aut = [(q,a,trans q a) |
                       a <- [minBound .. maxBound],
                       q <- states aut,
                       length (trans q a) > 0
                     ]
                     where trans = transition aut

leftA :: Auto a q -> Auto a (Either q r)
leftA aut = A {
        states = mapToLeft (states aut),
        initStates = mtl (initStates aut),
        isAccepting = either (isAccepting aut) false,
        transition = newTrans
    }
    where newTrans q a = either (caseLeftTrans a) caseRightTrans q
          caseLeftTrans a q = mtl $(transition aut) q a
          caseRightTrans _ = []

sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA aut1 aut2 = A {
        states = mtl (states aut1) ++ mtr (states aut2),
        initStates = mtl (initStates aut1) ++ mtr (initStates aut2),
        isAccepting = either (isAccepting aut1) (isAccepting aut2),
        transition = newTrans
    }
    where newTrans q a = either (caseLeftTrans a) (caseRightTrans a) q
          caseLeftTrans a q = mtl $(transition aut1) q a
          caseRightTrans a q = mtr $(transition aut2) q a

thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA aut1 aut2 = A { 
        states = mtl (states aut1) ++ mtr (states aut2), 
        initStates = addEpsilons (initStates aut1),
        isAccepting = either false (isAccepting aut2), 
        transition = newTrans
    }
    where newTrans q a = either (caseLeftTrans a) (caseRightTrans a) q
          caseLeftTrans a q = addEpsilons $(transition aut1) q a
          caseRightTrans a q = mtr $(transition aut2) q a
          addEpsilons q = case any (isAccepting aut1) q of 
              True -> mtl q ++ mtr (initStates aut2)
              False -> mtl q

accepts :: Eq q => Auto a q -> [a] -> Bool
accepts aut w = acceptsRec aut (initStates aut) w

acceptsRec :: Eq q => Auto a q -> [q] -> [a] -> Bool
acceptsRec _ [] _ = False
acceptsRec aut states [] = any (isAccepting aut) states
acceptsRec aut states (w:ws) = acceptsRec aut newStates ws
                               where newStates = nub $concat (map f states)
                                     f state = (transition aut) state w

mapToLeft :: [a] -> [Either a r]
mapToLeft = map Left

mapToRight :: [a] -> [Either r a]
mapToRight = map Right

mtl :: [a] -> [Either a r]
mtl = mapToLeft

mtr :: [a] -> [Either r a]
mtr = mapToRight
