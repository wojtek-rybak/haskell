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
        isAccepting = \q -> False, 
        transition = \q -> \a -> [] 
    }

epsA :: Auto a ()
epsA = A {
        states = [()],
        initStates = [()],
        isAccepting = \q -> True,
        transition = \q -> \a -> []
    }

symA :: Eq a => a -> Auto a Bool
symA c = A {
        states = [True, False],
        initStates = [False],
        isAccepting = id,
        transition = \q -> \a -> if not q && a == c then [True] else []
    }

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
genTransitions aut = [(q,a,trans q a) | a <- [minBound .. maxBound], 
                                        q <- states aut,
                                        length (trans q a) > 0
                     ]
                     where trans = transition aut

leftA :: Auto a q -> Auto a (Either q r)
leftA aut = A {
        states = mapToLeft (states aut),
        initStates = mapToLeft (initStates aut),
        isAccepting = either caseLeftAcc caseRightAcc,
        transition = newTrans
    }
    where caseLeftAcc = isAccepting aut
          caseRightAcc q = False
          newTrans q a = either (caseLeftTrans a) caseRightTrans q
          caseLeftTrans a q = mapToLeft $(transition aut) q a
          caseRightTrans q = []

mapToLeft :: [a] -> [Either a r]
mapToLeft = map Left

mapToRight :: [a] -> [Either r a]
mapToRight = map Right

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
          mtl = mapToLeft
          mtr = mapToRight

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
          mtl = mapToLeft
          mtr = mapToRight
          false _ = False

accepts :: Eq q => Auto a q -> [a] -> Bool
accepts aut w = acceptsRec aut (initStates aut) w

acceptsRec :: Eq q => Auto a q -> [q] -> [a] -> Bool
acceptsRec _ [] _ = False
acceptsRec aut s [] = any (isAccepting aut) s
acceptsRec aut s (w:ws) = acceptsRec aut newStates ws
                          where newStates = nub $concat (map f s)
                                f state = (transition aut) state w
