{-
100 pytań do:
- czy w fromLists trzeba wykrywac nielegalne automaty? np takie, które mają
    przejście do stanu x ale nie maja stanu x
-}

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
toLists a = (
        states a,
        initStates a,
        filter (isAccepting a) (states a),
        genTransitions a
    )

genTransitions :: (Enum a,Bounded a) => Auto a q -> [(q,a,[q])]
genTransitions aut = [(q,a,trans q a) | a <- [minBound .. maxBound], 
                                        q <- states aut,
                                        length (trans q a) > 0
                     ]
                     where trans = transition aut

-------------- JAK DOTĄD CHYBA OK --------------

accepts :: Eq q => Auto a q -> [a] -> Bool
accepts a w = accepts' a (initStates a) w

accepts' :: Eq q => Auto a q -> [q] -> [a] -> Bool
accepts' _ [] _ = False
accepts' a s [] = any (isAccepting a) s
accepts' a s (w:ws) = accepts' a s' ws
                      where s' = concat (map f s)
                            f = \x -> (transition a) x w
