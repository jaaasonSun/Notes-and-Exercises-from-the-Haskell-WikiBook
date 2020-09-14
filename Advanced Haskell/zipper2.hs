type Zipper a = (Thread a, Node a)

data Branch a = KeepStraightOn a | TurnLeft a (Node a) | TurnRight a (Node a)
type Thread a = [Branch a]

data Node a = DeadEnd a | Passage a (Node a) | Fork a (Node a) (Node a)

turnRight :: Zipper a -> Maybe (Zipper a)
turnRight (t, Fork x l r) = Just (TurnRight x l : t, r)
turnRight _ = Nothing

turnLeft :: Zipper a -> Maybe (Zipper a)
turnLeft (t, Fork x l r) = Just (TurnLeft x r : t, l)
turnLeft _ = Nothing

keepStraightOn :: Zipper a -> Maybe (Zipper a)
keepStraightOn (t, Passage x n) = Just (KeepStraightOn x : t, n)

back :: Zipper a -> Maybe (Zipper a)
back ([], _) = Nothing
back (TurnRight x l : t, r) = Just (t, Fork x l r)
back (TurnLeft x r : t, l) = Just (t, Fork x l r)
back (KeepStraightOn x : t, n) = Just (t, Passage x n)

get :: Zipper a -> a
get (_, DeadEnd a) = a
get (_, Passage a _) = a
get (_, Fork a _ _) = a

put :: a -> Zipper a -> Zipper a
put a (t, DeadEnd _) = (t, DeadEnd a)
put a (t, Passage _ n) = (t, Passage a n)
put a (t, Fork _ l r) = (t, Fork a l r)

update :: (a -> a) -> Zipper a -> Zipper a
update f = put . f . get 
