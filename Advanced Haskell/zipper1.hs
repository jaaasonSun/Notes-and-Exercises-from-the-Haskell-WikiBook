-- Zippers

data Node a = DeadEnd a | Passage a (Node a) | Fork a (Node a) (Node a)

get :: Node p -> p
get (DeadEnd a) = a
get (Passage a _) = a
get (Fork a _ _) = a

put :: a -> Node a -> Node a
put a (DeadEnd _) = DeadEnd a
put a (Passage _ n) = Passage a n
put a (Fork _ n1 n2) = Fork a n1 n2

labyrinth :: Node (Integer, Integer)
labyrinth = Fork (0, 2)
  (Fork (-2, 0) (DeadEnd (-1, 0)) (DeadEnd (0, -2)))
  (Passage (2, 0) (Fork (1, 0) (Passage (0, 1) (DeadEnd (0, 0))) (DeadEnd (0, -1))))

data Branch = KeepStrightOn | TurnLeft | TurnRight
type Thread = [Branch]

turnRigh :: [Branch] -> [Branch]
turnRigh t = t ++ [TurnRight]

retrieve :: [Branch] -> Node p -> p
retrieve [] n = get n
retrieve (KeepStrightOn : bs) (Passage _ n) = retrieve bs n
retrieve (TurnLeft : bs) (Fork _ n _) = retrieve bs n
retrieve (TurnRight : bs) (Fork _ _ n) = retrieve bs n

update :: (a -> a) -> [Branch] -> Node a -> Node a
update f [] n = put (f $ get n) n
update f (KeepStrightOn : bs) (Passage a n) = Passage a (update f bs n)
update f (TurnLeft : bs) (Fork a l r) = Fork a (update f bs l) r
update f (TurnRight : bs) (Fork a l r) = Fork a l (update f bs r)

