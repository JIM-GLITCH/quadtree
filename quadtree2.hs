


data QuadTree
  = Leaf Color
  | Internal QuadTree QuadTree QuadTree QuadTree
  deriving (Eq, Show)

data Color = Black | White
  deriving (Eq, Show)

allBlack :: QuadTree
allBlack = Leaf Black

allWhite :: QuadTree
allWhite = Leaf White

clockwise :: QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree
clockwise t1 t2 t3 t4 =
  simp $ Internal t1 t2 t3 t4

anticlockwise :: QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree
anticlockwise t1 t2 t3 t4 =
  simp $ Internal t1 t4 t3 t2

simp :: QuadTree -> QuadTree
simp tree = case tree of
  Leaf _ -> tree
  Internal t1 t2 t3 t4 ->
    let [t1', t2', t3', t4'] = map simp [t1, t2, t3, t4]
     in case (t1', t2', t3', t4') of
          (Leaf Black, Leaf Black, Leaf Black, Leaf Black) -> Leaf Black
          (Leaf White, Leaf White, Leaf White, Leaf White) -> Leaf White
          _ -> Internal t1' t2' t3' t4'

coarsework :: Int -> QuadTree -> QuadTree
coarsework n tree = case n of
  0 -> allWhite
  _ | n > 0 -> case tree of
    Leaf White -> Leaf White
    Leaf Black -> Leaf Black
    Internal a b c d ->
      Internal (f a) (f b) (f c) (f d)
      where
        f = coarsework (n - 1)
q :: QuadTree
q = clockwise allWhite allBlack allWhite q 
my_solution::(QuadTree->Bool)->QuadTree

-- >>>coarsework 10 q
-- Internal (Leaf White) (Leaf Black) (Leaf White) (Internal (Leaf White) (Leaf Black) (Leaf White) (Internal (Leaf White) (Leaf Black) (Leaf White) (Internal (Leaf White) (Leaf Black) (Leaf White) (Internal (Leaf White) (Leaf Black) (Leaf White) (Internal (Leaf White) (Leaf Black) (Leaf White) (Internal (Leaf White) (Leaf Black) (Leaf White) (Internal (Leaf White) (Leaf Black) (Leaf White) (Internal (Leaf White) (Leaf Black) (Leaf White) (Internal (Leaf White) (Leaf White) (Leaf White) (Leaf White))))))))))
