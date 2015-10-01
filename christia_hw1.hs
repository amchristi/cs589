module HW1 where

--
-- * Name: Arpit Christi
-- * id: christia
-- * Completion: All problems completed.
-- * email: christia@oregonstate.edu
-- * collaboration: none
-- * online resources used: none




-- * Part 1: Binary trees
--

-- | Integer-labeled binary trees.
data Tree = Node Int Tree Tree   -- ^ Internal nodes
          | Leaf Int             -- ^ Leaf nodes
  deriving (Eq,Show)


-- | An example binary tree, which will be used in tests.
t1 :: Tree
t1 = Node 1 (Node 2 (Node 3 (Leaf 4) (Leaf 5))
                    (Leaf 6))
            (Node 7 (Leaf 8) (Leaf 9))

-- | Another example binary tree, used in tests.
t2 :: Tree
t2 = Node 6 (Node 2 (Leaf 1) (Node 4 (Leaf 3) (Leaf 5)))
            (Node 8 (Leaf 7) (Leaf 9))


-- | The integer at the left-most node of a binary tree.
--
--   >>> leftmost (Leaf 3)
--   3
--
--   >>> leftmost (Node 5 (Leaf 6) (Leaf 7))
--   6
--   
--   >>> leftmost t1
--   4
--
--   >>> leftmost t2
--   1
--
leftmost :: Tree -> Int
leftmost (Leaf i)     = i
leftmost (Node _ l _) = leftmost l


-- | The integer at the right-most node of a binary tree.
--
--   >>> rightmost (Leaf 3)
--   3
--
--   >>> rightmost (Node 5 (Leaf 6) (Leaf 7))
--   7
--   
--   >>> rightmost t1
--   9
--
--   >>> rightmost t2
--   9
--

rightmost :: Tree -> Int
rightmost (Leaf i) = i
rightmost (Node _ _ r) = rightmost r


-- | Get the maximum integer from a binary tree.
--
--   >>> maxInt (Leaf 3)
--   3
--
--   >>> maxInt (Node 5 (Leaf 4) (Leaf 2))
--   5
--
--   >>> maxInt (Node 5 (Leaf 7) (Leaf 2))
--   7
--
--   >>> maxInt t1
--   9
--
--   >>> maxInt t2
--   9
--

maxTwo :: Int -> Int -> Int
maxTwo a b | a >= b  = a
           | otherwise = b

maxThree :: Int -> Int -> Int -> Int
maxThree a b c | a >= maxTwo b c = a
               | otherwise = maxTwo b c

maxInt :: Tree -> Int
maxInt (Leaf i) = i
maxInt ( Node i l r) = maxThree i (maxInt l) (maxInt r)


-- | Get the minimum integer from a binary tree.
--
--   >>> minInt (Leaf 3)
--   3
--
--   >>> minInt (Node 2 (Leaf 5) (Leaf 4))
--   2
--
--   >>> minInt (Node 5 (Leaf 4) (Leaf 7))
--   4
--
--   >>> minInt t1
--   1
--
--   >>> minInt t2
--   1
--

minTwo :: Int -> Int -> Int
minTwo a b | a <= b  = a
           | otherwise = b

minThree :: Int -> Int -> Int -> Int
minThree a b c | a <= minTwo b c = a
               | otherwise = minTwo b c

minInt :: Tree -> Int
minInt (Leaf i) = i
minInt (Node i l r) = minThree i (minInt l) (minInt r)


-- | Get the sum of the integers in a binary tree.
--
--   >>> sumInts (Leaf 3)
--   3
--
--   >>> sumInts (Node 2 (Leaf 5) (Leaf 4))
--   11
--
--   >>> sumInts t1
--   45
--
--   >>> sumInts (Node 10 t1 t2)
--   100
--
sumInts :: Tree -> Int
sumInts (Leaf i) = i
sumInts (Node i l r) = i + sumInts(l) + sumInts(r)


-- | The list of integers encountered by a pre-order traversal of the tree.
--
--   >>> preorder (Leaf 3)
--   [3]
--
--   >>> preorder (Node 5 (Leaf 6) (Leaf 7))
--   [5,6,7]
--
--   >>> preorder t1
--   [1,2,3,4,5,6,7,8,9]
--
--   >>> preorder t2
--   [6,2,1,4,3,5,8,7,9]
--   
preorder :: Tree -> [Int]
preorder (Leaf i) =  i : []
preorder (Node i l r) = (i : []) ++ preorder (l)  ++ preorder(r)

-- | The list of integers encountered by an in-order traversal of the tree.
--
--   >>> inorder (Leaf 3)
--   [3]
--
--   >>> inorder (Node 5 (Leaf 6) (Leaf 7))
--   [6,5,7]
--
--   >>> inorder t1
--   [4,3,5,2,6,1,8,7,9]
--
--   >>> inorder t2
--   [1,2,3,4,5,6,7,8,9]
--   
inorder :: Tree -> [Int]
inorder (Leaf i) = i : []
inorder (Node i l r) = inorder (l) ++ (i : []) ++ inorder(r)


-- | Check whether a binary tree is a binary search tree.
--
--   >>> isBST (Leaf 3)
--   True
--
--   >>> isBST (Node 5 (Leaf 6) (Leaf 7))
--   False
--   >>> isBST(Node 6 (Leaf 5) (Leaf 7))
--   True
--   
--   >>> isBST t1
--   False
--
--   >>> isBST t2
--   True
--   
isBST :: Tree -> Bool
isBST (Leaf i) = True
isBST (Node i l r) = (maxInt(l) <= i && minInt(r) >= i) && isBST(l) && isBST(r)  


-- | Check whether a number is contained in a binary search tree.
--   (You may assume that the given tree is a binary search tree.)
--
--   >>> inBST 2 (Node 5 (Leaf 2) (Leaf 7))
--   True
--
--   >>> inBST 3 (Node 5 (Leaf 2) (Leaf 7))
--   False
--
--   >>> inBST 4 t2
--   True
--
--   >>> inBST 10 t2
--   False
--   
inBST :: Int -> Tree -> Bool
inBST i (Leaf x) = i == x
inBST i (Node x l r) = (i == x) || (inBST i l) || (inBST i r) 



--
-- * Part 2: Run-length lists
--


-- | Convert a regular list into a run-length list.
--

--   >>> appendBefore 1 [1,1,1,2,3,3,3,1,2,2,2,2]
--   [(4,1),(1,2),(3,3),(1,1),(4,2)]
--  
--   >>> appendBefore 5 [1,1,1,2,3,3,3,1,2,2,2,2]
--   [(1,5),(3,1),(1,2),(3,3),(1,1),(4,2)]




appendBefore :: Eq a => a -> [(Int,a)] -> [(Int,a)] 
appendBefore x list1 
                | x == element = [(num + 1,element)] ++ reminderlist
                | otherwise = [(1,x)] ++ list1
                 where 
                        (num,element) = head(list1)
                        reminderlist = tail(list1)


-- | Convert a regular list into a run-length list.
--
--   >>> compress [1,1,1,2,3,3,3,1,2,2,2,2]
--   [(3,1),(1,2),(3,3),(1,1),(4,2)]
-- 
--   >>> compress "Mississippi"
--   [(1,'M'),(1,'i'),(2,'s'),(1,'i'),(2,'s'),(1,'i'),(2,'p'),(1,'i')]
--


compress :: Eq a => [a] -> [(Int,a)]
compress [x] = [(1,x)]
compress (y : ys) = appendBefore y (compress ys)   

--
--   >>> decompressIndividual 5 'a'
--   "aaaaa"
--   >>> decompressIndividual 1 'b'
--   "b"
--  

decompressIndividual :: Eq a => Int -> a -> [a]
decompressIndividual 1 x = [x]
decompressIndividual num y = decompressIndividual (num - 1) y ++ [y]

-- | Convert a run-length list back into a regular list.
--
--   >>> decompress [(5,'a'),(3,'b'),(4,'c'),(1,'a'),(2,'b')]
--   "aaaaabbbccccabb"
--  
decompress :: Eq a => [(Int,a)] -> [a]
decompress [(num,x)] = decompressIndividual num x
decompress ((num1,x1): ys) = (decompressIndividual num1 x1) ++ (decompress ys)  