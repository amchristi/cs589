module HW2 where


--
-- * Name: Arpit Christi
-- * id: christia
-- * Completion: valueAt and mapTree are completed. pathTo is not completed.
-- * Reason and attempt: Logic is correct. Somehow I cannot convert and re convert between Maybe Path Maybe [Step] or [Maybe Step]
-- * Need to spend more time understanding maybe and just and look at more examples.  
-- * email: christia@oregonstate.edu
-- * collaboration: none
-- * online resources used: none



-- | One step in a path, indicating whether to follow the left subtree (L)
--   or the right subtree (R).
data Step = L | R
  deriving (Eq,Show)


-- | A path is just a sequence of steps. Each node in a binary tree can be
--   identified by a different path, indicating how to walk down the tree
--   starting from the root. See the examples for `valueAt`.
type Path = [Step]


-- | Binary trees with nodes labeled by an arbitrary type.
data Tree a = Node a (Tree a) (Tree a)
            | End
  deriving (Eq,Show)


-- | Create a leaf node.
leaf :: a -> Tree a
leaf x = Node x End End


-- | An example tree.
ex :: Tree Int
ex = Node 4 (Node 3 (leaf 2) End)
            (Node 7 (Node 5 End (leaf 6))
                    (leaf 8))


-- | Get the value at the node specified by a path.
--
--   >>> valueAt [] ex
--   Just 4
--
--   >>> valueAt [L,L] ex
--   Just 2
--
--   >>> valueAt [L,R] ex
--   Nothing
--
--   >>> valueAt [R,L,R] ex
--   Just 6
--
--   >>> valueAt [L,L,L] ex
--   Nothing
--
valueAt :: Path -> Tree a -> Maybe a
valueAt _ End = Nothing
valueAt  [] (Node x _ _) = Just x 
valueAt (x:xs) (Node _ ltree rtree)  
                                      | x == L = valueAt xs ltree
                                      | x == R = valueAt xs rtree 	

-- | Find a path to a node that contains the given value.
--
--   >>> pathTo 3 (leaf 5)
--   Nothing
--
--   >>> pathTo 5 ex
--   Just [R,L]
--
--   >>> pathTo 6 ex
--   Just [R,L,R]
--
--   >>> pathTo 4 ex
--   Just []
--
--   >>> pathTo 10 ex
--   Nothing
--


-- | InTree method testing
--
--   >>> inTree 3 (leaf 5)
--   False
--
--   >>> inTree 5 ex
--   True
--
--   >>> inTree 1000 ex
--   False
--
--   >>> inTree 2 End
--   False
--
--   >>> inTree 6 ex
--   True
--


inTree :: Eq a => a -> Tree a -> Bool
inTree _ End = False
inTree p (Node q ltree rtree) = (p == q) || (inTree p ltree) || (inTree p rtree)


              
--pathTo :: Eq a => a -> Tree a -> Maybe Path
--pathTo _ End = Nothing

--pathTo p (Node q  ltree rtree) 
--                       | p == q = Just [] 
--                       | inTree p ltree = Just(L) :  (pathTo p ltree)
--                       | inTree p rtree = Just(R) :  (pathTo p rtree)					   
--					   | otherwise = Nothing	

					   

-- | Apply a function to the value at every node in the tree.
--   
--   >>> mapTree odd End
--   End
--
--   >>> mapTree even (Node 5 (leaf 2) (leaf 3))
--   Node False (Node True End End) (Node False End End)
--
--   >>> (mapTree not . mapTree even) (Node 5 (leaf 2) (leaf 3))
--   Node True (Node False End End) (Node True End End)
--
--   >>> mapTree (+10) ex
--   Node 14 (Node 13 (Node 12 End End) End) (Node 17 (Node 15 End (Node 16 End End)) (Node 18 End End))
--
--   >>> ex == (mapTree (subtract 27) . mapTree (+27)) ex
--   True
--

mapTree :: (a -> b) -> (Tree a) -> (Tree b)
mapTree _ End = End
mapTree f (Node p ltree rtree)  = 
                                 (Node x (mapTree f ltree) (mapTree f rtree))
                                 where x = f p 								 