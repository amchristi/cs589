module KarelSemantics where

-- Homework 5
-- id: christia
-- email: christia@oregonstate.edu
-- Completion: All tasks completed. Though not all tests are passing. 
-- Completion: 5 tests failing in KarelTest.
-- Collaboration: None
-- Online resources used: None
-- Submission: 11-22 at 11:20 PM.


import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState

-- take directon, cardinality, current position of robot and world as input and reutrn true or false.
func1 :: Dir -> Card -> (Int,Int) -> World -> Bool
func1 d car (xpos,ypos) w = case d of
								Right	-> w (xpos + 1, ypos) == Just 0
								Left    -> w (xpos -1, ypos) == Just 0
								Front 	-> w (xpos, ypos + 1) == Just 0
								Back 	-> w (xpos, ypos - 1) == Just 0

moveInPlace :: Dir -> Card -> Card
moveInPlace Right North = East
moveInPlace Left North = West
moveInPlace Front North = North
moveInPlace Back North = South
moveInPlace Right South = West
moveInPlace Left South = East
moveInPlace Front South = South
moveInPlace Back South = North
moveInPlace _ _ = East

func2 :: Int -> Stmt -> World -> Robot -> Result
func2 0 s w r = OK w r
func2 n s w r = case stmt s [] w r of
					OK w1 r1	-> func2 (n-1) s w1 r1 
					Error str	-> Error str
								
findMacroStmt :: String -> Defs -> Maybe Stmt
findMacroStmt str [] = Nothing
findMacroStmt str ((a,b): xs) = if a == str then Just b else findMacroStmt str xs
										


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) (w) (r)  = not (test t w r)
test (Facing x) (_) ((pos,car,y)) = x == car
test (Clear d) (w) (((x,y), car, z)) = func1 d car (x,y) w 
test (Beeper) (w) (((x,y), car, z)) = hasBeeper (x,y) (w)
test (Empty) (w) (((x,y), car, z)) = z == 0

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper _ w r = let p = getPos r
                       in if isEmpty r  
					          then Error ("Current robot has empty bag" ++ show r)
							  else OK (incBeeper p w) (decBag r)
							  
stmt (Turn d) _ w (((x,y), car, z)) = OK w (((x,y), cardTurn d car, z)) 	
stmt (Iterate n s ) _ w r = func2 n s w r 
stmt (If t s1 s2) defs1 w r = if test t w r
							then stmt s1 defs1 w r 
							else stmt s2 defs1 w r
stmt (While t s) defs1 w r = if test t w r 
								then OK w r
								else case stmt s defs1 w r of
										OK w1 r1 	-> stmt (While t s) defs1 w1 r1
										Error str	-> Error str
stmt (Block []) defs1 w r = OK w r										
stmt (Block (x:xs)) defs1 w r = case stmt x defs1 w r of 
										OK w1 r1	-> stmt (Block xs ) defs1 w1 r1
										Error str 	-> Error str
stmt (Call m) defs1 w r = case findMacroStmt m defs1 of 
								Just s1 -> stmt s1 defs1 w r
								Nothing -> Error ("Undefined macro: " ++ m)
stmt _ _ _ _ = undefined
    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r


