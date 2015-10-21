
module KarelLang where

--
-- * Name: Arpit Christi
-- * id: christia
-- * Completion: All tasks completed.
-- * email: christia@oregonstate.edu
-- * collaboration: none
-- * online resources used: none


import Prelude 

import Data.List (group)





-- part 1 : start
  
type Macro = String

data Prog = DefineMainAs Defs Stmt

data Def = DefAs Macro Stmt

type Defs = [Def]

data Card = North
            | East
			| South
            | West
     deriving(Show) 			


	 
	 
data Dir = Front
            | Back
			| Right
            | Left
     deriving(Show)
	 
	 
data Test = Not Test
            | Facing Card
            | Clear Dir
            | Beeper
            | Empty
  			
data Stmt = Shutdown
          | Move
          | Pickbeeper
          | Putbeeper
          | Turn Dir
          | Call Macro
          | Iteratetimes Int Stmt
          | Ifthenelse Test Stmt Stmt
          | Whiledo Test 	Stmt
          | Beginend Stmts

type Stmts = [Stmt]		  

-- part 1 : end







-- part 2 : start

-- bottom part of program
stmt1 :: Stmt
stmt1 = Pickbeeper

fetch :: Macro
fetch = "fetch"

stmt2 :: Stmt
stmt2 = Call fetch

stmt3 :: Stmt
stmt3 = Turn Back

stmt4 :: Stmt
stmt4 = Shutdown



stmtBeginEnd1 :: Stmt
stmtBeginEnd1 = Beginend [stmt1, stmt2, stmt3, stmt2, stmt1, stmt1,stmt4]

-- End: bottom part of program

-- top part of the program
ifstmt :: Stmt
ifstmt = Ifthenelse (Clear Front) (Move) (Shutdown)

-- End: top part of the program
  
whiledostmt :: Stmt
whiledostmt = Whiledo (Not Beeper)(ifstmt)

stmtBeginEnd2 :: Stmt
stmtBeginEnd2 = Beginend [whiledostmt, stmt1]

macroDefinition :: Def
macroDefinition = DefAs fetch stmtBeginEnd2

fetcher :: Prog
fetcher = DefineMainAs ([macroDefinition]) (stmtBeginEnd1)  
  
  
-- part 2 : end





-- part 3 : start


func1 :: Def -> Macro
func1 (DefAs (a) (_)) = a

macros :: Prog -> [Macro]
macros (DefineMainAs (def2)(_)) = map (func1) (def2)

-- part 3 : end


-- part 4 : start
-- assumption: there are no walls  during the move of the robot. 


nStepmove :: Int -> Stmt
nStepmove x = Iteratetimes x Move

rectanble :: Int -> Int -> Stmt

rectanble a b = (Beginend ([(nStepmove a), Turn KarelLang.Right,  (nStepmove b), Turn KarelLang.Right, (nStepmove a), Turn KarelLang.Right, (nStepmove b)]))

-- part 4 : end
  
  

