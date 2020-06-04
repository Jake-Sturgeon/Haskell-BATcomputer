{-
     COM2001 Spring Assignment 1
     Haskell Template
     (c) 2018 Mike Stannett
     Email: m.stannett@sheffield.ac.uk
-}

import Debug.Trace
type Input  = Int
type Output = Int

-- bs -> list of boxes
-- b -> a box
-- p -> list of intructions
 
class (Eq cfg) => ProgrammableComputer cfg where
  initialise   :: Program -> [Input] -> cfg
  getOutput    :: cfg -> Output
  acceptState  :: Program -> cfg -> Bool
  doNextMove   :: Program -> cfg -> cfg
  runFrom      :: Program -> cfg -> cfg
  runProgram   :: Program -> [Input] -> cfg
  -- Default implementation
  runProgram p is = runFrom p (initialise p is)


  
-- The BATcomputer has just 3 types of instruction
-- CLR b        == empty box b
-- INC b        == add a token to box b
-- JEQ b1 b2 t  == if boxes b1 and b2 contain the same 
--                 number of tokens, jump to instruction t
--
data Instruction
  = CLR {box :: Int} 
  | INC {box :: Int}
  | JEQ {box1   :: Int, 
         box2   :: Int,
         target :: Int}
  deriving (Eq, Show)
   
type Program = [Instruction]

{- 

TEST 1      Check if empty list returns correct output 
INPUT:      maxBoxNum []
EXPECTED:   0
RESULT:     0
PASS?       TRUE

TEST 2      Check if normal input is handled
INPUT:      maxBoxNum [CLR 10]
EXPECTED:   10
RESULT:     10
PASS?       TRUE

TEST 3      Check if a normal input of list of more than one instruction
INPUT:      maxBoxNum [CLR 10, INC 110]
EXPECTED:   110
RESULT:     110
PASS?       TRUE

TEST 4      Check if JEQ is handled corectly
INPUT:      maxBoxNum [INC 110, JEQ 100 120 1,CLR 10]
EXPEXTED:   120
RESULT:     120
PASS?       TRUE


TEST 4      Check if JEQ jump is ignored
INPUT:      maxBoxNum [INC 110, JEQ 100 120 300,CLR 10]
EXPEXTED:   120
RESULT:     120
PASS?       TRUE

-}
maxBoxNum :: Program -> Int
maxBoxNum [] = 0                                        -- Return 0 if the list is empty
maxBoxNum p = maximum $ map maxBoxNumA p                -- return the largest number in the list
 where maxBoxNumA (JEQ b1 b2 _) = maximum [b1, b2]      -- In the case of a JEQ return the largest number
       maxBoxNumA p = box p                             -- Otherwise, just return the number

data BATConfig = BATConfig {
    boxes   :: [Int],
    counter :: Int
    } deriving (Eq)

{-
TEST 1      Check if normal case works  
INPUT:      show (BATConfig [1,2,3,4] 2)
EXPECTED:   "Boxes = [1,2,3,4]; Counter = 2"   
RESULT:     "Boxes = [1,2,3,4]; Counter = 2"
PASS?       TRUE

TEST 2      Check if an empty list is correctly handled
INPUT:      show (BATConfig [] 0)
EXPECTED:   "Boxes = []; Counter = 0"  
RESULT:     "Boxes = []; Counter = 0"
PASS?       TRUE
-}
instance Show BATConfig where
 show (BATConfig boxes counter) = "Boxes = " ++ show boxes ++ "; Counter = " ++ show counter
  


instance ProgrammableComputer BATConfig  where
    {-

    TEST 1      Check if normal initialise is handled
    INPUT:      initialise [CLR 1, INC 1] [1, 2] :: BATConfig
    EXPECTED:   Boxes = [0,0]; Counter = 0
    RESULT:     Boxes = [0,0]; Counter = 0
    PASS?:      TRUE

    TEST 2      Check if empty boxes is handled
    INPUT:      initialise [CLR 1, INC 1] [] :: BATConfig
    EXPECTED:   Boxes = [0,0]; Counter = 0
    RESULT:     Boxes = [0,0]; Counter = 0
    PASS?:      TRUE

    TEST 3      Check if empty program is handled
    INPUT:      initialise [] [1, 2] :: BATConfig
    EXPECTED:   Boxes = [0,1,2]; Counter = 0
    RESULT:     Boxes = [0,1,2]; Counter = 0
    PASS?:      TRUE

    TEST 4      Check if both empty is handled
    INPUT:      initialise [] [] :: BATConfig
    EXPECTED:   Boxes = [0,0]; Counter = 0
    RESULT:     Boxes = [0,0]; Counter = 0
    PASS?:      TRUE

    TEST 5      Check if more boxes are added if needed 
    INPUT:      initialise [CLR 1, INC 1, CLR 10] [1,2,3,4,5] :: BATConfig
    EXPECTED:   Boxes = [0,1,2,3,4,5,0,0,0,0,0]; Counter = 0
    RESULT:     Boxes = [0,1,2,3,4,5,0,0,0,0,0]; Counter = 0
    PASS?:      TRUE

    -}

    initialise [] [] = BATConfig [0,0] 0
    initialise p bs = BATConfig (0:bs ++ take (maxBoxNum p - length bs)(repeat 0)) 0    -- Init the BAT Computer with a counter of 0 and create box 0                                                                                           
                                                                                        -- create boxes according to instructions used
    {- FOR ALL CASES BOX 0 IS ADDED TO FRONT OF LIST

    TEST 1      Check when c = 0
    INPUT:      acceptState [CLR 1, INC 1] (BATConfig [0,1] 0) :: Bool
    EXPECTED:   False
    RESULT:     False
    PASS?:      TRUE
    
    TEST 2      Check when c = 1
    INPUT:      acceptState [CLR 1, INC 1] (BATConfig [0,1] 1) :: Bool
    EXPECTED:   False
    RESULT:     False
    PASS?:      TRUE

    TEST 3      Check when c == p
    INPUT:      acceptState [CLR 1, INC 1] (BATConfig [0,1] 2) :: Bool
    EXPECTED:   True
    RESULT:     True
    PASS?:      TRUE

    TEST 3      Check when c > p
    INPUT:      acceptState [CLR 1, INC 1] (BATConfig [0,1] 3) :: Bool
    EXPECTED:   True
    RESULT:     True
    PASS?:      TRUE

    -}

    acceptState p (BATConfig _ c) = c >= length p || c < 0          -- Set the accept state to where c is greater than the number of instructions                                                   

    {- FOR ALL CASES BOX 0 IS ADDED TO FRONT OF LIST
    
    TEST 1      Check When p is empty
    INPUT:      doNextMove [] (BATConfig [0,1,2,3] 0)
    EXPECTED:   Boxes = [0,1,2,3]; Counter = 0
    RESULT:     Boxes = [0,1,2,3]; Counter = 0
    PASS?:      TRUE

    TEST 2      Check When cfg is in an accept state
    INPUT:      doNextMove [CLR 1, CLR 1, CLR 1] (BATConfig [0,1,2,3] 3)
    EXPECTED:   Boxes = [0,1,2,3]; Counter = 3
    RESULT:     Boxes = [0,1,2,3]; Counter = 3
    PASS?:      TRUE

    TEST 3      Check if CLR instruction is handled
    INPUT:      doNextMove [CLR 1] (BATConfig [0,1,2,3] 0)
    EXPECTED:   Boxes = [0,0,2,3]; Counter = 1
    RESULT:     Boxes = [0,0,2,3]; Counter = 1
    PASS?:      TRUE

    TEST 4     Check if INC instruction is handled
    INPUT:      doNextMove [CLR 1] (BATConfig [0,1,2,3] 0)
    EXPECTED:   Boxes = [0,2,2,3]; Counter = 1
    RESULT:     Boxes = [0,2,2,3]; Counter = 1
    PASS?:      TRUE

    TEST 5      Check if JEQ false instruction is handled
    INPUT:      doNextMove [JEQ 1 2 3] (BATConfig [0,1,2,3] 0)
    EXPECTED:   Boxes = [0,1,2,3]; Counter = 1
    RESULT:     Boxes = [0,1,2,3]; Counter = 1
    PASS?:      TRUE

    TEST 6      Check if JEQ true instruction is handled
    INPUT:      doNextMove [JEQ 1 2 3] (BATConfig [0,1,1,3] 0)
    EXPECTED:   Boxes = [0,1,1,3]; Counter = 3
    RESULT:     Boxes = [0,1,1,3]; Counter = 3
    PASS?:      TRUE

    -}

    doNextMove p cfg@(BATConfig bs c)
     | null p = cfg                                                                                             -- if the instruction list is empty, return the current state f cfg
     | acceptState p cfg = cfg                                                                                  -- if an accept state, return current state of cfg
     | otherwise = BATConfig bs' c'                                                                             -- otherwise, change the currnt cfg                                
     where BATConfig bs' c' = case p !! c of 
            CLR b           -> BATConfig (nlis b 0) $ c + 1                                                      -- if currnet instruction is CLR, then clear b and inc counter
            INC b           -> BATConfig (nlis b $ getTokens b + 1) $ c + 1                                      -- if currnet instruction is INC, then inc b by 1 and inc counter
            JEQ b1 b2 t     -> if getTokens b1 == getTokens b2 then BATConfig bs t else BATConfig bs $ c + 1     -- if currnet instruction is JEQ and b1 == b2 then jump to instruciton t
           getTokens b = bs !! b                                                                                -- used to get the current token count in box b
           nlis b b' = front ++ b':tail                                                                         -- create the new list of boxes
            where (front, _:tail) = splitAt b bs

    {- FOR ALL CASES BOX 0 IS ADDED TO FRONT OF LIST

    TEST 1      Check if empty p is handled
    INPUT:      runFrom [] (BATConfig [0,1,2,3] 0)
    EXPECTED:   Boxes = [0,1,2,3]; Counter = 0
    RESULT:     Boxes = [0,1,2,3]; Counter = 0
    PASS?:      TRUE

    TEST 2      Check if acceptstate is handled
    INPUT:      runFrom [CLR 1, CLR 1, CLR 1] (BATConfig [0,1,2,3] 3)
    EXPECTED:   Boxes = [0,1,2,3]; Counter = 3
    RESULT:     Boxes = [0,1,2,3]; Counter = 3
    PASS?:      TRUE

    TEST 3      Check normal input
    INPUT:      runFrom [INC 1] (BATConfig [0,1,2,3] 0)
    EXPECTED:   Boxes = [0,2,2,3]; Counter = 1
    RESULT:     Boxes = [0,2,2,3]; Counter = 1
    PASS?:      TRUE

    -}
    runFrom p cfg
     | null p = cfg                                                         -- if the instruction list is empty, return the current state f cfg
     | acceptState p cfg = cfg                                              -- if an accept state, return current state of cfg
     | otherwise = runFrom p $ doNextMove p cfg                             -- otherwise, do next move

    
    {- FOR ALL CASES BOX 0 IS ADDED TO FRONT OF LIST

    TEST 1      Check if it handles the normal case
    INPUT:      getOutput (BATConfig [0,1] 0)
    EXPECTED:   1
    RESULT:     1
    PASS?:      TRUE

    TEST 2      Check if system fails when there is no box 1
    INPUT:      getOutput (BATConfig [0] 0)
    EXPECTED:   ERROR
    RESULT:     ERROR
    PASS?:      SUCCESS AS THE PROGRAM SHOULD ALWAYS HAVE BOX 1.
                GIVING THE OUTPUT A VALUE COULD LEAD TO UNPREDICTABLE
                  REULTS LATER

    -}  
    getOutput cfg@(BATConfig (_: b :_) _ ) = traceShow (cfg) b                          -- Output the number of token is box 1

-- This function is included to help with testing. Running
-- "execute p xs" should show the output generated when
-- running program p with user input(s) xs  
execute :: Program -> [Input] -> Output
execute p ins = getOutput ((runProgram p ins) :: BATConfig)

{-

TEST 1      Check empty list 
INPUT:      transpose 10 []
EXPECTED:   []
RESULT:     []
PASS?       TRUE

TEST 2      To see that INC and CLR are not adjusted
INPUT:      transpose 10 [INC 1, CLR 1]
EXPECTED:   [INC {box = 1},CLR {box = 1}]
RESULT:     [INC {box = 1},CLR {box = 1}]
PASS?       TRUE

TEST 3      To see that JEQ is adjusted
INPUT:      transpose 10 [INC 1, CLR 1, JEQ 1 1 1]
EXPECTED:   [INC {box = 1},CLR {box = 1},JEQ {box1 = 1, box2 = 1, target = 11}]
RESULT:     [INC {box = 1},CLR {box = 1},JEQ {box1 = 1, box2 = 1, target = 11}]
PASS?       TRUE

TEST 4      To see that all JEQs are adjusted
INPUT:      transpose 10 [JEQ 1 1 1, JEQ 1 1 1]
EXPECTED:   [JEQ {box1 = 1, box2 = 1, target = 11},JEQ {box1 = 1, box2 = 1, target = 11}]
RESULT:     [JEQ {box1 = 1, box2 = 1, target = 11},JEQ {box1 = 1, box2 = 1, target = 11}]
PASS?       TRUE

-}
transpose :: Int -> Program -> Program
transpose _ [] = []
transpose n p = map convert p                           -- if the instruction is JEQ then added n to t
 where convert (JEQ b1 b2 t) = JEQ b1 b2 (t + n)
       convert p = p  



{- join two programs together, so as to run one
-- after the other

TEST 1      To see that two empty list are handled
INPUT:      [] *->* []
EXPECTED:   []
RESULT:     []
PASS?       TRUE

TEST 2      To see that an empty list can be joined to a program
INPUT:      [] *->* [CLR 1]
EXPECTED:   [CLR {box = 1}]
RESULT:     [CLR {box = 1}]
PASS?       TRUE

TEST 3      To see that an empty list doesn't adjust JEQ
INPUT:      [] *->* [JEQ 1 1 1]
EXPECTED:   [JEQ {box1 = 1, box2 = 1, target = 1}]
RESULT:     [JEQ {box1 = 1, box2 = 1, target = 1}]
PASS?       TRUE

TEST 4      To see that a program can be joined to an empty list
INPUT:      [INC 1] *->* [] 
EXPECTED:   [INC {box = 1}]
RESULT:     [INC {box = 1}]
PASS?       TRUE

TEST 5      To see that CLR and INC are not effected
INPUT:      [INC 1, CLR 1] *->* [INC 1, CLR 1] 
EXPECTED:   [INC {box = 1},CLR {box = 1},INC {box = 1},CLR {box = 1}]
RESULT:     [INC {box = 1},CLR {box = 1},INC {box = 1},CLR {box = 1}]
PASS?       TRUE

TEST 6      To see that the first program's JEQ is not effected
INPUT:      [JEQ 1 1 1] *->* [INC 1] 
EXPECTED:   [JEQ {box1 = 1, box2 = 1, target = 1},INC {box = 1}]
RESULT:     [JEQ {box1 = 1, box2 = 1, target = 1},INC {box = 1}]
PASS?       TRUE

TEST 7      To see that the second program's JEQ is effected
INPUT:      [JEQ 1 1 1] *->* [JEQ 1 1 1] 
EXPECTED:   [JEQ {box1 = 1, box2 = 1, target = 1},JEQ {box1 = 1, box2 = 1, target = 2}]
RESULT:     [JEQ {box1 = 1, box2 = 1, target = 1},JEQ {box1 = 1, box2 = 1, target = 2}]
PASS?       TRUE

-}

(*->*) :: Program -> Program -> Program
p1 *->* p2 = p1 ++ transpose (length p1) p2             -- Concat p1 onto a transposed p2



{- program to compute B1 = B1 + B2

CLR 0        -->    CLR 0 to make sure 0 is empty
JEQ 0 2 5    -->    if box 0 == box 2 then jump to instruction 4
INC 0        -->    Increment box 0 by 1
INC 0        -->    Increment box 1 by 1
JEQ 0 0 0    -->    jump to instuction 0
CLR 0        -->    Clear box 0 and halt


A TRACE OF THE LAST STATE OF THE MACHINE WILL BE GIVEN FOR ALL TESTS

TEST 1      Normal case
INPUT:      execute adder [5, 5]
EXPECTED:   10      TRACE  ->  Boxes = [0,10,5]; Counter = 6
RESULT:     10      TRACE  ->  Boxes = [0,10,5]; Counter = 6
PASS?       TRUE

TEST 2      TWO 0's can be handled
INPUT:      execute adder [0, 0]
EXPECTED:   0       TRACE  ->  Boxes = [0,0,0]; Counter = 6
RESULT:     0       TRACE  ->  Boxes = [0,0,0]; Counter = 6
PASS?       TRUE

TEST 3      Number can be added to 0
INPUT:      execute adder [5, 0]
EXPECTED:   5       TRACE  ->  Boxes = [0,5,0]; Counter = 6
RESULT:     5       TRACE  ->  Boxes = [0,5,0]; Counter = 6
PASS?       TRUE

TEST 4      Number can be added to 0
INPUT:      execute adder [0,5]
EXPECTED:   5       TRACE  ->  Boxes = [0,5,5]; Counter = 6
RESULT:     5       TRACE  ->  Boxes = [0,5,5]; Counter = 6
PASS?       TRUE

TEST 5      To see that to add an empty list doesnt break the program
INPUT:      execute adder []
EXPECTED:   0       TRACE  ->  Boxes = [0,0,0]; Counter = 6
RESULT:     0       TRACE  ->  Boxes = [0,0,0]; Counter = 6
PASS?       TRUE

TEST 6      To see that you can add 1 numer
INPUT:      execute adder [1]
EXPECTED:   1       TRACE  ->  Boxes = [0,1,0]; Counter = 6
RESULT:     1       TRACE  ->  Boxes = [0,1,0]; Counter = 6
PASS?       TRUE

-}

adder :: Program
adder = [CLR 0, JEQ 0 2 5, INC 0, INC 1, JEQ 0 0 1, CLR 0]
    
{- create a program to copy the contents of box m to box n (leave box m unchanged)

JEQ m n 5    -->    if box m == box n then jump to instruction 5
CLR n        -->    Clear box n
INC n        -->    Increment box n by 1
JEQ m m 5    -->    if box m == box n then jump to instruction 5
JEQ 0 0 2    -->    Jump to instruction 2
CLR 0        -->    Clear box 0 and halt

A TRACE OF THE LAST STATE OF THE MACHINE WILL BE GIVEN FOR ALL TESTS

TEST 1      Normal case for copy
INPUT:      execute (copyBox 2 1) [1, 2]
EXPECTED:   2       TRACE  ->  Boxes = [0,2,2]; Counter = 7
RESULT:     2       TRACE  ->  Boxes = [0,2,2]; Counter = 7
PASS?       TRUE

TEST 2      Copy same box
INPUT:      execute (copyBox 1 1) [1, 2]
EXPECTED:   1       TRACE  ->  Boxes = [0,1,2]; Counter = 7
RESULT:     1       TRACE  ->  Boxes = [0,1,2]; Counter = 7
PASS?       TRUE

TEST 3      To see that copy 0 0 works
INPUT:      execute (copyBox 0 0) [1, 2]
EXPECTED:   1       TRACE  ->  Boxes = [0,1,2]; Counter = 7
RESULT:     1       TRACE  ->  Boxes = [0,1,2]; Counter = 7
PASS?       TRUE

TEST 4      To see that copyBox 0 1
INPUT:      execute (copyBox 0 1) [1, 2]
EXPECTED:   0       TRACE  ->  Boxes = [0,0,2]; Counter = 7
RESULT:     0       TRACE  ->  Boxes = [0,0,2]; Counter = 7
PASS?       TRUE

TEST 5      T0 see that Copy 1 0 works
INPUT:      execute (copyBox 1 0) [1, 2]
EXPECTED:   1       TRACE  ->  Boxes = [0,1,2]; Counter = 7
RESULT:     1       TRACE  ->  Boxes = [0,1,2]; Counter = 7
PASS?       TRUE

-}

copyBox :: Int -> Int -> Program
copyBox m n = [JEQ m n 5, CLR n, JEQ m n 6, INC n, JEQ m n 6, JEQ 0 0 2, CLR 0]


{- program to compute B1 = Bx + By

IF x == y
Copy the contents of box x into 1
CLR 0        -->    CLR 0 to make sure 0 is empty
JEQ 0 1 6    -->    if box 0 == box 1 then jump to instruction 5
INC 0        -->    Increment box 0 by 1
INC 0        -->    Increment box 0 by 1
INC 1        -->    Increment box 1 by 1
JEQ 0 0 0    -->    Jump to instruction 0
CLR 0        -->    Clear box 0 and halt

IF y == 1
CLR 0        -->    CLR 0 to make sure 0 is empty
JEQ 0 x 5    -->    if box 0 == box x then jump to instruction 5
INC 0        -->    Increment box 0 by 1
INC 1        -->    Increment box 1 by 1
JEQ 0 0 0    -->    Jump to instruction 0
CLR 0        -->    Clear box 0 and halt

Otherwise
Copy the contents of box x into 1
CLR 0        -->    CLR 0 to make sure 0 is empty
JEQ 0 y 5    -->    if box 0 == box y then jump to instruction 5
INC 0        -->    Increment box 0 by 1
INC 1        -->    Increment box 1 by 1
JEQ 0 0 0    -->    Jump to instruction 0
CLR 0        -->    Clear box 0 and halt

A TRACE OF THE LAST STATE OF THE MACHINE WILL BE GIVEN FOR ALL TESTS

TEST 1      Normal case
INPUT:      execute (addXY 2 3) [1, 2, 3, 4]
EXPECTED:   5       TRACE  ->  Boxes = [0,5,2,3,4]; Counter = 13
RESULT:     5       TRACE  ->  Boxes = [0,5,2,3,4]; Counter = 13
PASS?       TRUE

TEST 2      Special case of box 1 and box 1
INPUT:      execute (addXY 2 3) [1, 2, 3, 4]
EXPECTED:   2       TRACE  ->  Boxes = [0,2,2,3,4]; Counter = 14
RESULT:     2       TRACE  ->  Boxes = [0,2,2,3,4]; Counter = 14
PASS?       TRUE

TEST 3      Special case of box 2 and box 1
INPUT:      execute (addXY 2 1) [1,2,3,4]
EXPECTED:   3       TRACE  ->  Boxes = [0,3,2,3,4]; Counter = 6
RESULT:     3       TRACE  ->  Boxes = [0,3,2,3,4]; Counter = 6
PASS?       TRUE

TEST 4      Special case of box 1 and box 0
INPUT:      execute (addXY 1 0) [1,2,3,4]
EXPECTED:   1       TRACE  ->  Boxes = [0,1,2,3,4]; Counter = 13
RESULT:     1       TRACE  ->  Boxes = [0,1,2,3,4]; Counter = 13
PASS?       TRUE

TEST 5      Special case of box 0 and box 1
INPUT:      execute (addXY 0 1) [1,2,3,4]
EXPECTED:   1       TRACE  ->  Boxes = [0,1,2,3,4]; Counter = 6
RESULT:     1       TRACE  ->  Boxes = [0,1,2,3,4]; Counter = 6
PASS?       TRUE

TEST 6      Special case of box 0 and box 0
INPUT:      execute (addXY 0 0) [1,2,3,4]
EXPECTED:   0       TRACE  ->  [0,0,2,3,4]; Counter = 14
RESULT:     0       TRACE  ->  [0,0,2,3,4]; Counter = 14
PASS?       TRUE

-}

addXY :: Int -> Int -> Program
addXY x y 
 | x == y = copy *->* [CLR 0, JEQ 0 1 6, INC 0, INC 0, INC 1, JEQ 0 0 1, CLR 0]
 | y == 1 = [CLR 0, JEQ 0 x 5, INC 0, INC 1, JEQ 0 0 1, CLR 0] 
 | otherwise = copy *->* [CLR 0, JEQ 0 y 5, INC 0, INC 1, JEQ 0 0 1, CLR 0] 
 where copy = copyBox x 1


