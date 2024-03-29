-- Copyright (C) 2019-2021 Data Ductus AB
--
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

module MRO where

type Name                       = String
type Bases                      = [Name]
type ClassDef                   = (Name,Bases)
type Graph                      = [ClassDef]

type WName                      = ([Maybe Name],Name)
type WBases                     = [WName]
type WClassDef                  = (Name,WBases)
type WGraph                     = [WClassDef]



-- With witnesses:

mro                             :: Graph -> IO ()
mro graph                       = do mapM (putStrLn . uncurry showclass) graph
                                     putStrLn "----"
                                     mro' [] graph

mro'                            :: WGraph -> Graph -> IO ()
mro' lins []                    = return ()
mro' lins ((c,bases):graph)     = case merge [] (map lin wbases ++ [wbases]) of
                                    Right cs -> do
                                        putStrLn (showlin c cs)
                                        mro' ((c,cs):lins) graph
                                    Left err -> putStrLn err
  where
    wbases                      = case bases of [] -> []; n:ns -> ([Nothing],n) : [ ([Just n],n) | n <- ns ]
    
    lin                         :: WName -> WBases
    lin (is,n)                  = case lookup n lins of
                                    Just la -> (is,n) : [ (is++w,x) | (w,x) <- la ]
                                    Nothing -> error ("Forward ref from " ++ c ++ " to " ++ n)

    merge                       :: WBases -> [WBases] -> Either String WBases
    merge out lists
      | null heads              = Right $ reverse out
      | h:_ <- good             = merge (h:out) [ if snd hd == snd h then tl else hd:tl | (hd,tl) <- zip heads tails ]
      | otherwise               = Left (">>>>>> " ++ showlin c (reverse out) ++ 
                                        " ++ merge(" ++ commasep id (map showlist lists) ++ ") <<<<<<<")
      where (heads,tails)       = unzip [ (hd,tl) | hd:tl <- lists ]
            good                = [ h | h <- heads, all (snd h `notElem`) (map (map snd) tails) ]

showlin                         :: Name -> WBases -> String
showlin c cs                    = "L(" ++ c ++ ") = " ++ showlist cs

showlist cs                     = "[" ++ commasep wshow cs ++ "]"

wshow (w,x)                     = wsh w ++ ": " ++ x
  where wsh []                  = ""
        wsh [n]                 = wsh' n
        wsh (n:w)               = wsh' n ++ "." ++ wsh w
        wsh' Nothing            = "_"
        wsh' (Just n)           = n

commasep f []                   = ""
commasep f [x]                  = f x
commasep f (x:xs)               = f x ++ ", " ++ commasep f xs

showclass c bases               = c ++ "(" ++ commasep id bases ++ ")"


-- Alternative formulation:

type Env                        = [WClassDef]

linearize                       :: Env -> ClassDef -> WClassDef
linearize env (c,bases)         = (c, merge [] $ map lin wbases ++ [wbases])
  where 
    wbases                      = case bases of [] -> []; n:ns -> ([Nothing],n) : [ ([Just n],n) | n <- ns ]

    lin                         :: WName -> [WName]
    lin (w,a)                   = (w,a) : [ (w++w',x) | (w',x) <- la ]
      where Just la             = lookup a env

    merge                       :: [WName] -> [[WName]] -> [WName]
    merge out lists
      | null heads              = reverse out
      | h:_ <- good             = merge (h:out) [ if equal hd h then tl else hd:tl | (hd,tl) <- zip heads tails ]
      | otherwise               = error (">>>>>> " ++ showlin c (reverse out) ++ 
                                         " ++ merge(" ++ commasep id (map showlist lists) ++ ") <<<<<<<")
      where (heads,tails)       = unzip [ (hd,tl) | hd:tl <- lists ]
            good                = [ h | h <- heads, all (absent h) tails ]
    
    equal                       :: WName -> WName -> Bool
    equal (_,a) (_,b)           = a == b
    
    absent                      :: WName -> [WName] -> Bool
    absent (_,h) tl             = h `notElem` map snd tl

mro2                            :: Env -> Graph -> IO ()
mro2 env []                     = mapM (putStrLn . uncurry showlin) (reverse env) >> return ()
mro2 env (g:graph)              = mro2 (lmap:env) graph
  where lmap                    = linearize env g



-- Examples from https://www.python.org/download/releases/2.3/mro/

ex0 = [("O",[]), ("X",["O"]), ("Y",["O"]), ("A",["X","Y"]), ("B",["Y","X"]), ("C",["A","B"])]               -- fails

ex1 = [("O",[]), ("F",["O"]), ("E",["O"]), ("D",["O"]), ("C",["D","F"]), ("B",["D","E"]), ("A",["B","C"])]  -- ok

ex2 = [("O",[]), ("F",["O"]), ("E",["O"]), ("D",["O"]), ("C",["D","F"]), ("B",["E","D"]), ("A",["B","C"])]  -- ok

ex3 = ex0                                                               -- fails

ex4 = [("O",[]), ("F",["O"]), ("E",["F"]), ("G",["F","E"])]             -- fails

ex5 = [("O",[]), ("F",["O"]), ("E",["F"]), ("G",["E","F"])]             -- ok

ex6 = [("O",[]), ("A",["O"]), ("C",["A","A"])]                          -- fails

ex7 = [("O",[]), ("A",["O"]), ("B",["O"]), ("C",["O"]), ("D",["O"]), ("E",["O"]), 
       ("K1",["A","B","C"]), ("K2",["D","B","E"]), ("K3",["D","A"]), 
       ("Z",["K1","K2","K3"])]                                          -- ok



ex8a = [("A",[]), ("B",[]), ("C",[]), ("D",[]), ("E",[]), ("Z",[]),
        ("K1",["A","B","C"]), ("K2",["D","B","E"]), ("K3",["D","A"]), 
        ("Z1",["Z","K1"]), ("Z2",["Z","K2"]), ("Z3",["Z","K3"]),
        ("Z_tot",["Z3","Z2","Z1"])]                                     -- ok

ex8b = [("A",[]), ("B",[]), ("C",[]), ("D",[]), ("E",[]), ("Z",[]),
        ("K1",["A","B","C"]), ("K2",["D","B","E"]), ("K3",["D","A"]), 
        ("Z1",["Z","K1","K2"]), ("Z2",["Z","K3"]),
        ("Z_tot",["Z2","Z1"])]                                          -- fails

ex8c = [("A",[]), ("B",[]), ("C",[]), ("D",[]), ("E",[]), ("Z",[]),
        ("K1",["A","B","C"]), ("K2",["D","B","E"]), ("K3",["D","A"]), 
        ("Z1",["Z","K1","K3"]), ("Z2",["Z","K2"]),
        ("Z_tot",["Z2","Z1"])]                                          -- ok

ex8d = [("A",[]), ("B",[]), ("C",[]), ("D",[]), ("E",[]), ("Z",[]),
        ("K1",["A","B","C"]), ("K2",["D","B","E"]), ("K3",["D","A"]), 
        ("Z1",["Z","K2","K3"]), ("Z2",["Z","K1"]), 
        ("Z_tot",["Z1","Z2"])]                                          -- fails

ex8e = [("A",[]), ("B",[]), ("C",[]), ("D",[]), ("E",[]), ("Z",[]),
        ("K1",["A","B","C"]), ("K2",["D","B","E"]), ("K3",["D","A"]), 
        ("Z1",["Z","K1"]), ("Z2",["Z","K2","K3"]),
        ("Z_tot",["Z2","Z1"])]                                          -- fails

ex8f = [("A",[]), ("B",[]), ("C",[]), ("D",[]), ("E",[]), ("Z",[]),
        ("K1",["A","B","C"]), ("K2",["D","B","E"]), ("K3",["D","A"]), 
        ("Z1",["Z","K2"]), ("Z2",["Z","K1","K3"]),
        ("Z_tot",["Z2","Z1"])]                                          -- ok

ex8g = [("A",[]), ("B",[]), ("C",[]), ("D",[]), ("E",[]), ("Z",[]),
        ("K1",["A","B","C"]), ("K2",["D","B","E"]), ("K3",["D","A"]), 
        ("Z1",["Z","K3"]), ("Z2",["Z","K1","K2"]),
        ("Z_tot",["Z2","Z1"])]                                          -- fails


ex8h = [("A",[]), ("B",[]), ("C",[]), ("D",[]), ("E",[]), 
        ("K1",["A","B","C"]), ("K2",["D","B","E"]), ("K3",["D","A"]), 
        ("Z1",["K1"]), ("Z2",["Z1","K2"]), ("Z3",["Z2","K3"])]          -- fails

ex8i = [("A",[]), ("B",[]), ("C",[]), ("D",[]), ("E",[]), 
        ("K1",["A","B","C"]), ("K2",["D","B","E"]), ("K3",["D","A"]), 
        ("Z1",["K1"]), ("Z2",["Z1","K3"]), ("Z3",["Z2","K2"])]          -- ok

ex8j = [("A",[]), ("B",[]), ("C",[]), ("D",[]), ("E",[]), 
        ("K1",["A","B","C"]), ("K2",["D","B","E"]), ("K3",["D","A"]), 
        ("Z1",["K1"]), ("Z2",["Z1","K2","K3"])]                         -- ok

ex8k = [("A",[]), ("B",[]), ("C",[]), ("D",[]), ("E",[]), 
        ("K1",["A","B","C"]), ("K2",["D","B","E"]), ("K3",["D","A"]), 
        ("Z1",["K3"]), ("Z2",["Z1","K2"]), ("Z3",["Z2","K1"])]          -- ok

ex8l = [("A",[]), ("B",[]), ("C",[]), ("D",[]), ("E",[]), 
        ("K1",["A","B","C"]), ("K2",["D","B","E"]), ("K3",["D","A"]), 
        ("Z1",["K2","K3"]), ("Z3",["Z1","K1"])]                         -- fails

ex8m = [("A",[]), ("B",[]), ("C",[]), ("D",[]), ("E",[]), 
        ("K1",["A","B","C"]), ("K2",["D","B","E"]), ("K3",["D","A"]), 
        ("Z1",["K3"]), ("Z3",["Z1","K1","K2"])]                         -- ok


ex9a = [("Eq",[]), ("Ord",["Eq"]),
        ("a",["Ord"])]                              -- ok

ex9b = [("Eq",[]), ("Ord",["Eq"]),
        ("a",["Eq"]), ("a1",["Ord","a"])]           -- ok

ex9c = [("Eq",[]), ("Ord",["Eq"]),
        ("a",["Ord"]), ("a1",["Eq","a"])]           -- fails


ex10a = [("Rational",[]), ("Integer",["Rational"]), ("tt",["Integer","Rational"])]      -- ok

ex10b = [("Rational",[]), ("Integer",["Rational"]), ("tt",["Rational","Integer"])]      -- fails


ex10c = [("Rational",[]), ("Integer",["Rational"]), ("tt",[]),
         ("tt_IntRat",["tt","Integer","Rational"])]                                     -- ok

ex10d = [("Rational",[]), ("Integer",["Rational"]), ("tt",[]),
         ("tt_RatInt",["tt","Rational","Integer"])]                                     -- fails


ex10e = [("Rational",[]), ("Integer",["Rational"]), ("tt",[]),
        ("tt_Rational",["tt","Rational"]), ("tt_Integer",["tt_Rational","Integer"])]    -- ok

ex10f = [("Rational",[]), ("Integer",["Rational"]), ("tt",[]),
         ("tt_Integer",["tt","Integer"]), ("tt_Rational",["tt_Integer","Rational"])]    -- ok!


ex10x = [("Rational",[]), ("Integer",["Rational"]), ("tt",[]),
         ("tt_Integer",["tt","Integer"]), ("tt_Rational",["tt","Rational"]),
         ("tt_tot",["tt_Integer","tt_Rational"])]                                       -- ok

ex10y = [("Rational",[]), ("Integer",["Rational"]), ("tt",[]),
         ("tt_Integer",["tt","Integer"]), ("tt_Rational",["tt","Rational"]),
         ("tt_tot",["tt_Rational","tt_Integer"])]                                       -- ok


ex10g = [("Rational",[]), ("Integer",["Rational"]), ("tt",["Rational"]),
         ("tt_Integer",["tt","Integer"])]                                               -- ok

ex10h = [("Rational",[]), ("Integer",["Rational"]), ("tt",["Integer"]),
         ("tt_Rational",["tt","Rational"])]                                             -- ok!


ex11  = [("Rational",[]), ("Integer",["Rational"]), ("Eq",[]), ("Ord",["Eq"]),
         ("tt",[]),
         ("tt_1",["tt","Integer","Rational"]), 
         ("tt_2",["tt","Ord"]), 
         ("tt_tot",["tt_2","tt_1"])]                                                    -- ok


ex12a = [("Eq",[]), ("Ord",["Eq"]), ("Apa",["Eq"]), ("Bepa",["Apa","Ord"])]             -- Bepa,Apa,Ord,Eq

ex12b = [("Eq",[]), ("Ord",["Eq"]), ("Apa",["Eq"]), ("Bepa",["Ord","Apa"])]             -- Bepa,Ord,Apa,Eq


ex13a = [("Eq",[]), ("Ord",["Eq"]), ("Hmm",["Eq"]), 
         ("Apa",["Ord"]), ("Bepa",["Apa","Hmm"])]                                       -- Bepa,Apa,Ord,Hmm,Eq

ex13b = [("Eq",[]), ("Ord",["Eq"]), ("Hmm",["Eq"]), 
         ("Apa",["Ord"]), ("Bepa",["Apa","Hmm","Ord"])]                                 -- Bepa,Apa,Hmm,Ord,Eq


ex14a = [("Apa",[]), ("Bepa",["Apa"]), ("Cepa",["Apa"]), ("Depa",["Cepa","Bepa"])]      -- Depa,Cepa,Bepa,Apa

ex14b = [("Apa",[]), ("Bepa",["Apa"]), ("Cepa",["Apa"]), ("Depa",["Cepa","Bepa"]),
         ("Xtra",["Bepa"]), ("Epa",["Depa","Xtra","Bepa"])]                             -- Epa,Depa,Cepa,Xtra,Bepa,Apa

ex14c = [("Apa",[]), ("Bepa",["Apa"]), ("Cepa",["Apa"]), ("Depa",["Cepa","Bepa"]),
         ("Xtra",["Apa"]), ("Epa",["Depa","Xtra","Cepa"])]                              -- Epa,Depa,Xtra,Cepa,Bepa,Apa

ex15  = [("G",[]), ("F",[]), ("E",[]),
         ("D",["G","F"]), ("C",["F"]), ("B",["E","F"]),
         ("A",["B","C","D"])]                                                           -- A,B,E,C,D,G,F
         
ex16  = [("Eq",[]), ("Ord",["Eq"]), ("Hmm",["Eq","Ord"])]                               -- fails


------------------------------------------------------------------------------------------------------------------------------------------------
-- Desired extension semantics:

ex17a = [("A",[]),                                                                      -- A
         ("B",["A"]),                                                                   -- B, A
         ("C",["A"]),                                                                   -- C, A
         ("D",["C","B"]),                                                               -- D, C, B, A
         ("str_D",["D"])]                                                               -- str_D, D, C, B, A

ex17b = [("A",[]),                                                                      -- A
         ("B",["A"]),                                                                   -- B, A
         ("C",["A"]),                                                                   -- C, A
         ("str_CB",["C","B"])]                                                          -- str_CB, C, B, A

ex17c = [("A",[]),                                                                      -- A
         ("B",["A"]),                                                                   -- B, A
         ("C",["A"]),                                                                   -- C, A
         ("str_A",["A"]),                                                               -- str_A, A
         ("str_B",["str_A","B"]),                                                       -- str_B, str_A, B, A
         ("str_C",["str_B","str_A","C"])]                                               -- str_C, str_B, str_A, B, C, A

ex17d = [("A",[]),                                                                      -- A
         ("B",["A"]),                                                                   -- B, A
         ("C",["A"]),                                                                   -- C, A
         ("str_B",["B"]),                                                               -- str_B, B, A                      <--- implicit A dep
         ("str_C",["str_B","C"])]                                                       -- str_C, str_B, B, C, A

ex17e = [("A",[]),                                                                      -- A
         ("B",["A"]),                                                                   -- B, A
         ("C",["B","A"]),                                                               -- C, B, A
         ("str_A",["A"]),                                                               -- str_A, A
         ("str_B",["str_A","B"]),                                                       -- str_B, str_A, B, A
         ("str_C",["str_B","str_A","C"])]                                               -- str_C, str_B, str_A, C, B, A

ex17f = [("A",[]),                                                                      -- A
         ("B",["A"]),                                                                   -- B, A
         ("C",["B","A"]),                                                               -- C, B, A
         ("str_B",["B"]),                                                               -- str_B, B, A
         ("str_C",["str_B","C"])]                                                       -- str_C, str_B, C, B, A            <--- implicit A dep

-- Conclusion: a single multi-extension left->right corresponds to *accumulating* extensions top->down.
-- That is, later extensions cover earlier ones, but protocol methods (the method defaults) only 
-- apply for methods implemented in neither of the extensions.
-----------------------------------------------------------------------------------------------------------------------------------------------


ex18  = [("A",[]), ("B",["A"]), ("C",["A"]), 
         ("str_A",["A"]),
         ("str_B",["B"]), 
         ("str_C",["C"]), 
         ("str_tot",["str_C","str_B","str_A"])]                                        -- str_tot,str_C,C,str_B,B,str_A,A  <--- bad, picks C before str_B...
         

ex20a = [("A",[]), ("B",["A"]), ("C",["A"]), ("D",["B","C"])]

ex20b = [("A",[]), ("B",["A"]), ("C",["A"]), ("D",["B","C"]), ("E",["D"])]

ex20c = [("A",[]), ("B",["A"]), ("C",["A"]), ("D",["B","C"]), ("E",[]), ("F",["D","E"])]


---------------

ex21a = [("Eq",[]), ("Ord",["Eq"]),
         ("Eq$set",["Eq"]), 
         ("Ord$set",["Ord","Eq$set"])]

ex21b = [("Eq",[]), ("Ord",["Eq"]),
         ("Eq$set",["Eq"]), 
         ("Ord$set",["Eq$set","Ord"])]

ex21c = [("Eq",[]), ("Ord",["Eq"]), ("Container",[]), ("Set",["Container","Ord"]),
         ("Eq$set",["Eq"]), 
         ("Ord$set",["Ord","Eq$set"]),
         ("Set$set",["Set","Ord$set"])]

ex21d = [("Eq",[]), ("Ord",["Eq"]), ("Container",[]), ("Set",["Container","Ord"]),
         ("Eq$set",["Eq"]), 
         ("Ord$set",["Eq$set","Ord"]),
         ("Set$set",["Ord$set","Set"])]

ex21e = [("Eq",[]), ("Ord",["Eq"]), ("Container",[]), ("Set",["Container","Ord"]),
         ("Ord$set",["Ord"]),
         ("Ord$set$2",["Ord$set","Ord"])]                                               -- Succeeds... Acceptable? Or detect by a separate check?

ex22  = [("Eq",[]), ("Ord",["Eq"]), ("Plus",[]), ("Minus",[]), ("Logical",[]),
         ("Complex",["Ord","Plus","Minus"]),
         ("Real",["Complex"]), ("Rational",["Real"]),
         ("Integral",["Rational","Logical"])]                                           -- Rational,Real,Complex,Ord,Eq,Plus,Minus,Logical


--   Indexed         Iterable
--    |               |
--    |               |
--   Sliceable       Collection      Plus
--    |               |               |
--    |               |               |
--   Sequence <-------- <--------------
--    |
--    |
--   Sequence$list

ex23  = [("Plus",[]), ("Indexed",[]), ("Iterable",[]),
         ("Sliceable",["Indexed"]), ("Collection",["Iterable"]),
         ("Sequence",["Sliceable","Collection","Plus"]),
         ("Sequence$list",["Sequence"])]                        -- Sequence,Sliceable,Indexed,Collection,Iterable,Plus



--   (Indexed)       Iterable                        Indexed
--    |               |                               |
--    |               |                               |
--   (Sliceable)     Collection      Plus            Sliceable
--    |               |               |               |
--    |               |               |               |
--   Sequence <-------- <--------------              Sliceable$list
--    |                                               |
--    |                                               |
--   Sequence$list <-----------------------------------

ex24  = [("Plus",[]), ("Indexed",[]), ("Iterable",[]),
         ("Sliceable",["Indexed"]), ("Collection",["Iterable"]),
         ("Sequence",["Sliceable","Collection","Plus"]),
         ("Sliceable$list",["Sliceable"]),
         ("Sequence$list",["Sliceable$list","Sequence"])]       -- Sliceable$list,Sequence,Sliceable,Indexed,Collection,Iterable,Plus



--   (Indexed)       Iterable
--    |               |
--    |               |
--   Sliceable       Collection      Plus            Indexed
--    |               |               |               |
--    |               |               |               |
--   Sequence <-------- <--------------              Indexed$list
--    |                                               |
--    |                                               |
--   Sequence$list <-----------------------------------

ex25  = [("Plus",[]), ("Indexed",[]), ("Iterable",[]),
         ("Sliceable",["Indexed"]), ("Collection",["Iterable"]),
         ("Sequence",["Sliceable","Collection","Plus"]),
         ("Indexed$list",["Indexed"]),
         ("Sequence$list",["Indexed$list","Sequence"])]         -- Indexed$list,Sequence,Sliceable,Indexed,Collection,Iterable,Plus



--                   Indexed         Iterable
--                    |               |
--                    |               |
--   (Iterable)      Sliceable       Collection      Plus        
--    |               |               |               |
--    |               |               |               |
--   (Collection)    Sequence <-------- <--------------          
--    |               |
--    |               |
--   Container       Sequence$list                               
--    |               |
--    |               |
--   Container$list <--

ex26  = [("Plus",[]), ("Indexed",[]), ("Iterable",[]),
         ("Sliceable",["Indexed"]), ("Collection",["Iterable"]),
         ("Sequence",["Sliceable","Collection","Plus"]),
         ("Container",["Collection"]),
         ("Sequence$list",["Sequence"]),
         ("Container$list",["Sequence$list","Container"])]      -- Sequence$list,Sequence,Sliceable,Indexed,Container,Collection,Iterable,Plus
