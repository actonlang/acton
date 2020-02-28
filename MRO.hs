module MRO where
    
type Graph = [(String,[String])]

mro                             :: Graph -> IO ()
mro graph                       = do mapM (putStrLn . uncurry showclass) graph
                                     putStrLn "----"
                                     mro' [] graph

mro'                            :: Graph -> Graph -> IO ()
mro' lins []                    = return ()
mro' lins ((c,bases):graph)     = case merge [] (map lin bases ++ [bases]) of
                                    Right cs -> do
                                        putStrLn (showlin c cs)
                                        mro' ((c,cs):lins) graph
                                    Left err -> putStrLn err
  where lin a                   = case lookup a lins of
                                    Just la -> a:la
                                    Nothing -> error ("Forward ref from " ++ c ++ " to " ++ a)

        merge out lists
          | null heads          = Right $ reverse out
          | h:_ <- good         = merge (h:out) [ if hd==h then tl else hd:tl | (hd,tl) <- zip heads tails ]
          | otherwise           = Left (">>>>>> " ++ showlin c (reverse out) ++ 
                                      " ++ merge(" ++ commasep (map showlist lists) ++ ") <<<<<<<")
          where (heads,tails)   = unzip [ (hd,tl) | hd:tl <- lists ]
                good            = [ h | h <- heads, all (h `notElem`) tails ]

type Name                       = String
type Env                        = [(Name,[Name])]

linearize                       :: Env -> (Name,[Name]) -> (Name,[Name])
linearize env (c,bases)         = (c, merge [] (map findlin bases ++ [bases]))
  where findlin a               = a : case lookup a env of Just la -> la
        merge out lists
          | null heads          = reverse out
          | h:_ <- good         = merge (h:out) [ if hd==h then tl else hd:tl | (hd,tl) <- zip heads tails ]
          | otherwise           = error (">>>>>> " ++ showlin c (reverse out) ++ 
                                      " ++ merge(" ++ commasep (map showlist lists) ++ ") <<<<<<<")
          where (heads,tails)   = unzip [ (hd,tl) | hd:tl <- lists ]
                good            = [ h | h <- heads, all (h `notElem`) (tails::[[Name]]) ]

mro2                            :: Env -> Graph -> IO ()
mro2 env []                     = mapM (putStrLn . uncurry showlin) (reverse env) >> return ()
mro2 env (g:graph)              = mro2 (lmap:env) graph
  where lmap                    = linearize env g

commasep []                     = ""
commasep [x]                    = x
commasep (x:xs)                 = x ++ "," ++ commasep xs

showlin c cs                    = "L(" ++ c ++ ") = " ++ showlist (c:cs)
showclass c bases               = c ++ "(" ++ commasep bases ++ ")"
showlist cs                     = "[" ++ commasep cs ++ "]"

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

ex17a = [("A",[]), ("B",["A"]), ("C",["A"]), ("D",["C","B"]),
         ("str_D",["D"])]                                                               -- str_D,D,C,B,A

ex17b = [("A",[]), ("B",["A"]), ("C",["A"]), 
         ("str_CB",["C","B"])]                                                          -- str_CB,C,B,A

ex17c = [("A",[]), ("B",["A"]), ("C",["A"]), 
         ("str_C",["C"]), ("str_B",["str_C","B"])]                                      -- str_B,str_C,C,B,A                <--- desired successive extension semantics!

ex18  = [("A",[]), ("B",["A"]), ("C",["A"]), 
         ("str_C",["C"]), ("str_B",["B"]), ("str_tot",["str_C","str_B"])]               -- str_tot,str_C,C,str_B,B,A        <--- bad, picks C before str_B
         
-- Conclusion: accumulating extensions top->down correspond to one multi-extension left->right
-- That is, later extensions cover earlier ones, but protocol methods (the method defaults) only 
-- apply for methods implemented in neither of the extensions.