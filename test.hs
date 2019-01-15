grid =
  [ "__C________R___"
  , "__SI________U__"
  , "__HASKELL____B_"
  , "__A__A_____S__Y"
  , "__R___B___C____"
  , "__PHP____H_____"
  , "____S_LREP_____"
  , "____I__M_Y__L__"
  , "____L_E__T_A___"
  , "_________HB____"
  , "_________O_____"
  , "________CN_____"
  ]

languages =
  [ "BASIC"
  , "COBAL"
  , "CSHARP"
  , "HASKELL"
  , "LISP"
  , "PERL"
  , "PHP"
  , "PYTHON"
  , "RUBY"
  , "SCHEME"
  ]

data Cell =
  Cell (Integer, Integer)
       Char
  deriving (Eq, Ord, Show)

og :: Show a => [a] -> IO ()
og = putStrLn . unlines . map show

coordinates = do
  row <- [0 ..]
  return
    (do coulmn <- [0 ..]
        return (row, coulmn))

zipOverGrid = zipWith zip

zipOverGridWith = zipWith . zipWith
