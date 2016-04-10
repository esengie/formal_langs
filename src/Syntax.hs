module Syntax where

import Text.PrettyPrint.HughesPJClass

data Expr = Num Integer
          | Var String  
          | Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Divide Expr Expr 
          | Mod Expr Expr
          | Eqq Expr Expr
          | Ne Expr Expr
          | GR Expr Expr
          | GE Expr Expr
          | LS Expr Expr
          | LE Expr Expr
          | And Expr Expr
          | Or Expr Expr
    deriving (Show)

instance Pretty Expr where
  pPrint (Num a) = pPrint a
  pPrint (Var s) = text "Var: " <> pPrint s
  pPrint (Plus l r) = vcat [ text "+"
                           , nest 2 (pPrint l)
                           , nest 2 (pPrint r)]
  pPrint (Minus l r) = vcat [ text "-"
                           , nest 2 (pPrint l)
                           , nest 2 (pPrint r)]
  pPrint (Times l r) = vcat [ text "*"
                           , nest 2 (pPrint l)
                           , nest 2 (pPrint r)]
  pPrint (Divide l r) = vcat [ text "/"
                           , nest 2 (pPrint l)
                           , nest 2 (pPrint r)]                                                     
  pPrint (Mod l r) = vcat [ text "%"
                           , nest 2 (pPrint l)
                           , nest 2 (pPrint r)]
  pPrint (Eqq l r) = vcat [ text "=="
                           , nest 2 (pPrint l)
                           , nest 2 (pPrint r)]
  pPrint (Ne l r) = vcat [ text "!="
                           , nest 2 (pPrint l)
                           , nest 2 (pPrint r)]
  pPrint (GR l r) = vcat [ text ">"
                           , nest 2 (pPrint l)
                           , nest 2 (pPrint r)]                                                                               
  pPrint (GE l r) = vcat [ text ">="
                           , nest 2 (pPrint l)
                           , nest 2 (pPrint r)]
  pPrint (LS l r) = vcat [ text "<"
                           , nest 2 (pPrint l)
                           , nest 2 (pPrint r)]
  pPrint (LE l r) = vcat [ text "<="
                           , nest 2 (pPrint l)
                           , nest 2 (pPrint r)]
  pPrint (And l r) = vcat [ text "&&"
                           , nest 2 (pPrint l)
                           , nest 2 (pPrint r)]                                                                                                          
  pPrint (Or l r) = vcat [ text "||"
                           , nest 2 (pPrint l)
                           , nest 2 (pPrint r)] 

data Stat = Skip | Assign String Expr | Semi Stat Stat 
        | Write Expr | Read Expr 
        | While Expr Stat | If Expr Stat Stat
    deriving (Show)

instance Pretty Stat where
  pPrint Skip = text "skip"
  pPrint (Assign l r) = vcat [ text ":="
                           , nest 2 (pPrint l)
                           , nest 2 (pPrint r)]
  pPrint (Semi l r) = vcat [ text ";"
                           , nest 2 (pPrint l)
                           , nest 2 (pPrint r)]
  pPrint (Read s) = text "Read " <> pPrint s
  pPrint (Write s) = text "Write " <> pPrint s
  pPrint (While l r) = vcat [ text "While"
                           , nest 2 (pPrint l)
                           , nest 2 (pPrint r)]                                                                                                          
  pPrint (If l r s) = vcat [ text "If"
                           , nest 2 (pPrint l)
                           , nest 2 (pPrint r)
                           , nest 2 (pPrint s)]
  

