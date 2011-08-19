module Formula
    (
     Formula (..)
    ,zero
    ,one
    ,eval
    ,simplify
    ,toLatex
    ,writeLatexDocument
) where

import Data.Char (intToDigit,digitToInt)
import System.IO
import Data.List (partition)

data Formula = Pi
             | Exp1
			 | Phi
             | Integer Int
             | Log Formula
             | Exp Formula Formula
             | Plus Formula Formula
             | Minus Formula Formula
             | Times Formula Formula
             | Quotient Formula Formula
             | Sqrt Formula
             | Cos Formula
             | Sin Formula
               deriving (Show,Eq,Ord)

zero = Integer 0
one = Integer 1
                         

eval :: Formula -> Double
eval fs = 
    case fs of
      Pi -> pi
      Exp1 -> 2.718281828
      Phi -> eval $ Quotient (Plus one (Sqrt (Integer 5))) (Integer 2)
      Log f -> (log $ eval f)
      Exp a b -> (eval a) ** (eval b)
      Plus a b -> (eval a) + (eval b)
      Minus a b -> (eval a) - (eval b)
      Times a b -> (eval a) * (eval b)
      Quotient a b -> (eval a) / (eval b)
      Cos f -> cos (eval f)
      Sin f -> sin (eval f)
      Sqrt f -> sqrt $ eval f
      Integer n -> fromIntegral n

simplify :: Formula -> Formula
simplify fs = 
    case fs of
      Log (Exp1) -> one
      Exp one _ -> one
      Exp a one -> simplify a
      Log (Exp Exp1 f) -> simplify f
      Exp Exp1 (Log f) -> simplify f
      Times zero f -> zero
      Times f zero -> zero
      Plus zero f -> simplify f
      Plus f zero -> simplify f
      Minus f zero -> simplify f
      Quotient zero f -> simplify zero
      Quotient a b -> if a==b then simplify one else fs
      Log one -> zero
      Log (Times f Exp1) -> Plus one $ Log $ simplify f
      Log (Times Exp1 f) -> Plus one $ Log $ simplify f
      Exp _ zero -> one
      Log f -> Log $ simplify f
      Exp a b -> Exp (simplify a) (simplify b)
      Minus a b -> Minus (simplify a) (simplify b)
      Plus a b -> Plus (simplify a) (simplify b)
      Times a b -> Times (simplify a) (simplify b)
      Quotient a b -> Quotient (simplify a) (simplify b)
      Sqrt f -> Sqrt $ simplify f
      Cos Pi -> Minus zero one
      Cos (Minus zero Pi) -> one
      Cos f -> Cos $ simplify f
      Sin Pi -> zero
      Sin (Minus zero Pi) -> zero
      Sin f -> Sin $ simplify f
      _ -> fs

toLatex :: Formula -> String
toLatex fs = 
    case fs of
      Pi -> "\\pi"
      Exp1 -> "e"
      Phi -> "\\phi"
      Log f -> "\\log{"++(toLatex f)++"}"
      Exp a b -> "{"++(toLatex a)++"}^{"++(toLatex b)++"}"
      Plus a b -> "\\left("++(toLatex a)++" + "++(toLatex b)++"\\right)"
      Minus a b -> "\\left("++(toLatex a)++" - "++(toLatex b)++"\\right)"
      Times a b -> "\\left("++(toLatex a)++" "++(toLatex b)++"\\right)"
      Quotient a b -> "\\frac{"++(toLatex a)++"}{"++(toLatex b)++"}"
      Sqrt f -> "\\sqrt{"++(toLatex f)++"}"
      Cos f -> "\\cos{"++(toLatex f)++"}"
      Sin f -> "\\sin{"++(toLatex f)++"}"
      Integer n -> [intToDigit n]

writeLatexDocument :: [(Formula,Double)] -> String -> IO ()
writeLatexDocument fs name = 
    do 
      outh <- openFile name WriteMode
      hPutStrLn outh $ header ++ contentsOthers ++ onesSeparator ++ contentsOnes ++ footer
      hClose outh
          where header = "\\documentclass[a4]{article}\n\\usepackage[T1]{fontenc}\n\\usepackage[utf8]{inputenc}\n\\begin{document}\n"
                onesSeparator = "\\newpage And now, for the ones!\n"
                footer = "\\end{document}\n"
                (ones,others) = partition (\(_,x) -> (abs x) > 0.9 && (abs x) < 1.1) $ fs
                contentsOnes = concat $ map (\(f,v) -> "$$"++(toLatex f)++" = "++(show v)++"$$\n")  ones
                contentsOthers = concat $ map (\(f,v) -> "$$"++(toLatex f)++" = "++(show v)++"$$\n")  others