module Formula
    (
     Formula (..)
    ,zero
    ,one
    ,eval
    ,simplify
    ,toLatex
    ,writeLatexDocument
    ,writeHTMLDocument
    ,writeStdout
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
      Log Exp1 -> (Integer 1)
      Exp (Integer 1) _ -> (Integer 1)
      Exp a (Integer 1) -> simplify a
      Log (Exp Exp1 f) -> simplify f
      Exp Exp1 (Log f) -> simplify f
      Times (Integer 0) f -> (Integer 0)
      Times f (Integer 0) -> (Integer 0)
      Plus (Integer 0) f -> simplify f
      Plus f (Integer 0) -> simplify f
      Minus f (Integer 0) -> simplify f
      Quotient (Integer 0) f -> simplify (Integer 0)
      Quotient a b -> if a==b then (Integer 1) else fs
      Log (Integer 1) -> (Integer 0)
      Log (Times f Exp1) -> Plus (Integer 1) $ Log $ simplify f
      Log (Times Exp1 f) -> Plus (Integer 1) $ Log $ simplify f
      Exp _ (Integer 0) -> (Integer 1)
      Log f -> Log $ simplify f
      Exp a b -> Exp (simplify a) (simplify b)
      Minus a b -> Minus (simplify a) (simplify b)
      Plus a b -> Plus (simplify a) (simplify b)
      Times a b -> Times (simplify a) (simplify b)
      Quotient a b -> Quotient (simplify a) (simplify b)
      Sqrt (Integer 1) -> Integer 1
      Sqrt (Integer 0) -> Integer 0
      Sqrt f -> Sqrt $ simplify f
      Cos Pi -> Minus (Integer 0) (Integer 1)
      Cos (Minus (Integer 0) Pi) -> (Integer 1)
      Cos f -> Cos $ simplify f
      Sin Pi -> (Integer 0)
      Sin (Minus (Integer 0) Pi) -> (Integer 0)
      Sin f -> Sin $ simplify f
      fs -> fs

toLatex :: Formula -> String
toLatex fs = 
    case fs of
      Pi -> "\\pi"
      Exp1 -> "e"
      Phi -> "\\phi"
      Log f -> "\\left(\\ln{"++(toLatex f)++"}\\right)"
      Exp a b -> "\\left({"++(toLatex a)++"}\\right)^{"++(toLatex b)++"}"
      Plus a b -> "\\left("++(toLatex a)++" + "++(toLatex b)++"\\right)"
      Minus a b -> "\\left("++(toLatex a)++" - "++(toLatex b)++"\\right)"
      Times a b -> "\\left("++(toLatex a)++" "++(toLatex b)++"\\right)"
      Quotient a b -> "\\frac{"++(toLatex a)++"}{"++(toLatex b)++"}"
      Sqrt f -> "\\sqrt{"++(toLatex f)++"}"
      Cos f -> "\\left(\\cos{"++(toLatex f)++"}\\right)"
      Sin f -> "\\left(\\sin{"++(toLatex f)++"}\\right)"
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

writeHTMLDocument :: [(Formula,Double)] -> String -> IO ()
writeHTMLDocument fs name =
  do
    outh <- openFile name WriteMode
    hPutStrLn outh $ header ++ contentsOthers ++ onesSeparator ++ contentsOnes ++ footer
    hClose outh
      where header = "<html>\n<script src=\"https://polyfill.io/v3/polyfill.min.js?features=es6\"></script><script id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js\"></script><h2>Coincidences?</h2>"
            onesSeparator = "<h2>Ones</h2>"
            footer = ""
            (ones,others) = partition (\(_,x) -> (abs x) > 0.9 && (abs x) < 1.1) $ fs
            contentsOnes = concat $ map (\(f,v) -> "$$"++(toLatex f)++" = "++(show v)++"$$\n")  ones
            contentsOthers = concat $ map (\(f,v) -> "$$"++(toLatex f)++" = "++(show v)++"$$\n")  others

writeStdout :: [(Formula,Double)] -> IO ()
writeStdout fs = do
  mapM_ print fs
