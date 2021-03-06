module SearchCoincidence
    (
     coincidences
    ,isCoincidence
    ) where

import Formula
import Data.List

testFormula = Minus (Exp Exp1 Pi) Pi

isCoincidence :: Formula -> (Bool,Maybe Double)
isCoincidence a =
    if ((delta < toleranceH) && (delta > toleranceL) ||
       (1-delta < toleranceH) && (1-delta > toleranceL)) &&
       rounded /= value &&
       value > 0.5 &&
       not (isNaN value)
    then (True, Just value)
    else (False, Nothing)
    where toleranceH = 0.0001
          toleranceL = 0.000000000000000001
          value = abs $ eval a
          rounded = abs $ fromIntegral $ floor value
          delta = abs $ value - rounded

oneStepFartherFormulas :: Formula -> [Formula] -> [Formula]
oneStepFartherFormulas f base = do
  b <- base
  -- [Log f,Sqrt f,Exp b f,Exp f b,Plus b f,Minus b f,Times b f,Quotient b f,Quotient f b, Cos f, Sin f]
  [Log f,Sqrt f,Exp b f,Exp f b,Plus b f,Minus b f,Times b f,Quotient b f,Quotient f b]

coincidences :: [Formula] -> [Formula] -> Int -> [(Formula,Double)]
coincidences fs base distance
    | distance == 0 = map (\x -> (x, eval x)) fs
    | otherwise =
        coincidences newFs base (distance-1)
            where newFs =  (fs >>= \f -> oneStepFartherFormulas f base)


