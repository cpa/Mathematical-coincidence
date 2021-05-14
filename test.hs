import Data.List
import Formula
import SearchCoincidence
import System.Environment (getArgs)

main :: IO ()
main = 
    do 
      fileName:_ <- getArgs
      writeHTMLDocument endList fileName
      -- writeStdout endList
      -- putStrLn $ show $ length endList
    where base = [Pi,Exp1,Phi,one, Sqrt (Integer 2)]
          endList = nubBy (\(_,v) (_,v') -> v==v') $ filter (fst . isCoincidence . fst) $ map (\(x,y) -> (simplify x, y)) (coincidences base base 4)
