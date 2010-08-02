import Formula
import SearchCoincidence
import System(getArgs)

main :: IO ()
main = 
    do 
      fileName:_ <- getArgs
      writeLatexDocument endList fileName
      putStrLn $ show $ length endList
    where base = [Pi,Exp1,Phi]
          endList = coincidences base base 3
