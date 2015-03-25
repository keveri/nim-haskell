import NimGame as Nim

printWinner :: String -> IO ()
printWinner = putStrLn . (++) "Winner is: "

main :: IO ()
main = Nim.play [] >>= printWinner

