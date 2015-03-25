module NimGame.Internal where
-- Should contain only logic, IO is in NimGame.

type Table   = [Int]
type Move    = (Int,Int)
type Player  = String
type Players = (Player,Player)

data Game = Game Table Players deriving (Eq, Show)

defaultTable :: Table
defaultTable = [5,4..1]

-- Validate table by checking that it contains at least some stars.
checkTable :: Table -> Table
checkTable t | sum t < 1 = defaultTable
             | otherwise = t

-- Table in printable format.
printableTable :: Game -> [String]
printableTable (Game t _) =
  zipWith (curry toStr) [1..length t] $ map (concat . flip replicate "*") t
    where toStr (n,s) = show n ++ ": " ++ s

-- Change player positions. First one in tuple is in turn.
changeTurn :: Game -> Game
changeTurn (Game t (x,y)) = Game t (y,x)

-- Return player in turn.
inTurn :: Game -> Player
inTurn (Game _ (p,_)) = p

-- Check that move is valid for the game.
validMove :: Game -> Move -> Bool
validMove (Game t _) (n,c) =
  (n <= length t) && n > 0 && c > 0 && row > 0 && c <= row
    where row = t !! (n - 1)

-- Apply move to the game. Return new game or error string.
applyMove :: Game -> Move -> Either String Game
applyMove g@(Game t ps) m@(n,c)
    | validMove g m = Right $ Game updated ps
    | otherwise     = Left "Invalid move."
    where (x,y:ys) = splitAt (n - 1) t
          updated  = x ++ [y - c] ++ ys

-- Check if game is over. Game is over when no stars are left on the table.
gameOver :: Game -> Bool
gameOver (Game t _) = all (==0) t

