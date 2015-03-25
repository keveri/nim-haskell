module NimGame.InternalSpec (main, spec) where

import NimGame.Internal
import Test.Hspec
import Test.QuickCheck
import Control.Applicative
import qualified Data.Tuple as T (swap)
import qualified Data.Char  as C (isDigit)

genTable :: Gen [Int]
genTable = listOf $ elements [0..99]

instance Arbitrary Game where
  arbitrary = Game <$> genTable <*> arbitrary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "defaultTable" $ do
    it "equals [5,4,3,2,1]" $ do
      defaultTable `shouldBe` [5,4..1]

  describe "checkTable" $ do
    it "returns default table with invalid table." $ do
      checkTable [] `shouldBe` defaultTable

    it "returns default table with empty table." $ do
      checkTable [0,0,0] `shouldBe` defaultTable

    it "returns same table if table is valid" $ do
      checkTable [1,2] `shouldBe` [1,2]

  describe "printableTable" $ do
    it "changes numbers to strings containing stars" $ do
      let expected = ["1: **", "2: *"]
      expected `shouldBe` (printableTable $ Game [2,1] ("",""))

    it "row count stays the same" $ property $
      \g@(Game t _) -> length t == length (printableTable g)

    it "rows have correct amount of stars" $ property $
      \g@(Game t _) -> map (length . filter (=='*')) (printableTable g) == t

    it "rows have correct numbers" $ property $
      \g@(Game t _) ->
      let numbers = map (\x -> read (filter C.isDigit x) :: Int) (printableTable g)
      in numbers == [1..(length t)]

  describe "changeTurn" $ do
    it "swaps players in tuple" $ property $
      \g@(Game t ps) -> changeTurn g == Game t (T.swap ps)
      
  describe "inTurn" $ do
    it "returns the first player from tuple" $ property $
      \g@(Game _ (x,y)) -> inTurn g == x

  describe "validMove" $ do
    let game = Game [2,1,0] ("a","b")
    it "returns true when move is valid" $ do
      validMove game (1,1) `shouldBe` True

    it "returns false when row doesn't exist" $ do
      validMove game (4,1) `shouldBe` False

    it "returns false when row is empty" $ do
      validMove game (3,1) `shouldBe` False

    it "returns false when star count is too high" $ do
      validMove game (2,2) `shouldBe` False

    it "returns false when star count is negative" $ do
      validMove game (2,-1) `shouldBe` False

    it "returns false when row is negative" $ do
      validMove game (-2,1) `shouldBe` False

  describe "applyMove" $ do
    let game = Game [2,1] ("","")
    it "applys the valid move to game" $ do
      applyMove game (1,1) `shouldBe` Right (Game [1,1] ("",""))

    it "returns error string with invalid move" $ do
      applyMove game (9,1) `shouldBe` Left "Invalid move."
     
  describe "gameOver" $ do
    it "returns true when all zeros" $ do
      let game = Game [0,0,0,0,0] ("","")
      gameOver game `shouldBe` True

    it "returns false when all not zeros" $ do
      let game = Game [0,0,0,0,1] ("","")
      gameOver game `shouldBe` False

