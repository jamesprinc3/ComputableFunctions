import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Coder
import Defs

geni :: Gen Pair
geni = do
  x <- arbitrary
  return $ I (abs x)

gensb :: Gen Pair
gensb = do
  x <- arbitrary
  y <- arbitrary
  return $ SB (I (abs x)) (I (abs y))

gendb :: Gen Pair
gendb = do
  x <- arbitrary
  y <- arbitrary
  return $ DB (I (abs x)) (I (abs y))


-- instance Arbitrary Pair where
--    arbitrary = do
--     ret <- oneof [geni, gensb, gendb]
--     return $ ret

-- instance Arbitrary PairInt where
--   arbitrary = do
--     x <- arbitrary
--     return $ (I x)


main :: IO ()
main = hspec $ do
  describe "pairToInt" $ do
    it "<0,0> == 0" $ do 
      pairToInt (SB (I 0) (I 0)) `shouldBe` 0

    -- it "Converts a Pair = Int | <x,y> | <<x,y>> to an integer" $ property $
    --   \(p :: Pair) -> (pairToInt p) == (x :: Integer) where (I x) = p

  -- describe "pairToInt" $ do
  --   it "Converts a Pair = Int | <x,y> | <<x,y>> to an integer" $ property $
  --     \
  -- describe "toSinglePair" $ do
  --   it "Converts from integer z = (2^x)*(2y + 1)−1 -> <x,y>" $ property $
  --     \x -> toSinglePair x == (x :: Int)
