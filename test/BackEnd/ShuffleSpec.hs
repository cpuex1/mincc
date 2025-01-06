module BackEnd.ShuffleSpec (spec) where

import BackEnd.Shuffle
import Test.Hspec

spec :: Spec
spec = do
    describe "runDetectLoop" $ do
        it "noLoop" $ do
            (runDetectLoop 0 [(0, 1), (1, 2), (0, 2)] :: Maybe [Int]) `shouldBe` Nothing
        it "loop1" $ do
            (runDetectLoop 0 [(0, 1), (1, 0), (1, 2)] :: Maybe [Int]) `shouldBe` Just [0, 1]
        it "loop2" $ do
            (runDetectLoop 0 [(1, 2), (2, 4), (0, 1), (3, 0), (2, 3)] :: Maybe [Int]) `shouldBe` Just [0, 3, 2, 1]
    describe "detectAllLoops" $ do
        it "noLoop" $ do
            (detectAllLoops [0, 1, 2] [(0, 1), (1, 2), (0, 2)] :: [[Int]]) `shouldBe` []
        it "loop1" $ do
            (detectAllLoops [0, 1, 2] [(0, 1), (1, 0), (2, 0)] :: [[Int]]) `shouldBe` [[0, 1]]
        it "loop2" $ do
            (detectAllLoops [0, 1, 2, 3, 4] [(0, 1), (1, 0), (2, 3), (3, 4), (4, 2)] :: [[Int]]) `shouldBe` [[0, 1], [2, 4, 3]]
    describe "genCirc" $ do
        it "loop1" $ do
            genCirc 0 [1, 2] `shouldBe` ([(0, 2), (2, 1), (1, 0)] :: [(Int, Int)])
        it "loop2" $ do
            genCirc 0 [1, 2, 3] `shouldBe` ([(0, 3), (3, 2), (2, 1), (1, 0)] :: [(Int, Int)])
    describe "shuffle" $ do
        it "noLoop" $ do
            shuffle 0 [(1, 2), (3, 4)] `shouldBe` ([(1, 2), (3, 4)] :: [(Int, Int)])
        it "loop1" $ do
            shuffle 0 [(1, 2), (2, 1)] `shouldBe` ([(0, 2), (2, 1), (1, 0)] :: [(Int, Int)])
        it "loop2" $ do
            shuffle 0 [(1, 3), (2, 2), (3, 1), (4, 1)] `shouldBe` ([(4, 1), (0, 3), (3, 1), (1, 0)] :: [(Int, Int)])
