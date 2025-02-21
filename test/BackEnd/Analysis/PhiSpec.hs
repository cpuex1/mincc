module BackEnd.Analysis.PhiSpec (spec) where

import BackEnd.Analysis.Phi
import Data.Set (fromList)
import Test.Hspec

spec :: Spec
spec = do
    describe "addGroup" $ do
        it "merging1" $ do
            let set1 = fromList [1, 2, 3]
            let set2 = fromList [1]
            let set3 = fromList [3]
            let answer = [fromList [1, 2, 3]]
            addGroup set1 (addGroup set2 (addGroup set3 (PhiGroups [])))
                `shouldBe` PhiGroups answer
        it "merging2" $ do
            let set1 = fromList [1, 2]
            let set2 = fromList [3, 4]
            let set3 = fromList [4, 5]
            let answer = [fromList [3, 4, 5], fromList [1, 2]]
            addGroup set1 (addGroup set2 (addGroup set3 (PhiGroups [])))
                `shouldBe` PhiGroups answer
