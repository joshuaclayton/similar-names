module SimilarNamesSpec
    ( main
    , spec
    ) where

import qualified Data.Map as Map
import qualified Data.SimilarNames as S
import           Test.Hspec

data Person = Person
    { pName :: String
    , pAge :: Int
    } deriving (Eq, Show)

instance S.IsNamed Person where
    toName = pName
    updateName p updatedName = p { pName = updatedName }

main :: IO ()
main = hspec spec

spec :: Spec
spec = context "SimilarNames" $ do
    describe "reduceSimilarlyNamed" $ do
        it "reduces similar words" $
            S.reduceSimilarlyNamed ["hello world", "hello worlds"] `shouldBe` ["hello world"]

        it "doesn't reduce dissimilar words" $
            S.reduceSimilarlyNamed ["hello world", "totally different"] `shouldBe` ["hello world", "totally different"]

        it "supports custom data types" $
            S.reduceSimilarlyNamed [Person "Jane Doe" 20, Person "Jane K Doe" 20, Person "John Doe" 20] `shouldBe` [Person "Jane Doe" 20, Person "John Doe" 20]

    describe "groupSimilarlyNamed" $ do
        it "groups similar words" $ do
            let list = ["hello world", "hello worlds"]
            S.groupSimilarlyNamed list `shouldBe` Map.fromList [(head list, list)]

        it "supports custom data types" $
            S.groupSimilarlyNamed [Person "Jane Doe" 20, Person "Jane K Doe" 20, Person "John Doe" 20] `shouldBe` Map.fromList [("Jane Doe", [Person "Jane Doe" 20, Person "Jane K Doe" 20]), ("John Doe", [Person "John Doe" 20])]
