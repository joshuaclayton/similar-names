{-# LANGUAGE FlexibleInstances #-}

module Data.SimilarNames
    ( IsNamed(..)
    , Settings(..)

    , reduceSimilarlyNamed
    , groupSimilarlyNamed
    ) where

import           Control.Monad.Reader (Reader, runReader, asks, ask)
import qualified Data.Clustering.Hierarchical as C
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Text.EditDistance as ED

class (Ord a, Eq a) => IsNamed a where
    toName :: a -> String
    updateName :: a -> String -> a

instance IsNamed String where
    toName = id
    updateName _ = id

data Settings = Settings
    { sDefaultDendrogramDistance :: Double
    , sMinimumLengthForStringComparison :: Int
    , sDefaultEditDistanceWhenUnderLengthMinimum :: Double
    , sEditCosts :: ED.EditCosts
    }

type WithSettings a = Reader Settings a

reduceSimilarlyNamed :: IsNamed a => [a] -> [a]
reduceSimilarlyNamed xs = runReader (reduceSimilarlyNamed' xs) defaultSettings

groupSimilarlyNamed :: IsNamed a => [a] -> Map.Map a [a]
groupSimilarlyNamed xs = runReader (groupSimilarlyNamed' xs) defaultSettings

reduceSimilarlyNamed' :: IsNamed a => [a] -> WithSettings [a]
reduceSimilarlyNamed' xs =
    (uniqueElementsFromMap . updateNamesOfValuesFromKey) <$> groupSimilarlyNamed' xs
  where
    uniqueElementsFromMap = L.nub . concat . Map.elems
    updateNamesOfValuesFromKey = Map.mapWithKey (\k -> map (($ toName k) . updateName))

groupSimilarlyNamed' :: IsNamed a => [a] -> WithSettings (Map.Map a [a])
groupSimilarlyNamed' xs = do
    settings <- ask
    (dendrosToMap . C.cutAt (dendro xs (distanceFormula settings))) <$> distance
  where
    distance = asks sDefaultDendrogramDistance
    dendrosToMap = foldl (flip go) Map.empty
    go v = Map.insert (dendroToKey v) (C.elements v)
    dendro = C.dendrogram C.SingleLinkage

dendroToKey :: IsNamed a => C.Dendrogram a -> a
dendroToKey = head . C.elements

distanceFormula :: IsNamed a => Settings -> a -> a -> Double
distanceFormula settings a b =
    if length namedA >= minimumLength && length namedB >= minimumLength
        then fromIntegral $ ED.levenshteinDistance editCosts namedA namedB
        else defaultEditDistance
  where
    namedA = toName a
    namedB = toName b
    minimumLength = sMinimumLengthForStringComparison settings
    editCosts = sEditCosts settings
    defaultEditDistance = sDefaultEditDistanceWhenUnderLengthMinimum settings

defaultSettings :: Settings
defaultSettings =
    Settings
        { sDefaultDendrogramDistance = 2
        , sMinimumLengthForStringComparison = 3
        , sDefaultEditDistanceWhenUnderLengthMinimum = 20
        , sEditCosts = defaultEditCosts
        }

defaultEditCosts :: ED.EditCosts
defaultEditCosts =
    ED.EditCosts {
        ED.deletionCosts = ED.ConstantCost 5,
        ED.insertionCosts = ED.ConstantCost 1,
        ED.substitutionCosts = ED.ConstantCost 10,
        ED.transpositionCosts = ED.ConstantCost 20
    }
