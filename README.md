# similar-names

This is a package allowing for reduction/grouping of a list based on edit
distance clustering of a field.

## Usage

```haskell
import qualified Data.SimilarNames as S

data Person = Person
    { pName :: String
    , pAge :: Int
    } deriving (Eq, Ord, Show)

instance S.IsNamed Person where
    toName = pName
    updateName p updatedName = p { pName = updatedName }

S.reduceSimilarlyNamed [Person "Jane Doe" 20, Person "Jane K Doe" 20, Person "John Doe" 20]
-- results in: [Person "Jane Doe" 20, Person "John Doe" 20]

S.groupSimilarlyNamed [Person "Jane Doe" 20, Person "Jane K Doe" 20, Person "John Doe" 20]
-- results in: Map.fromList [(Person "Jane Doe" 20, [Person "Jane Doe" 20, Person "Jane K Doe" 20]), (Person "John Doe" 20, [Person "John Doe" 20])]
```

## License

Copyright 2017 Josh Clayton. See the [LICENSE](LICENSE).
