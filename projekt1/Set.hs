module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems
              ) where
import Prelude hiding(null)

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

empty :: Set a
empty = Empty 

null :: Set a -> Bool
null Empty = True
null _  = False

member :: Eq a => a -> Set a -> Bool
member _ Empty = False
member x (Singleton y) = x == y
member x (Union s1 s2) = member x s1 || member x s2

singleton :: a -> Set a
singleton = Singleton

fromList :: [a] -> Set a
fromList [] = Empty
fromList [x] = Singleton x
fromList (x:xs) = Union (Singleton x) (fromList xs)

toList :: Set a -> [a]
toList Empty = []
toList (Singleton x) = [x]
toList (Union s1 s2) = toList s1 ++ toList s2

toAscList :: Ord a => Set a -> [a]
toAscList = undefined 

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union = undefined 

insert :: a -> Set a -> Set a
insert = undefined 

-- instance Ord a => Eq (Set a) where

-- instance Semigroup (Set a) where

-- instance Monoid (Set a) where

instance Show a => Show (Set a) where
    show Empty = "null"
    show (Singleton x) = show x
    show (Union s1 s2) = show (s1, s2)

-- instance Functor Set where
