module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems
              ) where
import Prelude hiding(null)
import qualified Data.List as L

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

-- toAscList :: Ord a => Set a -> [a]
-- toAscList = removeDuplicates . L.sort . toList
--     where 
--         removeDuplicates l = foldr (\x acc -> if x `elem` acc then acc else x : acc) [] l

toAscList :: Ord a => Set a -> [a]
toAscList = removeDuplicates . L.sort . toList
    where 
        removeDuplicates l = foldr (\x acc -> if not (L.null acc) && x == head acc then acc else x : acc) [] l

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union s Empty = s
union Empty s = s
union s1 s2 = Union s1 s2
-- union = Union

insert :: a -> Set a -> Set a
insert x Empty = Singleton x
insert x s = Union (Singleton x) s

instance Ord a => Eq (Set a) where
    Empty == Empty = True
    Singleton x == Singleton y = x == y
    s1 == s2 = toAscList s1 == toAscList s2

instance Semigroup (Set a) where
    -- s1 <> s2 = s1 `union` s2
    (<>) = union
    
instance Monoid (Set a) where
    mempty = Empty

instance Show a => Show (Set a) where
    show Empty = "null"
    show (Singleton x) = show x
    show (Union s1 s2) = show (s1, s2)

instance Functor Set where
    fmap f Empty = Empty
    fmap f (Singleton x) = Singleton (f x)
    fmap f (Union s1 s2) = Union (fmap f s1) (fmap f s2)
