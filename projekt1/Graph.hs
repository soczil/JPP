module Graph where
import Set(Set)
import qualified Set as Set
class Graph g where
  empty   :: g a
  vertex  :: a -> g a
  union   :: g a -> g a -> g a
  connect :: g a -> g a -> g a

data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
    deriving (Eq, Show)

data Basic a = Empty
                | Vertex a
                | Union (Basic a) (Basic a)
                | Connect (Basic a) (Basic a)

instance Graph Relation where
    empty = Relation Set.empty Set.empty 
    vertex x = Relation (Set.singleton x) Set.empty
    union (Relation d1 r1) (Relation d2 r2) = Relation (Set.union d1 d2) (Set.union r1 r2)
    connect (Relation d1 r1) (Relation d2 r2) = 
        let cartezianProduct = [(x, y) | x <- Set.elems d1, y <- Set.elems d2]
            oldEdges = Set.union r1 r2
            newEdges = foldl (\acc pair -> Set.union (Set.singleton pair) acc) Set.empty cartezianProduct
        in Relation (Set.union d1 d2) (Set.union oldEdges newEdges)
                
instance (Ord a, Num a) => Num (Relation a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty 
    abs         = id
    negate      = id

instance Graph Basic where
    empty = Empty
    vertex = Vertex
    union = Union
    connect = Connect

instance Ord a => Eq (Basic a) where
    g1 == g2 = fromBasicToRelation g1 == fromBasicToRelation g2

instance (Ord a, Num a) => Num (Basic a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Semigroup (Basic a) where
    (<>) = union

instance Monoid (Basic a) where
    mempty = Empty

fromBasic :: Graph g => Basic a -> g a
fromBasic Empty = empty
fromBasic (Vertex x) = vertex x
fromBasic (Union g1 g2) = union (fromBasic g1) (fromBasic g2)
fromBasic (Connect g1 g2) = connect (fromBasic g1) (fromBasic g2)

fromBasicToRelation :: Basic a -> Relation a
fromBasicToRelation = fromBasic

instance (Ord a, Show a) => Show (Basic a) where
    show g = showAux $ fromBasicToRelation g
        where
            showAux (Relation d r) = 
                let edges = Set.toAscList r
                    vertices = loneVertices (Set.toAscList d) edges
                in "edges " ++ show edges ++ " + vertices " ++ show vertices

loneVertices :: Eq a => [a] -> [(a, a)] -> [a]
loneVertices [] _ = []
loneVertices (x:xs) edges
    | isConnected x edges = loneVertices xs edges
    | otherwise = x : loneVertices xs edges

isConnected :: Eq a => a -> [(a, a)] -> Bool
isConnected v [] = False
isConnected v (x:xs)
    | v == fst x || v == snd x = True
    | otherwise = isConnected v xs

-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

todot :: (Ord a, Show a) => Basic a -> String
todot g = "digraph {\n" ++ todotAux (fromBasic g) ++ "}"
    where 
        todotAux (Relation d r) =
            let edges = Set.toAscList r
                vertices = loneVertices (Set.toAscList d) edges
            in edgesToString edges ++ verticesToString vertices

edgesToString :: (Show a) => [(a, a)] -> String 
edgesToString = foldl (\acc (x, y) -> acc ++ show x ++ " -> " ++ show y ++ ";\n") ""

verticesToString :: (Show a) => [a] -> String
verticesToString = foldl (\acc x -> show x ++ ";\n") ""

instance Functor Basic where
    fmap _ Empty = Empty
    fmap f (Vertex x) = Vertex (f x) 
    fmap f (Union g1 g2) = Union (fmap f g1) (fmap f g2)
    fmap f (Connect g1 g2) = Connect (fmap f g1) (fmap f g2)

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV a b c = fmap (\x -> if x == a || x == b then c else x)

instance Applicative Basic where
    pure = Vertex
    Empty <*> _ = Empty
    Vertex f <*> g = fmap f g
    Union f1 f2 <*> g = Union (f1 <*> g) (f2 <*> g)
    Connect f1 f2 <*> g = Connect (f1 <*> g) (f2 <*> g)

instance Monad Basic where
    return = Vertex
    Empty >>= _ = Empty
    Vertex x >>= f = f x
    Union g1 g2 >>= f = Union (g1 >>= f) (g2 >>= f)
    Connect g1 g2 >>= f = Connect (g1 >>= f) (g2 >>= f)
    
-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV a b c g = do
    g1 <- renameVertices a b g
    g2 <- renameVertices a c g
    Union g1 g2

renameVertices :: (Eq a, Monad m) => a -> a -> Basic a -> m (Basic a)
renameVertices _ _ Empty = return Empty
renameVertices a b (Vertex x) = if x == a then return (Vertex b) else return (Vertex x)
renameVertices a b (Union g1 g2) = do
    g1' <- renameVertices a b g1
    g2' <- renameVertices a b g2
    return $ Union g1' g2'
renameVertices a b (Connect g1 g2) = do
    g1' <- renameVertices a b g1
    g2' <- renameVertices a b g2
    return $ Connect g1' g2'
