module Data.PSQueue.Internal 
  (
  -- * Binding Type
    Binding(..)
  , key
  , prio
  -- * Priority Search Queue Type
  , PSQ(..)
  -- * Query
  , size
  , null
  , lookup
  -- * Construction
  , empty
  , singleton
  -- * Insertion
  , insert
  , insertWith
  , insertWithKey
  -- * Delete/Update
  , delete
  , adjust
  , adjustWithKey
  , update
  , updateWithKey
  , alter
  -- * Conversion
  , keys
  , fromList
  , fromAscList
  , fromDistinctAscList
  , foldm
  , toList
  , toAscList
  , toAscLists
  , toDescList
  , toDescLists
  -- * Priority Queue
  , findMin
  , deleteMin
  , minView
  , secondBest
  , atMost
  , atMosts
  , atMostRange
  , atMostRanges
  , inrange
  -- * Fold
  , foldr
  , foldl
  -- * Internals
  , Size
  , LTree(..)
  , size'
  , left
  , right
  , maxKey
  , lloser
  , rloser
  , omega
  , lbalance
  , rbalance
  , lbalanceLeft
  , lbalanceRight
  , rbalanceLeft
  , rbalanceRight
  , lsingleLeft
  , rsingleLeft
  , lsingleRight
  , rsingleRight
  , ldoubleLeft
  , ldoubleRight
  , rdoubleLeft
  , rdoubleRight
  , play
  , TourView(..)
  , tourView
  ) where

import           Data.Function (on)
import           Prelude hiding (foldl, foldr, lookup, null)
import qualified Prelude as P

-- | @k :-> p@ binds the key @k@ with the priority @p@.
data Binding k p = !k :-> !p deriving (Eq,Ord,Show,Read)

infix 0 :->

-- | The key of a binding
key  :: Binding k p -> k
key  (k :-> _) =  k

-- | The priority of a binding
prio :: Binding k p -> p
prio (_ :-> p) =  p


-- | A mapping from keys @k@ to priorites @p@.

data PSQ k p = Void | Winner !k !p !(LTree k p) !k

instance (Show k, Show p) => Show (PSQ k p) where
  show = show . toAscList
  --show Void = "[]"
  --show (Winner k1 p lt k2) = "Winner "++show k1++" "++show p++" ("++show lt++") "++show k2

instance (Eq k, Eq p) => Eq (PSQ k p) where
  (==) = (==) `on` toAscList


-- | /O(1)/ The number of bindings in a queue.
size :: PSQ k p -> Int
size Void              = 0
size (Winner _ _ lt _) = 1 + size' lt

-- | /O(1)/ True if the queue is empty.
null :: PSQ k p -> Bool
null Void             = True
null (Winner _ _ _ _) = False

-- | /O(log n)/ The priority of a given key, or Nothing if the key is not
-- bound.
{-# INLINABLE lookup #-}
lookup :: Ord k => k -> PSQ k p -> Maybe p
lookup k q =
  case tourView q of
    Null -> fail "PSQueue.lookup: Empty queue"
    Single k' p
      | k == k'   -> return p
      | otherwise -> fail "PSQueue.lookup: Key not found"
    tl `Play` tr
      | k <= maxKey tl -> lookup k tl
      | otherwise      -> lookup k tr



empty :: PSQ k p
empty = Void

-- | O(1) Build a queue with one binding.
singleton :: k -> p -> PSQ k p
singleton k p =  Winner k p Start k


-- | /O(log n)/ Insert a binding into the queue.
{-# INLINABLE insert #-}
insert :: (Ord k, Ord p) => k -> p -> PSQ k p -> PSQ k p
insert k p q =
  case tourView q of
    Null -> singleton k p
    Single k' p' ->
      case compare k k' of
        LT -> singleton k  p  `play` singleton k' p'
        EQ -> singleton k  p
        GT -> singleton k' p' `play` singleton k  p
    tl `Play` tr
      | k <= maxKey tl -> insert k p tl `play` tr
      | otherwise      -> tl `play` insert k p tr


-- | /O(log n)/ Insert a binding with a combining function.
insertWith :: (Ord k, Ord p) => (p->p->p) -> k -> p -> PSQ k p -> PSQ k p
insertWith f = insertWithKey (\_ p p'-> f p p')

-- | /O(log n)/ Insert a binding with a combining function.
{-# INLINABLE insertWithKey #-}
insertWithKey :: (Ord k, Ord p) => (k->p->p->p) -> k -> p -> PSQ k p -> PSQ k p
insertWithKey f k p q =
  case tourView q of
    Null -> singleton k p
    Single k' p' ->
      case compare k k' of
        LT -> singleton k  p  `play` singleton k' p'
        EQ -> singleton k  (f k p p')
        GT -> singleton k' p' `play` singleton k  p
    tl `Play` tr
      | k <= maxKey tl -> insertWithKey f k p tl `play` tr
      | otherwise      -> tl `play` insertWithKey f k p tr



-- | /O(log n)/ Remove a binding from the queue.
{-# INLINABLE delete #-}
delete :: (Ord k, Ord p) => k -> PSQ k p -> PSQ k p
delete k q =
  case tourView q of
    Null -> empty
    Single k' p
      | k == k'   -> empty
      | otherwise -> singleton k' p
    tl `Play` tr
      | k <= maxKey tl -> delete k tl `play` tr
      | otherwise      -> tl `play` delete k tr

-- | /O(log n)/ Adjust the priority of a key.
adjust ::  (Ord p, Ord k) => (p -> p) -> k -> PSQ k p -> PSQ k p
adjust f = adjustWithKey (\_ p -> f p)

-- | /O(log n)/ Adjust the priority of a key.
{-# INLINABLE adjustWithKey #-}
adjustWithKey :: (Ord k, Ord p) => (k -> p -> p) -> k -> PSQ k p -> PSQ k p
adjustWithKey f k q =
  case tourView q of
    Null -> empty
    Single k' p
      | k == k'   -> singleton k' (f k p)
      | otherwise -> singleton k' p
    tl `Play` tr
      | k <= maxKey tl -> adjustWithKey f k tl `play` tr
      | otherwise      -> tl `play` adjustWithKey f k tr


-- | /O(log n)/ The expression (@update f k q@) updates the
-- priority @p@ bound @k@ (if it is in the queue). If (@f p@) is 'Nothing',
-- the binding is deleted. If it is (@'Just' z@), the key @k@ is bound
-- to the new priority @z@.

update :: (Ord k, Ord p) => (p -> Maybe p) -> k -> PSQ k p -> PSQ k p
update f = updateWithKey (\_ p -> f p)

-- | /O(log n)/. The expression (@updateWithKey f k q@) updates the
-- priority @p@ bound @k@ (if it is in the queue). If (@f k p@) is 'Nothing',
-- the binding is deleted. If it is (@'Just' z@), the key @k@ is bound
-- to the new priority @z@.

{-# INLINABLE updateWithKey #-}
updateWithKey :: (Ord k, Ord p) => (k -> p -> Maybe p) -> k -> PSQ k p -> PSQ k p
updateWithKey f k q =
  case tourView q of
    Null -> empty
    Single k' p
      | k==k' -> case f k p of
                  Nothing -> empty
                  Just p' -> singleton k p'
      | otherwise -> singleton k' p
    tl `Play` tr
      | k <= maxKey tl -> updateWithKey f k tl `play` tr
      | otherwise      -> tl `play` updateWithKey f k tr


-- | /O(log n)/. The expression (@'alter' f k q@) alters the priority @p@ bound to @k@, or absence thereof.
-- alter can be used to insert, delete, or update a priority in a queue.

{-# INLINABLE alter #-}
alter :: (Ord k, Ord p) => (Maybe p -> Maybe p) -> k -> PSQ k p -> PSQ k p
alter f k q =
  case tourView q of
    Null ->
      case f Nothing of
        Nothing -> empty
        Just p  -> singleton k p
    Single k' p
      | k == k'   ->  case f (Just p) of
                        Nothing -> empty
                        Just p' -> singleton k' p'
      | otherwise ->  case f Nothing of
                        Nothing -> singleton k' p
                        Just p' -> insert k p' $ singleton k' p
    tl `Play` tr
      | k <= maxKey tl -> alter f k tl `play` tr
      | otherwise      -> tl `play` alter f k tr



-- | /O(n)/ The keys of a priority queue
keys :: PSQ k p -> [k]
keys = map key . toList

-- | /O(n log n)/ Build a queue from a list of bindings.
fromList :: (Ord k, Ord p) => [Binding k p] -> PSQ k p
fromList = P.foldr (\(k:->p) q -> insert k p q) empty

-- | /O(n)/ Build a queue from a list of bindings in order of
-- ascending keys. The precondition that the keys are ascending is not checked.
{-# INLINABLE fromAscList #-}
fromAscList :: (Eq k, Ord p) => [Binding k p] -> PSQ k p
fromAscList = fromDistinctAscList . stripEq
  where stripEq []     = []
        stripEq (x:xs) = stripEq' x xs
        stripEq' x' []     = [x']
        stripEq' x' (x:xs)
          | x' == x   = stripEq' x' xs
          | otherwise = x' : stripEq' x xs

-- | /O(n)/ Build a queue from a list of distinct bindings in order of
-- ascending keys. The precondition that keys are distinct and ascending is not checked.
{-# INLINABLE fromDistinctAscList #-}
fromDistinctAscList :: Ord p => [Binding k p] -> PSQ k p
fromDistinctAscList = foldm play empty . map (\(k:->p) -> singleton k p)

-- Folding a list in a binary-subdivision scheme.
foldm :: (a -> a -> a) -> a -> [a] -> a
foldm (*) e x
  | P.null  x             = e
  | otherwise             = fst (rec (length x) x)
  where rec 1 (a : as)    = (a, as)
        rec n as          = (a1 * a2, as2)
          where m         = n `div` 2
                (a1, as1) = rec (n - m) as
                (a2, as2) = rec m       as1

-- | /O(n)/ Convert a queue to a list.
toList :: PSQ k p -> [Binding k p]
toList = toAscList

-- | /O(n)/ Convert a queue to a list in ascending order of keys.
toAscList :: PSQ k p -> [Binding k p]
toAscList q  = seqToList (toAscLists q)

toAscLists :: PSQ k p -> Sequ (Binding k p)
toAscLists q = case tourView q of
  Null         -> emptySequ
  Single k p   -> singleSequ (k :-> p)
  tl `Play` tr -> toAscLists tl <+> toAscLists tr

-- | /O(n)/ Convert a queue to a list in descending order of keys.
toDescList :: PSQ k p -> [ Binding k p ]
toDescList q = seqToList (toDescLists q)

toDescLists :: PSQ k p -> Sequ (Binding k p)
toDescLists q = case tourView q of
  Null         -> emptySequ
  Single k p   -> singleSequ (k :-> p)
  tl `Play` tr -> toDescLists tr <+> toDescLists tl


-- | /O(1)/ The binding with the lowest priority.
findMin :: PSQ k p -> Maybe (Binding k p)
findMin Void             = Nothing
findMin (Winner k p t m) = Just (k :-> p)

-- | /O(log n)/ Remove the binding with the lowest priority.
deleteMin :: Ord p => PSQ k p -> PSQ k p
deleteMin Void             = Void
deleteMin (Winner k p t m) = secondBest t m

-- | /O(log n)/ Retrieve the binding with the least priority, and the rest of
-- the queue stripped of that binding.
minView :: Ord p => PSQ k p -> Maybe (Binding k p, PSQ k p)
minView Void             = Nothing
minView (Winner k p t m) = Just ( k :-> p , secondBest t m )

{-# INLINABLE secondBest #-}
secondBest :: Ord p => LTree k p -> k -> PSQ k p
secondBest Start _m                  = Void
secondBest (LLoser _ k p tl m tr) m' = Winner k p tl m `play` secondBest tr m'
secondBest (RLoser _ k p tl m tr) m' = secondBest tl m `play` Winner k p tr m'



-- | /O(r(log n - log r)/ @atMost p q@ is a list of all the bindings in @q@ with
-- priority less than @p@, in order of ascending keys.
-- Effectively,
--
-- @
--   atMost p' q = filter (\\(k:->p) -> p<=p') . toList
-- @
atMost :: Ord p => p -> PSQ k p -> [Binding k p]
atMost pt q = seqToList (atMosts pt q)

atMosts :: Ord p => p -> PSQ k p -> Sequ (Binding k p)
atMosts _pt Void  = emptySequ
atMosts pt (Winner k p t _) =  prune k p t
  where
  prune k p t
    | p > pt         = emptySequ
    | otherwise      = traverse k p t
  traverse k p Start                     = singleSequ (k :-> p)
  traverse k p (LLoser _ k' p' tl _m tr) = prune k' p' tl <+> traverse k p tr
  traverse k p (RLoser _ k' p' tl _m tr) = traverse k p tl <+> prune k' p' tr

-- | /O(r(log n - log r))/ @atMostRange p (l,u) q@ is a list of all the bindings in
-- @q@ with a priority less than @p@ and a key in the range @(l,u)@ inclusive.
-- Effectively,
--
-- @
--    atMostRange p' (l,u) q = filter (\\(k:->p) -> l<=k && k<=u ) . 'atMost' p'
-- @
{-# INLINABLE atMostRange #-}
atMostRange :: (Ord k, Ord p) => p -> (k, k) -> PSQ k p -> [Binding k p]
atMostRange pt (kl, kr) q = seqToList (atMostRanges pt (kl, kr) q)

{-# INLINABLE atMostRanges #-}
atMostRanges :: (Ord k, Ord p) => p -> (k, k) -> PSQ k p -> Sequ (Binding k p)
atMostRanges _pt _range Void = emptySequ
atMostRanges pt range@(kl, kr) (Winner k p t _) = prune k p t
  where
  prune k p t
    | p > pt    = emptySequ
    | otherwise = traverse k p t
  traverse k p Start
    | k `inrange` range = singleSequ (k :-> p)
    | otherwise         = emptySequ
  traverse k p (LLoser _ k' p' tl m tr) =
    guard (kl <= m) (prune k' p' tl) <+> guard (m <= kr) (traverse k p tr)
  traverse k p (RLoser _ k' p' tl m tr) =
    guard (kl <= m) (traverse k p tl) <+> guard (m <= kr) (prune k' p' tr)

{-# INLINE inrange #-}
inrange :: Ord a => a -> (a, a) -> Bool
a `inrange` (l, r)  =  l <= a && a <= r




-- | Right fold over the bindings in the queue, in key order.
foldr :: (Binding k p -> b -> b) -> b -> PSQ k p -> b
foldr f z q =
  case tourView q of
    Null       -> z
    Single k p -> f (k:->p) z
    l`Play`r   -> foldr f (foldr f z r) l


-- | Left fold over the bindings in the queue, in key order.
foldl :: (b -> Binding k p -> b) -> b -> PSQ k p -> b
foldl f z q =
  case tourView q of
    Null       -> z
    Single k p -> f z (k:->p)
    l`Play`r   -> foldl f (foldl f z l) r




-----------------------
------- Internals -----
----------------------

type Size = Int

data LTree k p = Start
               | LLoser {-# UNPACK #-}!Size !k !p !(LTree k p) !k !(LTree k p)
               | RLoser {-# UNPACK #-}!Size !k !p !(LTree k p) !k !(LTree k p)


size' :: LTree k p -> Size
size' Start                = 0
size' (LLoser s _ _ _ _ _) = s
size' (RLoser s _ _ _ _ _) = s

left, right :: LTree a b -> LTree a b

left  Start                  =  error "left: empty loser tree"
left  (LLoser _ _ _ tl _ _ ) =  tl
left  (RLoser _ _ _ tl _ _ ) =  tl

right Start                  =  error "right: empty loser tree"
right (LLoser _ _ _ _  _ tr) =  tr
right (RLoser _ _ _ _  _ tr) =  tr

maxKey :: PSQ k p -> k
maxKey Void                =  error "maxKey: empty queue"
maxKey (Winner _k _p _t m) =  m

lloser, rloser :: k -> p -> LTree k p -> k -> LTree k p -> LTree k p
lloser k p tl m tr =  LLoser (1 + size' tl + size' tr) k p tl m tr
rloser k p tl m tr =  RLoser (1 + size' tl + size' tr) k p tl m tr

--balance factor
omega :: Int
omega = 4

{-# INLINABLE lbalance #-}
{-# INLINABLE rbalance #-}
lbalance, rbalance ::
  Ord p => k -> p -> LTree k p -> k -> LTree k p -> LTree k p

lbalance k p l m r
  | size' r + size' l < 2     = lloser        k p l m r
  | size' r > omega * size' l = lbalanceLeft  k p l m r
  | size' l > omega * size' r = lbalanceRight k p l m r
  | otherwise                 = lloser        k p l m r

rbalance k p l m r
  | size' r + size' l < 2     = rloser        k p l m r
  | size' r > omega * size' l = rbalanceLeft  k p l m r
  | size' l > omega * size' r = rbalanceRight k p l m r
  | otherwise                 = rloser        k p l m r

{-# INLINABLE lbalanceLeft #-}
lbalanceLeft :: Ord p => k -> p -> LTree k p -> k -> LTree k p -> LTree k p
lbalanceLeft  k p l m r
  | size' (left r) < size' (right r) = lsingleLeft  k p l m r
  | otherwise                      = ldoubleLeft  k p l m r

{-# INLINABLE lbalanceRight #-}
lbalanceRight :: Ord p => k -> p -> LTree k p -> k -> LTree k p -> LTree k p
lbalanceRight k p l m r
  | size' (left l) > size' (right l) = lsingleRight k p l m r
  | otherwise                      = ldoubleRight k p l m r

{-# INLINABLE rbalanceLeft #-}
rbalanceLeft :: Ord p => k -> p -> LTree k p -> k -> LTree k p -> LTree k p
rbalanceLeft  k p l m r
  | size' (left r) < size' (right r) = rsingleLeft  k p l m r
  | otherwise                      = rdoubleLeft  k p l m r

{-# INLINABLE rbalanceRight #-}
rbalanceRight :: Ord p => k -> p -> LTree k p -> k -> LTree k p -> LTree k p
rbalanceRight k p l m r
  | size' (left l) > size' (right l) = rsingleRight k p l m r
  | otherwise                      = rdoubleRight k p l m r



{-# INLINABLE lsingleLeft #-}
lsingleLeft :: Ord p => k -> p -> LTree k p -> k -> LTree k p -> LTree k p
lsingleLeft k1 p1 t1 m1 (LLoser _ k2 p2 t2 m2 t3)
  | p1 <= p2  = lloser k1 p1 (rloser k2 p2 t1 m1 t2) m2 t3
  | otherwise = lloser k2 p2 (lloser k1 p1 t1 m1 t2) m2 t3

lsingleLeft k1 p1 t1 m1 (RLoser _ k2 p2 t2 m2 t3) =
  rloser k2 p2 (lloser k1 p1 t1 m1 t2) m2 t3

rsingleLeft :: k -> p -> LTree k p -> k -> LTree k p -> LTree k p
rsingleLeft k1 p1 t1 m1 (LLoser _ k2 p2 t2 m2 t3) =
  rloser k1 p1 (rloser k2 p2 t1 m1 t2) m2 t3

rsingleLeft k1 p1 t1 m1 (RLoser _ k2 p2 t2 m2 t3) =
  rloser k2 p2 (rloser k1 p1 t1 m1 t2) m2 t3

lsingleRight :: k -> p -> LTree k p -> k -> LTree k p -> LTree k p
lsingleRight k1 p1 (LLoser _ k2 p2 t1 m1 t2) m2 t3 =
  lloser k2 p2 t1 m1 (lloser k1 p1 t2 m2 t3)

lsingleRight k1 p1 (RLoser _ k2 p2 t1 m1 t2) m2 t3 =
  lloser k1 p1 t1 m1 (lloser k2 p2 t2 m2 t3)

{-# INLINABLE rsingleRight #-}
rsingleRight :: Ord p => k -> p -> LTree k p -> k -> LTree k p -> LTree k p
rsingleRight k1 p1 (LLoser _ k2 p2 t1 m1 t2) m2 t3 =
  lloser k2 p2 t1 m1 (rloser k1 p1 t2 m2 t3)

rsingleRight k1 p1 (RLoser _ k2 p2 t1 m1 t2) m2 t3
  | p1 <= p2  = rloser k1 p1 t1 m1 (lloser k2 p2 t2 m2 t3)
  | otherwise = rloser k2 p2 t1 m1 (rloser k1 p1 t2 m2 t3)


{-# INLINABLE ldoubleLeft #-}
ldoubleLeft :: Ord p => k -> p -> LTree k p -> k -> LTree k p -> LTree k p
ldoubleLeft k1 p1 t1 m1 (LLoser _ k2 p2 t2 m2 t3) =
  lsingleLeft k1 p1 t1 m1 (lsingleRight k2 p2 t2 m2 t3)

ldoubleLeft k1 p1 t1 m1 (RLoser _ k2 p2 t2 m2 t3) =
  lsingleLeft k1 p1 t1 m1 (rsingleRight k2 p2 t2 m2 t3)

{-# INLINABLE ldoubleRight #-}
ldoubleRight :: Ord p => k -> p -> LTree k p -> k -> LTree k p -> LTree k p
ldoubleRight k1 p1 (LLoser _ k2 p2 t1 m1 t2) m2 t3 =
  lsingleRight k1 p1 (lsingleLeft k2 p2 t1 m1 t2) m2 t3

ldoubleRight k1 p1 (RLoser _ k2 p2 t1 m1 t2) m2 t3 =
  lsingleRight k1 p1 (rsingleLeft k2 p2 t1 m1 t2) m2 t3

{-# INLINABLE rdoubleLeft #-}
rdoubleLeft :: Ord p => k -> p -> LTree k p -> k -> LTree k p -> LTree k p
rdoubleLeft k1 p1 t1 m1 (LLoser _ k2 p2 t2 m2 t3) =
  rsingleLeft k1 p1 t1 m1 (lsingleRight k2 p2 t2 m2 t3)

rdoubleLeft k1 p1 t1 m1 (RLoser _ k2 p2 t2 m2 t3) =
  rsingleLeft k1 p1 t1 m1 (rsingleRight k2 p2 t2 m2 t3)

{-# INLINABLE rdoubleRight #-}
rdoubleRight :: Ord p => k -> p -> LTree k p -> k -> LTree k p -> LTree k p
rdoubleRight k1 p1 (LLoser _ k2 p2 t1 m1 t2) m2 t3 =
  rsingleRight k1 p1 (lsingleLeft k2 p2 t1 m1 t2) m2 t3

rdoubleRight k1 p1 (RLoser _ k2 p2 t1 m1 t2) m2 t3 =
  rsingleRight k1 p1 (rsingleLeft k2 p2 t1 m1 t2) m2 t3

{-# INLINABLE play #-}
play :: Ord p => PSQ k p -> PSQ k p -> PSQ k p

Void `play` t' = t'
t `play` Void  = t

Winner k p t m  `play`  Winner k' p' t' m'
  | p <= p'   = Winner k  p  (rbalance k' p' t m t') m'
  | otherwise = Winner k' p' (lbalance k  p  t m t') m'


data TourView k p = Null | Single !k !p | !(PSQ k p) `Play` !(PSQ k p)

tourView :: PSQ k p -> TourView k p
{-# INLINE tourView #-}
tourView Void                  =  Null
tourView (Winner k p Start _m) =  Single k p

tourView (Winner k p (RLoser _ k' p' tl m tr) m') =
  Winner k  p  tl m `Play` Winner k' p' tr m'

tourView (Winner k p (LLoser _ k' p' tl m tr) m') =
  Winner k' p' tl m `Play` Winner k  p  tr m'






--------------------------------------
-- Hughes's efficient sequence type --
--------------------------------------

emptySequ    :: Sequ a
singleSequ   :: a -> Sequ a
(<+>)        :: Sequ a -> Sequ a -> Sequ a
seqFromList  :: [a] -> Sequ a
seqFromListT :: ([a] -> [a]) -> Sequ a
seqToList    :: Sequ a -> [a]

infixr 5 <+>

newtype Sequ a  =  Sequ ([a] -> [a])

emptySequ           = Sequ (\as -> as)
singleSequ a        = Sequ (\as -> a : as)
Sequ x1 <+> Sequ x2 = Sequ (\as -> x1 (x2 as))
seqFromList as      = Sequ (\as' -> as ++ as')
seqFromListT as     = Sequ as
seqToList (Sequ x)  = x []

instance Show a => Show (Sequ a) where
    showsPrec d a = showsPrec d (seqToList a)

guard :: Bool -> Sequ a -> Sequ a
guard False _as = emptySequ
guard True  as  = as
