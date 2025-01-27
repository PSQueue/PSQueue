
import Data.PSQueue.Internal

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.List (sort)

isBalanced Start = True
isBalanced (LLoser s k p l m r) =
  (size' l + size' r <= 2 ||(size' l<=omega*size' r && size' r<=omega*size' l))
  && isBalanced l && isBalanced r
isBalanced (RLoser s k p l m r) =
  (size' l + size' r <= 2 ||(size' l<=omega*size' r && size' r<=omega*size' l))
  && isBalanced l && isBalanced r

instance (Ord k, Ord p, Arbitrary k, Arbitrary p) => Arbitrary (PSQ k p)
  where
    arbitrary =
      do ks <- arbitrary
         ps <- arbitrary
         return . fromList $ zipWith (:->) ks ps

prop_Balanced :: PSQ Int Int -> Bool
prop_Balanced Void = True
prop_Balanced (Winner _ _ t _) = isBalanced t

prop_OrderedKeys :: PSQ Int Int -> Bool
prop_OrderedKeys t = let ks = map key . toAscList $ t in sort ks == ks

prop_AtMost :: (PSQ Int Int,Int) -> Bool
prop_AtMost (t,p) =
  let ps = map prio . atMost p $ t
  in all (<=p) ps

prop_AtMostRange :: (PSQ Int Int,Int,Int,Int) -> Bool
prop_AtMostRange (t,p,l_,r_) =
  let l = min (abs l_) (abs r_)
      r = max (abs l_) (abs r_)
      (ks,ps) = unzip . map (\b -> (key b,prio b)) . atMostRange p (l,r) $ t
  in  all (flip inrange (l,r)) ks && all (<=p) ps

prop_MinView :: PSQ Int Int -> Bool
prop_MinView t =
  case minView t of
    Nothing -> True
    Just (b1,t') ->
      case minView t' of
        Nothing -> True
        Just (b2,_) -> prio b1 <= prio b2 && prop_MinView t'

main = defaultMain $ testGroup "Tests"
  [ testProperty "Balanced" prop_Balanced
  , testProperty "OrderedKeys" prop_OrderedKeys
  , testProperty "MinView" prop_MinView
  , testProperty "AtMost" prop_AtMost
  , testProperty "AtMostRange" prop_AtMostRange
  ]
