{-# LANGUAGE GADTs #-}

module Group where

import Data.Semigroup --hiding (Sum)
import Data.Monoid --hiding (Sum)
import Control.Applicative
import Data.Either

--homomorphism :: Morphism -> BinaryOp (Group a) ->
--homomorphism f (op a1 a2)
{-}
newtype Homomorphism a = Homomorphism (a -> b)

data Group a where
    Group :: Monoid a => [a] -> (a -> a) -> Group a
    --Abelian :: Monoid a, Commutative a => [a] -> (a -> a) -> Group a
    --Cyclic :: Monoid a, Commutative a => [(a,Int)] -> Group a

instance Monoid a => Monoid (Homomorphism a) where
    (Homomorphism a1) <> (Homomorphism a2) = Homomorphism $ a1 <> a2
    mempty (Homomorphism a) = mempty a
-}
{-}
class Semigroup a where
    (<>) :: a -> a ->  a
-- combines thing and has set closure
-- Associativity?
class Semigroup a => Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mappend = (<>)
-- combines thing in same group with identity element
-- Associativity
-}

-- TODO
-- Add Set group of functions with set composition \mathbb{S}
-- Using typed functions in order to allow for commutativity, (if abelian a->a) not if not then a->b
-- For S_n use tuples to show pairings, from there form function by function composition on tuples (this may be done best by implementing an interpretter for finite groups)
-- What Monads are Groups? What subsets of the Monads are Groups?
    -- What about taking Applicative and foldr?
-- Lists under (++) (The space is all combinations)
-- Can you isolate sets of class types by using the forall structure?
data HyperOrder = Infinite | Unknown String
    deriving (Show, Eq)

class Monoid a => Group a where
    invert :: a -> a
    ord :: a -> Either HyperOrder Integer
    ord mappend = Right 1
    --getId :: a
    --getId = mempty
    pow :: Integral x => a -> x -> a
    pow x0 n0 = case compare n0 0 of
        LT -> invert . f x0 $ negate n0
        EQ -> mempty
        GT -> f x0 n0
        where
            f x n   | even n = f (x <> x) (quot n 2)
                    | n == 1 = x
                    | otherwise = g (x <> x) (quot n 2) x
            g x n c | even n = g (x <> x) (quot n 2) c
                    | n == 1 = x <> c
                    | otherwise = g (x <> x) (quot n 2) (x <> c)

------------------------------------------------------------------
{- newtype Sum a = Sum a
    deriving (Show)

instance Num a => Semigroup (Sum a) where
    (<>) (Sum a) (Sum b) = Sum $ a + b

instance Num a => Monoid (Sum a) where
    mempty = Sum 0
-}
instance Num a => Group (Sum a) where
    invert (Sum a) = Sum $ negate a
    pow (Sum a) b = Sum (a * fromIntegral b)
    ord _ = Left Infinite

-------------------------------------------------------------------
{-newtype Prod a = Prod a
    deriving (Show)

instance Fractional a => Semigroup (Prod a) where
    (<>) (Prod a) (Prod b) = Prod $ a*b

instance Fractional a => Monoid (Prod a) where
    mempty = Prod 1
-}
instance Fractional a => Group (Product a) where
    invert (Product a) = Product $ 1 / a
    pow (Product a) b = Product (a ^^ b)
    ord _ = Left Infinite

---------------------------------------------------------------
-- RIPPED FROM: https://hackage.haskell.org/package/groups-0.5.2/docs/src/Data.Group.html
-- NOT MY WORK
instance Group () where
  invert () = ()
  pow _ _ = ()

instance Group b => Group (a -> b) where
    -- On free bijective functions (pearl): http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.143.3248&rep=rep1&type=pdf
  invert f = invert . f
  pow f n e = pow (f e) n
  ord _ = Left $ Unknown "Functions?"

{-
    NOTE:
    Group (a, b) is taken to the result of the direct product between Group a and Group b
    a,b are groups => a x b = (a,b)
    order of this is defined to be the product of the orders
-}

instance (Group a, Group b) => Group (a, b) where
  invert (a, b) = (invert a, invert b)
  pow (a, b) n = (pow a n, pow b n)
  ord (a, b) = liftA2 (*) (ord a) (ord b)

instance (Group a, Group b, Group c) => Group (a, b, c) where
  invert (a, b, c) = (invert a, invert b, invert c)
  pow (a, b, c) n = (pow a n, pow b n, pow c n)
  ord (a, b, c) = liftA2 (*) (ord a) $ liftA2 (*) (ord b) (ord c)

instance (Group a, Group b, Group c, Group d) => Group (a, b, c, d) where
  invert (a, b, c, d) = (invert a, invert b, invert c, invert d)
  pow (a, b, c, d) n = (pow a n, pow b n, pow c n, pow d n)
  ord (a, b, c, d) = liftA2 (*) (ord a) $ liftA2 (*) (ord b) $ liftA2 (*) (ord c) (ord d)

instance (Group a, Group b, Group c, Group d, Group e) => Group (a, b, c, d, e) where
  invert (a, b, c, d, e) = (invert a, invert b, invert c, invert d, invert e)
  pow (a, b, c, d, e) n = (pow a n, pow b n, pow c n, pow d n, pow e n)
  ord (a, b, c, d, e) = liftA2 (*) (ord a) $ liftA2 (*) (ord b) $ liftA2 (*) (ord c) $ liftA2 (*) (ord d) (ord e)


-- This means the group is Commutative
-- So, a <> b == b <> a
class Group a => Abelian a
instance Abelian ()

instance Num a => Abelian (Sum a)

instance Fractional a => Abelian (Product a)

instance Abelian b => Abelian (a -> b)

instance (Abelian a, Abelian b) => Abelian (a, b)

instance (Abelian a, Abelian b, Abelian c) => Abelian (a, b, c)

instance (Abelian a, Abelian b, Abelian c, Abelian d) => Abelian (a, b, c, d)

instance (Abelian a, Abelian b, Abelian c, Abelian d, Abelian e) => Abelian (a, b, c, d, e)
--------------------------------------------------------------------------

class Abelian a => Cyclic a where
    generator :: a

{-}
--[Left Inverse] invert_left <> a = e
--[Right Inverse] a <> invert_right = e
class Monoid a => Group a where
    --set :: Set a
    --closure :: Set a -> (a -> a -> a) -> Bool
    invert :: a -> a

class Monoid a => Commutative a

class Group a => Abelian a

class Abelian a => Cyclic a where
    generator :: a

instance Abelian a => Num (Sum a) where
    invert = negate
--overlapping instances


instance Abelian a => Fractional (Product a) where
    invert a   | a == 0 = error "cannot invert 0"
                | otherwise = 1 / a
-}
-- From Monoid we inherit the following
-- [Right identity] @x '<>' 'mempty' = x@
-- [Left identity]  @'mempty' '<>' x = x@
-- [Associativity]  @x '<>' (y '<>' z) = (x '<>' y) '<>' z@ ('Semigroup' law)
-- [Concatenation]  @'mconcat' = 'foldr' ('<>') 'mempty'@

--isHomomorphic :: (Group a, Group b) => (a -> b) -> a -> a -> Bool
--isHomomorphic f g1 g2 = fmap f (g1 <> g2) == fmap f g1 <> fmap f g2
