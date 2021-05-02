{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}


module Group where

import Data.Semigroup --hiding (Any, All)
import Data.Monoid --hiding (Any, All)
import Control.Applicative
import Data.Either
import Data.Bits (xor)
import GHC.TypeLits
import Data.Proxy

{-}

data Group a where
    Group :: Monoid a => [a] -> (a -> a) -> Group a
    --Abelian :: Monoid a, Commutative a => [a] -> (a -> a) -> Group a
    --Cyclic :: Monoid a, Commutative a => [(a,Int)] -> Group a

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

-- Used for getting the order of an element, where ord = n st x^n = id
data HyperOrder = Infinite | Unknown String
    deriving (Show, Eq)

class Monoid a => Group a where
    invert :: a -> a -- get the inverse of an element
    invert mempty = mempty

    ord :: a -> Either HyperOrder Integer -- get the order of an element
    ord mempty = Right 1

    pow :: Integral x => a -> x -> a --take an element to an integral power
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
instance Num a => Group (Sum a) where -- A Sum-type wrapper for numeric groups
    invert (Sum a) = Sum $ negate a
    pow (Sum a) b = Sum (a * fromIntegral b)
    ord _ = Left Infinite

instance Fractional a => Group (Product a) where -- A product-type wrapper for numeric groups
    invert (Product a) = Product $ 1 / a
    pow (Product a) b = Product (a ^^ b)
    ord _ = Left Infinite


----------------------------------------------
-- This is me trying to test the stuff from: https://wiki.haskell.org/wikiupload/1/11/Hiw2012-iavor-diatchki.pdf

newtype Test (n :: Nat) = Test Integer
    deriving Show

newtype Sing (n :: Nat) = SNat Integer
fromSing :: Sing n -> Integer
fromSing (SNat n) = n

class SingI n where
    sing :: Sing n

size :: SingI n => Test n -> Sing n
size _ = sing


{-
data family Sing n

newtype instance Sing n = SNat Integer
fromSing (SNat n) = n

singToNum :: Num a => Sing (n :: Nat) -> a
singToNum = fromInteger . fromSing

numToSing :: Integer -> Sing (n :: Nat)
numToSing = SNat

test3 :: Sing (n :: Nat) -> Integer
test3 = fromSing
-}
------------------------------------------------------
--Implmentation of finite Abelian groups
--phantom typing
newtype Z_n p = Z_n Integer
    deriving Show

-- Equivalence under modulo p => Covers things < 0 and >= p
instance TypeNum p => Eq (Z_n p) where
    a == b = case a <> invert b of
        Z_n 0   -> True
        _       -> False

-- Order under modulo p => Covers things < 0 and >= p
instance TypeNum p => Ord (Z_n p) where
    a <= b = a1 <= b1
        where
            Z_n a1 = a <> mempty -- forces things outside of [0, p-1] into the modulo range
            Z_n b1 = b <> mempty

class TypeNum p where
    typeNum :: p -> Integer

fixTypeNum :: TypeNum p => (Integer -> Z_n p) -> Z_n p
fixTypeNum f = r
    where
        r = f (typeNum $ ty r)
        ty :: Z_n p -> p
        ty _ = undefined

data Four = Four
instance TypeNum Four where
    typeNum _ = 4

instance TypeNum () where
    typeNum _ = 0

mkTest2 :: TypeNum p => Integer -> Z_n p
mkTest2 x = fixTypeNum $ \n -> Z_n $ mod x n

instance TypeNum a => Semigroup (Z_n a) where
    (Z_n a) <> (Z_n b) = mkTest2 $ a + b

instance TypeNum a => Monoid (Z_n a) where
    mempty = Z_n 0

instance TypeNum a => Group (Z_n a) where
    invert (Z_n x) = fixTypeNum $ \n -> Z_n $ n - x
    ord (Z_n x) = Left $ Unknown "Unknown "--case fixTypeNum $ \n -> Z_n $ n of
        --Z_n n -> Right (cd x n ::Integer)

instance TypeNum a => Abelian (Z_n a)
instance TypeNum a => Cyclic (Z_n a) where
    generator = Z_n 1

-- If you take group actions applied to a Tree
-- G acts on BT => Should preserve order of nodes in the tree

{-
LOOOK
https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/type-level-literals.html
GHC.TypeLits> natVal (Proxy :: Proxy (2 + 3))
import GHC.TypeLits
import Data.Proxy
https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-TypeLits.html
Example: https://wiki.haskell.org/wikiupload/1/11/Hiw2012-iavor-diatchki.pdf
-}

-------------------------------------------------------------------

--Me trying to add Z_2 using truth tables
{-}
instance Semigroup (Sum Bool) where
    (Sum a) <> (Sum b) = Sum $ xor a b

instance Semigroup (Product Bool) where
     (Product a) <> (Product b) = Product $ a == b

instance Monoid (Sum Bool) where
    mempty = Sum False

instance Monoid (Product Bool) where
    mempty = Product True

instance Group (Sum Bool) where
    invert a = a
    pow (Sum True) b    | even b = Sum True
                        | otherwise = Sum False
    pow _ _ = Sum False
    ord (Sum True) = Right 2
    ord _ = Right 1

instance Group (Product Bool) where
    invert a = a
    pow (Product False) b   | even b = Product False
                            | otherwise = Product True
    pow _ _ = Product True
    ord (Product False) = Right 2
    ord _ = Right 1
-}

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

instance Integral a => Cyclic (Sum a) where
    generator = 1

--isHomomorphic :: (Group a, Group b) => (a -> b) -> a -> a -> Bool
--isHomomorphic f g1 g2 = fmap f (g1 <> g2) == fmap f g1 <> fmap f g2
{-
class (Group a, Group b) => Homomorphism (a -> b) where

-}

-- <.> := f(a_g + b_g)

{-
isHomomorphic :: (Group a, Group b) => (a -> b) -> a -> a -> Bool
isHomomorphic f g1 g2 = c && m
    where
        c = f (g1 <> g2) == f g1 <> f g2
        m = (f mempty) == mempty
-}

--data Homomorphism a = Phi (a -> a -> b)

{-
class Homomorphism a b where
    phi :: (Group a, Group b) => a -> b
    (<.>) :: (Group a, Group b) => a -> a -> b
    a <.> b = (phi a) <> (phi b)
--    isHomomorphic :: (Group a, Group b) => a -> a -> Bool
--    isHomomorphic a b = (phi $ a <> b) == (phi a) <> (phi b)

instance Homomorphism (Sum Int) (Product Int) where
    phi (Sum a) = Product $ mod a 2

homomorphism :: (Group a, Group b) => (a -> b) -> a -> a -> b
homomorphism f a1 a2 = (f a1) <> (f a2)
-}

-- Me trying to implement group actions
-- A group action is a relation between the bijective functions on a set applied to itself
-- ex, symmetries of a triangle => S_3 
data GAction g a where
    GX :: Group g => (g -> a) -> GAction g a

--gAct :: Group g => (g -> a) -> g -> a

{-
instance Semigroup (GAction) where
    (GX g1 gx) <> (GX g2 _) = GX (g1 <> g2) gx
-}

runGX :: GAction g a -> (g -> a)
runGX (GX g) = g


askGX :: Group g => GAction g g
askGX = GX $ \ g -> g
{-}

instance Monad GAction where
    return a = GX $ \ _ -> a
    GX f >>= k = GX \k f


instance Applicative GAction where
    pure = return
    f <*> g = do { x <- f ; y <- g ; return (x y) }
-}

instance Group g => Functor (GAction g) where
    fmap f (GX gx) = GX $ \ g -> f $ gx g

test :: TypeNum p => Z_n p -> [Char]
test (Z_n p) = show p
