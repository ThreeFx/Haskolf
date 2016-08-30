module Haskolf where

import Utils

import Control.Applicative
import Control.Monad hiding (join, replicateM)
import qualified Control.Monad as M
import Data.Bits
import Data.Char
import Data.Complex
import Data.Function
import Data.List hiding (transpose)
import qualified Data.List as L
import Data.Tree hiding (flatten)
import qualified Data.Tree as T

import Numeric

import System.Environment

import Prelude hiding ((+), not, sqrt)
import qualified Prelude as P

main :: IO ()
main = do
    args <- getArgs
    print args

codepage :: String
codepage = "¡¢£¤¥¦©¬®µ½¿€ÆÇÐÑ×ØŒÞßæçðıȷñ÷øœþ !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~¶"
         ++"°¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ƁƇƊƑƓƘⱮƝƤƬƲȤɓƈɗƒɠɦƙɱɲƥʠɼʂƭʋȥẠḄḌẸḤỊḲḶṂṆỌṚṢṬỤṾẈỴẒȦḂĊḊĖḞĠḢİĿṀṄȮṖṘṠṪẆẊẎŻạḅḍẹḥịḳḷṃṇọṛṣṭụṿẉỵẓȧḃċḋėḟġḣŀṁṅȯṗṙṡṫẇẋẏż«»‘’“”"

data Constant a = Constant
                  { cname :: Char,
                    cval :: a
                  }

data UnaryFunc a b = UnaryFunc
                   { uname :: Char,
                     uarity :: Integer,
                     ueffect :: a -> b
                   }

data BinaryFunc a b c = BinaryFunc
                      { bname :: Char,
                        barity :: Integer,
                        beffect :: a -> b -> c
                      }

evalUnary :: UnaryFunc a b -> a -> b
evalUnary = ueffect

evalBinary :: BinaryFunc a b c -> a -> b -> c
evalBinary = beffect

-- SECTION: Built-in function

abs :: Num a => UnaryFunc a a
abs = UnaryFunc { uname = 'A', uarity = 1, ueffect = P.abs }

and :: IsBool b => BinaryFunc b b b
and = BinaryFunc { bname = 'a', barity = 1, beffect = (fromBool .) . on (&&) toBool }

ap :: Applicative f => BinaryFunc (f (a -> b)) (f a) (f b)
ap = BinaryFunc { bname = 'p', barity = 2, beffect = (<*>) }

bind :: Monad m => BinaryFunc (m a) (a -> m b) (m b)
bind = BinaryFunc { bname = '}', barity = 0, beffect = (>>=) }

bind' :: Monad m => BinaryFunc (a -> m b) (m a) (m b)
bind' = BinaryFunc { bname = '{', barity = 0, beffect = (=<<) }

bitAnd :: Bits b => BinaryFunc b b b
bitAnd = BinaryFunc { bname = '&', barity = 0, beffect = (.&.) }

bitNot :: Bits b => UnaryFunc b b
bitNot = UnaryFunc { uname = '~', uarity = 1, ueffect = complement }

bitOr :: Bits b => BinaryFunc b b b
bitOr = BinaryFunc { bname = '|', barity = 0, beffect = (.|.) }

bitXor :: Bits a => BinaryFunc a a a
bitXor = BinaryFunc { bname = '^', barity = 1, beffect = xor }

ceil :: (RealFrac f, Integral i) => UnaryFunc f i
ceil = UnaryFunc { uname = 'C', uarity = 1, ueffect = ceiling }

eq :: Eq a => BinaryFunc a a Bool
eq = BinaryFunc { bname = '=', barity = 1, beffect = (==) }

factorial :: (Enum a, Num a) => UnaryFunc a a
factorial = UnaryFunc { uname = '!', uarity = 2, ueffect = \n -> product [1..n] }

flatten :: UnaryFunc (Tree a) [a]
flatten = UnaryFunc { uname = 'K', uarity = 1, ueffect = T.flatten }

flip :: UnaryFunc (a -> b -> c) (b -> a -> c)
flip = UnaryFunc { uname = 'X', uarity = 1, ueffect = P.flip }

floatingDivision :: Floating a => BinaryFunc a a a
floatingDivision = BinaryFunc { bname = '÷', barity = 2, beffect = (/) }

floor :: (RealFrac f, Integral i) => UnaryFunc f i
floor = UnaryFunc { uname = 'F', uarity = 1, ueffect = P.floor }

fmap :: Functor f => BinaryFunc (a -> b) (f a) (f b)
fmap = BinaryFunc { bname = 'm', barity = 2, beffect = P.fmap }

foldl :: Foldable t => BinaryFunc (a -> a -> a) (t a) a
foldl = BinaryFunc { bname = '\\', barity = 0, beffect = foldl1 }

foldr :: Foldable t => BinaryFunc (a -> a -> a) (t a) a
foldr = BinaryFunc { bname = '/', barity = 0, beffect = foldr1 }

greaterthan :: Ord a => BinaryFunc a a Bool
greaterthan = BinaryFunc { bname = '>', barity = 1, beffect = (>) }

head :: UnaryFunc [a] a
head = UnaryFunc { uname = 'H', uarity = 1, ueffect = P.head }

id :: UnaryFunc a a
id = UnaryFunc { uname = 'I', uarity = 1, ueffect = P.id }

join :: Monad m => UnaryFunc (m (m a)) (m a)
join = UnaryFunc { uname = 'J', uarity = 1, ueffect = M.join }

length :: Foldable t => UnaryFunc (t a) Integer
length = UnaryFunc { uname = 'L', uarity = 1, ueffect = fromIntegral . P.length }

logBase :: Floating f => BinaryFunc f f f
logBase = BinaryFunc { bname = 'l', barity = 2, beffect = P.logBase }

lessthan :: Ord a => BinaryFunc a a Bool
lessthan = BinaryFunc { bname = '<', barity = 1, beffect = (<) }

modulus :: Integral a => BinaryFunc a a a
modulus = BinaryFunc { bname = '%', barity = 2, beffect = mod }

multiplication :: Num a => BinaryFunc a a a
multiplication = BinaryFunc { bname = '×', barity = 2, beffect = (*) }

newline :: Constant Char
newline = Constant { cname = '¶', cval = '\n' }

not :: (IsBool b, IsBool c) => UnaryFunc b c
not = UnaryFunc '¬' 0 (fromBool . P.not . toBool)

or :: IsBool b => BinaryFunc b b b
or = BinaryFunc { bname = 'o', barity = 1, beffect = (fromBool .) . on (||) toBool }

plus :: Semiring a => BinaryFunc a a a
plus = BinaryFunc { bname = '+', barity = 1, beffect = (<>) }

pure :: Applicative f => UnaryFunc a (f a)
pure = UnaryFunc { uname = 'W', uarity = 1, ueffect = P.pure }

rangeex :: Enum n => UnaryFunc n [n]
rangeex = UnaryFunc { uname = 'Q', uarity = 1, ueffect = init . enumFromTo (toEnum 0) }

rangeinc :: Enum n => UnaryFunc n [n]
rangeinc = UnaryFunc { uname = 'R', uarity = 1, ueffect = enumFromTo (toEnum 1) }

replicate :: Integral i => BinaryFunc i a [a]
replicate = BinaryFunc { bname = 'r', barity = 1, beffect = P.replicate . fromIntegral}

replicateM :: (Integral i, Monad m) => BinaryFunc i (m a) (m [a])
replicateM = BinaryFunc { bname = 'z', barity = 1, beffect = M.replicateM . fromIntegral}

sqrt :: Floating a => UnaryFunc a a
sqrt = UnaryFunc { uname = '½', uarity = 2, ueffect = P.sqrt }

square :: Num a => UnaryFunc a a
square = UnaryFunc { uname = '²', uarity = 1, ueffect = M.join (*) }

subtraction :: Num a => BinaryFunc a a a
subtraction = BinaryFunc { bname = '-', barity = 1, beffect = (-) }

sum :: (Num n, Foldable t) => UnaryFunc (t n) n
sum = UnaryFunc { uname = 'S', uarity = 1, ueffect = P.sum }

toBase :: (Show i, Integral i) => BinaryFunc i i String
toBase = BinaryFunc { bname = 'b', barity = 0, beffect = \number base -> showIntAtBase base charfunc number $ "" }
  where
    charfunc = (!!) (['0'..'9']++['a'..'z'])

transpose :: UnaryFunc [[a]] [[a]]
transpose = UnaryFunc { uname = 'T', uarity = 1, ueffect = L.transpose }

vectorize :: UnaryFunc (a -> a -> b) ([a] -> [a] -> [b])
vectorize = UnaryFunc { uname = 'Z', uarity = 2, ueffect = zipWith }
