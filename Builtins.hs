module Builtins
( builtins
) where

import Types

import Control.Monad
import Control.Monad.Except
import Data.Complex
import Data.Function

class Convertable a where
    convert :: S_Object -> ExceptT S_Error IO a
    unconvert :: a -> S_Object

newtype B_Number = BoxN { unbox :: I_Number }

instance Num B_Number where
    (+) = curry $ BoxN . uncurry ((+) `on` unbox)
    (*) = curry $ BoxN . uncurry ((*) `on` unbox)
    (-) = curry $ BoxN . uncurry ((-) `on` unbox)
    negate = BoxN . negate . unbox
    fromInteger = BoxN . fromInteger
    signum = BoxN . signum . unbox
    abs = BoxN . abs . unbox

instance Convertable B_Number where
    convert (C_Number x) = return . BoxN $ x
    convert _ = throwError $ Default "Expected a number"
    unconvert = C_Number . unbox

folds :: (Convertable a) => (a -> a -> a) -> a -> S_Object -> ExceptT S_Error IO a
folds f a (C_List C_EmptyList) = return a
folds f a (C_List (C_Cons x xs)) = convert x >>= return . f a >>= (\a' -> seq a' $ folds f a' xs)

builtins :: [(String, BuiltinFunction)]
builtins = [
    ("+", BuiltinFunction $ fmap unconvert . (folds (+) (BoxN $ 0 :+ 0)))
    ]
