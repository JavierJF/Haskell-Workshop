module Monoid where

import Data.Monoid

f :: Endo Int
f = mconcat $ map Endo [(+1), (*2), negate]
