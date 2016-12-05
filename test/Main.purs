module Test.Main
( main
) where

import Data.ByteString
import Prelude
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.Monoid (checkMonoid)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Test.QuickCheck.Laws.Data.Semigroup (checkSemigroup)
import Type.Proxy (Proxy(..))

main = do
    checkEq (Proxy :: Proxy ByteString)
    checkMonoid (Proxy :: Proxy ByteString)
    checkOrd (Proxy :: Proxy ByteString)
    checkSemigroup (Proxy :: Proxy ByteString)
