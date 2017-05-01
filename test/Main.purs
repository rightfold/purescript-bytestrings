module Test.Main
( main
) where

import Data.ByteString
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Prelude hiding (map)
import Prelude as Prelude
import Test.QuickCheck ((===), quickCheck)
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.Monoid (checkMonoid)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Test.QuickCheck.Laws.Data.Semigroup (checkSemigroup)
import Type.Proxy (Proxy(..))
import Type.Quotient (mkQuotient, runQuotient)

main = do
    -- laws
    checkEq        (Proxy :: Proxy ByteString)
    checkMonoid    (Proxy :: Proxy ByteString)
    checkOrd       (Proxy :: Proxy ByteString)
    checkSemigroup (Proxy :: Proxy ByteString)

    -- singleton
    quickCheck $ withOctet \b -> unpack (singleton b) === [b]

    -- pack and unpack
    quickCheck $ withOctets \b -> unpack (pack b) === b
    quickCheck $ \b -> pack (unpack b) === b

    -- cons
    quickCheck $ withOctet \c b -> unpack (cons c b) === [c] <> unpack b

    -- snoc
    quickCheck $ withOctet \c b -> unpack (snoc b c) === unpack b <> [c]

    -- uncons
    quickCheck $ withOctet \c b -> case uncons (cons c b) of
                                       Just r  -> r.head == c && r.tail == b
                                       Nothing -> false

    -- unsnoc
    quickCheck $ withOctet \c b -> case unsnoc (snoc b c) of
                                       Just r  -> r.init == b && r.last == c
                                       Nothing -> false

    -- head
    quickCheck $ head empty === Nothing
    quickCheck $ withOctet \c b -> head (cons c b) === Just c

    -- tail
    quickCheck $ tail empty === Nothing
    quickCheck $ withOctet \c b -> tail (cons c b) === Just b

    -- last
    quickCheck $ last empty === Nothing
    quickCheck $ withOctet \c b -> last (snoc b c) === Just c

    -- init
    quickCheck $ init empty === Nothing
    quickCheck $ withOctet \c b -> init (snoc b c) === Just b

    -- length
    quickCheck $ \b c -> length (b <> c) === length b + length c

    -- isEmpty
    quickCheck $ isEmpty empty
    quickCheck $ \b -> isEmpty b === (length b == 0)

    -- map
    quickCheck $ \b f -> map f b === pack (Prelude.map f (unpack b))

    -- reverse
    quickCheck $ \b -> reverse (reverse b) === b

    -- foldl
    quickCheck $ \b -> foldl subL 0 b === Foldable.foldl (-) 0 (runQuotient <$> unpack b)

    -- foldr
    quickCheck $ \b -> foldr subR 0 b === Foldable.foldr (-) 0 (runQuotient <$> unpack b)

  where
  subL a b = a - runQuotient b
  subR a b = runQuotient a - b

withOctet :: ∀ a. (Octet -> a) -> Int -> a
withOctet f x = f (mkQuotient $ abs x)

withOctets :: ∀ a. (Array Octet -> a) -> Array Int -> a
withOctets f xs = f (Prelude.map (withOctet id) xs)
