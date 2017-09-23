module Test.Main
( main
) where

import Control.Monad.Eff.Console (log)
import Data.ByteString
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..))
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
    log "laws"
    checkEq        (Proxy :: Proxy ByteString)
    checkMonoid    (Proxy :: Proxy ByteString)
    checkOrd       (Proxy :: Proxy ByteString)
    checkSemigroup (Proxy :: Proxy ByteString)

    log "singleton"
    quickCheck $ \b -> unpack (singleton b) === [b]

    log "pack and unpack"
    quickCheck $ \b -> unpack (pack b) === b
    quickCheck $ \b -> pack (unpack b) === b

    log "cons"
    quickCheck $ \c b -> unpack (cons c b) === [c] <> unpack b

    log "snoc"
    quickCheck $ \c b -> unpack (snoc b c) === unpack b <> [c]

    log "uncons"
    quickCheck $ \c b -> case uncons (cons c b) of
                           Just r  -> r.head == c && r.tail == b
                           Nothing -> false

    log "unsnoc"
    quickCheck $ \c b -> case unsnoc (snoc b c) of
                           Just r  -> r.init == b && r.last == c
                           Nothing -> false

    log "head"
    quickCheck $ head empty === Nothing
    quickCheck $ \c b -> head (cons c b) === Just c

    log "tail"
    quickCheck $ tail empty === Nothing
    quickCheck $ \c b -> tail (cons c b) === Just b

    log "last"
    quickCheck $ last empty === Nothing
    quickCheck $ \c b -> last (snoc b c) === Just c

    log "init"
    quickCheck $ init empty === Nothing
    quickCheck $ \c b -> init (snoc b c) === Just b

    log "length"
    quickCheck $ \b c -> length (b <> c) === length b + length c

    log "isEmpty"
    quickCheck $ isEmpty empty
    quickCheck $ \b -> isEmpty b === (length b == 0)

    log "map"
    quickCheck $ \b f -> map f b === pack (Prelude.map f (unpack b))

    log "reverse"
    quickCheck $ \b -> reverse (reverse b) === b

    log "foldl"
    quickCheck $ \b -> foldl subL 0 b === Foldable.foldl (-) 0 (runQuotient <$> unpack b)

    log "foldr"
    quickCheck $ \b -> foldr subR 0 b === Foldable.foldr (-) 0 (runQuotient <$> unpack b)

    log "fromString"
    quickCheck $ fromString "ABCD" Hex === Just (withOctets pack [0xAB, 0xCD])
    quickCheck $ fromString "LOL" Hex === Nothing

  where
  subL a b = a - runQuotient b
  subR a b = runQuotient a - b

withOctet :: ∀ a. (Octet -> a) -> Int -> a
withOctet = flip $ (#) <<< mkQuotient

withOctets :: ∀ a. (Array Octet -> a) -> Array Int -> a
withOctets f xs = f (Prelude.map (withOctet id) xs)
