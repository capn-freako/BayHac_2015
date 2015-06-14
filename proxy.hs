-- Playing w/ Chachaf's ideas (from Haskell meetup).

data Proxy a = Proxy

data Nat = Z
         | S Nat
    deriving (Show, Eq, Ord)

predNat     Z = Z
predNat (S n) = n

succNat n = S n

decodeNat Z = 0
decodeNat n = 1 + decodeNat (predNat n)

main = print $ decodeNat $ S (S (S Z))

