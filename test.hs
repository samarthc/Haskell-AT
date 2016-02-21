instance (Enum a, Bounded a, Eq b) => Eq (a -> b) where
    f == g = let frange = map f [minBound .. maxBound]; grange = map g [minBound :: a .. maxBound :: a] in frange == grange