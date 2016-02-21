class Tofu t where
    tofu :: j a -> t a j

data Frank a b = Frank (b a) deriving (Show, Eq)

instance Tofu Frank where
    tofu = Frank