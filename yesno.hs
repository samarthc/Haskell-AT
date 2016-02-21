class YesNo a where
yesno :: a -> Bool

instance YesNo Int where
yesno 0 = False
yesno _ = True