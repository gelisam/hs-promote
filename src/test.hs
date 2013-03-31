{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}


(><) :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
(><) f g (x, y) = (f x, g y)


class Compatible a b where
  type Promote a b :: *
  promote :: (a, b) -> (Promote a b, Promote a b)

instance Compatible Int Double where
  type Promote Int Double = Double
  promote = fromIntegral >< id

instance Compatible Int Int where
  type Promote Int Int = Int
  promote = id >< id

instance Compatible Double Double where
  type Promote Double Double = Double
  promote = id >< id

add :: (Compatible a b, Num (Promote a b)) => a -> b -> Promote a b
add x y = let (x', y') = promote (x, y)
           in x' + y'


i42 :: Int
i42 = 42

d42 :: Double
d42 = 42

main = do print $ i42 + i42  -- typechecks
          print $ d42 + d42  -- typechecks
          -- print $ i42 + d42  -- does not typecheck
          print $ i42 `add` i42  -- typechecks
          print $ d42 `add` d42  -- typechecks
          print $ i42 `add` d42  -- typechecks!
