{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Promote where


class Compatible a b where
  type Promote a b :: *
  promote :: (a, b) -> (Promote a b, Promote a b)

add :: (Compatible a b, Num (Promote a b)) => a -> b -> Promote a b
add x y = let (x', y') = promote (x, y)
           in x' + y'
