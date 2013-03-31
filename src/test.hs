{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts,
             TemplateHaskell #-}

import Promote
import GenInstances


-- instance Compatible Int Int where
--   type Promote Int Int = Int
--   promote = id >< id

instance Compatible Int Double where
  type Promote Int Double = Double
  promote = fromIntegral >< id


i42 :: Int
i42 = 42

d42 :: Double
d42 = 42


$(genInstances [''Int, ''Double])

main = do print $ i42 + i42  -- typechecks
          print $ d42 + d42  -- typechecks
          -- print $ i42 + d42  -- does not typecheck
          print $ i42 `add` i42  -- typechecks
          print $ d42 `add` d42  -- typechecks
          print $ i42 `add` d42  -- typechecks!
