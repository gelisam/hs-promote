{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts,
             TemplateHaskell #-}

import Promote
import GenInstances


i42 :: Int
i42 = 42

d42 :: Double
d42 = 42


$(genInstances [''Int, ''Double])
$(promote_both ''Int ''Double ['fromIntegral])

main = do print $ i42 + i42  -- typechecks
          print $ d42 + d42  -- typechecks
          -- print $ i42 + d42  -- does not typecheck
          print $ i42 `add` i42  -- typechecks
          print $ d42 `add` d42  -- typechecks
          print $ i42 `add` d42  -- typechecks!
