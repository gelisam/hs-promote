{-# LANGUAGE MultiParamTypeClasses, TypeFamilies,
             TemplateHaskell #-}

import Promote
import GenInstances


i42 :: Int
i42 = 42

f42 :: Float
f42 = 42

d42 :: Double
d42 = 42


$(genInstances [''Int, ''Float, ''Double])
$(promote_both ''Int ''Float [[|fromIntegral|]])
$(promote_both ''Float ''Double [[|realToFrac|]])
$(promote_both ''Int ''Double [[|fromIntegral :: Int -> Float|]
                              ,[|realToFrac :: Float -> Double|]])

main = do print $ i42 + i42  -- typechecks
          print $ f42 + f42  -- typechecks
          print $ d42 + d42  -- typechecks
          -- print $ i42 + d42  -- does not typecheck
          
          print $ i42 `add` i42
          print $ i42 `add` f42
          print $ i42 `add` d42
          
          print $ f42 `add` i42
          print $ f42 `add` f42
          print $ f42 `add` d42
          
          print $ d42 `add` i42
          print $ d42 `add` f42
          print $ d42 `add` d42
