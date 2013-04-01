{-# LANGUAGE MultiParamTypeClasses, TypeFamilies,
             TemplateHaskell #-}

import Data.Int
import Promote
import GenInstances


i42 :: Int
i42 = 42

f42 :: Float
f42 = 42

d42 :: Double
d42 = 42


$(promote_all [''Int8, ''Int16, ''Int32, ''Int64, ''Int, ''Float, ''Double]
              ['fromIntegral, -- Int8 to...
               'fromIntegral, -- Int16 to...
               'fromIntegral, -- Int32 to...
               'fromIntegral, -- Int64 to...
               'fromIntegral, -- Int to Float
               'realToFrac])

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
          
          print $ (3 :: Int8) `add` (40000 :: Int32)
