{-# LANGUAGE TypeFamilies, TemplateHaskell #-}
module GenInstances where

import Language.Haskell.TH.Syntax
import Control.Applicative
import Promote


(><) :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
(><) f g (x, y) = (f x, g y)


decl :: Q [Dec] -> Q Dec
decl = fmap head

-- > appT [''Either, ''Int, ''Bool]
-- [t| Either Int Bool |]
appT :: [Name] -> Type
appT = appT' . reverse where
  appT' [x] = ConT x
  appT' (x:xs) = AppT (appT' xs) (ConT x)

-- > mkInstance [''Num, ''Int] ds
-- [d| instance Num Int where
--       $(ds) |]
mkInstance :: [Name] -> Q [Dec] -> Q Dec
mkInstance = fmap . InstanceD [] . appT

-- > [d| instance Compatible Int Double where
--         $([''Promote, ''Int, ''Double] `type_eq` ''Double) |]
-- [d| instance Compatible Int Double where
--       type Promote Int Double = Double |]
type_eq :: [Name] -> Name -> Q Dec
type_eq (x:xs) r = return $ TySynInstD x (map ConT xs) $ ConT r

-- > promote_id ''Int
-- [d| instance Compatible Int Int where
--       type Promote Int Int = Int
--       convert = id |]
promote_id :: Name -> Q Dec
promote_id t = mkInstance [''Compatible, t, t] $ sequence [
                 [''Promote, t, t] `type_eq` t,
                 decl [d| promote = id |]
               ]

genInstances :: [Name] -> Q [Dec]
genInstances = mapM promote_id
