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

-- > composeE ['f, 'g, 'h]
-- [| f . g . h |]
composeE :: [Name] -> Q Exp
composeE = return . foldr compose (VarE 'id) . map VarE where
  compose x y = InfixE (Just x) (VarE '(.)) (Just y)

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

-- > promote_right ''Int ''Double ['f, 'g]
-- [d| instance Compatible Int Double where
--       type Promote Int Double = Double
--       convert = (f . g) >< id |]
promote_right :: Name -> Name -> [Name] -> Q Dec
promote_right t t' [] = promote_id t
promote_right t t' fs = mkInstance [''Compatible, t, t'] $ sequence [
                          [''Promote, t, t'] `type_eq` t',
                          decl [d| promote = $(composeE fs) >< id |]
                        ]

-- > promote_left ''Double ''Int ['f, 'g]
-- [d| instance Compatible Double Int where
--       type Promote Double Int = Double
--       convert = id >< (f . g) |]
promote_left :: Name -> Name -> [Name] -> Q Dec
promote_left t t' [] = promote_id t
promote_left t t' fs = mkInstance [''Compatible, t, t'] $ sequence [
                         [''Promote, t, t'] `type_eq` t,
                         decl [d| promote = id >< $(composeE fs) |]
                       ]

-- > promote_both ''Int ''Double ['int_to_foo, 'foo_to_double]
-- [d| instance Compatible Int Double where
--       type Promote Int Double = Double
--       convert = (foo_to_double . int_to_foo) >< id
--     instance Compatible Double Int where
--       type Promote Double Int = Double
--       convert = id >< (foo_to_double . int_to_foo) |]
promote_both :: Name -> Name -> [Name] -> Q [Dec]
promote_both t t' [] = sequence [
                         promote_id t
                       ]
promote_both t t' fs = sequence [
                         promote_right t t' (reverse fs),
                         promote_left t' t fs
                       ]

genInstances :: [Name] -> Q [Dec]
genInstances = mapM promote_id
