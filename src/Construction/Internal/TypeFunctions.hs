{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Construction.Internal.TypeFunctions where

import qualified Data.Map                        as M ((!), empty, lookup, unionWith)
import           Data.Text                       (pack)
import           Data.Map                        (member, fromList)
import           Data.Set                        (Set (..), elemAt, delete, singleton, toList)
import           Construction.Internal.Types
import           Construction.Internal.Functions hiding (Context, substitute)

-- Split a set of elements to the first element and rest set
split :: Ord a => Set a -> (a, Set a)
split set = let x = elemAt 0 set
            in  (x, delete x set)

-- Take variable type from context or return Nothing
(!) :: Context -> Name -> Maybe Type
ctx ! x | member x (getCtx ctx) = Just $ getCtx ctx M.! x
        | otherwise             = Nothing

-- Something we can perform substitution with
class Substitutable a where
  subst :: Substitution -> a -> a

-- Substitution in context
-- [a:=t]empty       => empty
--   [a:=t]{x:t1 ... } => {x:([a:=t]t1) ... }
instance Substitutable Context where
  subst a b
    | value == M.empty = Context (M.empty)
    | otherwise = Context (fmap (\x -> (subst a x)) value)
    where value = getCtx b

-- Substitution in type:
--   [a:=t] a     => t
--   [a:=t] b     => b
--   [a:=t](r->p) => ([a:=t]r)->([a:=t]p)
instance Substitutable Type where
  subst a b = 
    case b of
      TVar{..} -> case (M.lookup tvar (getSubs a)) of
                      Just m -> m
                      Nothing -> TVar tvar
      TArr{..} -> TArr (subst a from) (subst a to)  

-- Compose two substitutions
-- S@[a1 := t1, ...] . [b1 := s1 ...] = [b1 := S(s1) ... a1 := t1 ...]
compose :: Substitution -> Substitution -> Substitution
compose bc ab = Substitution (M.unionWith (\x y -> x) (fmap (\x -> subst bc x) (getSubs ab)) (getSubs bc))
