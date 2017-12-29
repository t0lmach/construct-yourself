{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction (Name, Term (..), Type (..), Context (..), Substitution (..)
  , Equation, Substitutable(subst), compose, termP)
import           Data.Map as Map (empty, fromList, unionWith, (!))
import           Data.Set as Set (fromList, empty)
import           Test.Hspec
import           Data.Set
import           Data.Maybe as MB (fromJust)
import           Text.Parsec
import           Text.Parsec.Text
import           Data.Text
import           Text.Printf (printf)

main :: IO ()
main = hspec $ do
    describe "Context monoid test" testContextMonoid
    describe "Substituion in context test" testSubstInContext
    describe "Substitution in type test" testSubstInType
    describe "Substitution composing test" testCompose

mkContext :: [(Name, Type)] -> Context
mkContext list = Context $ Map.fromList $ list

mkSubst :: [(Name, Type)] -> Substitution
mkSubst list = Substitution $ Map.fromList $ list

pretty :: (Show a, Show b) => Int -> a -> b -> String
pretty number x y = printf "#%i: %s == %s" number (show x) (show y)

checkStatement :: (Eq a, Show a) => Int -> a -> a -> SpecWith ()
checkStatement num x y = it (pretty num x y) $ x `shouldBe` y

nameA = "a"
nameB = "b"
nameC = "c";
nameD = "d"
nameE = "e"
nameF = "f"
nameG = "g"
nameH = "h"
nameI = "i"
nameJ = "j"
nameK = "k"
nameL = "l"

tpeA = TVar nameA
tpeB = TVar nameB
tpeC = TVar nameC
tpeD = TVar nameD
tpeE = TVar nameE
tpeF = TVar nameF
tpeG = TVar nameG
tpeK = TVar nameK
tpeL = TVar nameL
arrayTpe = TArr tpeA tpeB

emptyContext = Context Map.empty
contextBA = mkContext [(nameB, tpeA)]
contextBC = mkContext [(nameB, tpeC)]
contextCB = mkContext [(nameC, tpeB)]
contextCC = mkContext [(nameC, tpeC)]
contextComp1 = mkContext [(nameH, TArr tpeK tpeL), (nameI, tpeK)]
contextComp2 = mkContext [(nameH, TVar nameI)]

emptySubst = Substitution Map.empty
substAA = mkSubst [(nameA, tpeA)]
substBC = mkSubst [(nameB, tpeC)]
substCC = mkSubst [(nameC, tpeC)]
substAC = mkSubst [(nameA, tpeC)]
substHF = mkSubst [(nameH, tpeF)]
substDE = mkSubst [(nameD, tpeE)]
substEF = mkSubst [(nameE, tpeF)]
substGF = mkSubst [(nameG, tpeF)]
substComp1 = mkSubst [(nameH, TArr tpeF tpeE)]
substComp2 = mkSubst [(nameE, TArr tpeF tpeG)]

testContextMonoid :: SpecWith () 
testContextMonoid = do
    checkStatement 1 (mappend mempty mempty) emptyContext
    checkStatement 2 (mappend contextBC mempty) contextBC
    checkStatement 3 (mappend contextBC contextCB) (mkContext [(nameB, tpeC), (nameC, tpeB)])
    checkStatement 4 (mappend contextBA contextBC) contextBA
    checkStatement 5 (mappend contextComp1 contextComp2) contextComp1

testSubstitutionMonoid :: SpecWith ()
testSubstitutionMonoid = do
    checkStatement 1 (mappend mempty mempty) (Substitution Map.empty)
    checkStatement 2 (mappend substAA substAC) substAA
    checkStatement 3 (mappend substAA substBC) (mkSubst [(nameA, tpeA), (nameB, tpeC)])
    checkStatement 4 (mappend substAA (mappend substBC substAC)) substAA
    checkStatement 5 (mappend substComp1 substComp2) substAA

testSubstInContext :: SpecWith ()
testSubstInContext = do
    checkStatement 1 (subst substAA emptyContext) emptyContext
    checkStatement 2 (subst substAA contextBA) contextBA
    checkStatement 3 (subst substBC contextCB) contextCC
    checkStatement 4 (subst substBC (subst substAA contextCB)) contextCC
    checkStatement 5 (subst substAA (subst substBC contextCB)) contextCC

testSubstInType :: SpecWith ()
testSubstInType = do
    checkStatement 1 (subst substAA tpeA) tpeA
    checkStatement 2 (subst substBC tpeB) tpeC
    checkStatement 3 (subst substBC arrayTpe) (TArr tpeA tpeC)
    checkStatement 4 (subst substBC (TArr arrayTpe tpeB)) (TArr (TArr tpeA tpeC) tpeC)
    checkStatement 5 (subst substBC (TArr arrayTpe (TArr tpeC tpeB))) (TArr (TArr tpeA tpeC) (TArr tpeC tpeC))

testCompose :: SpecWith ()
testCompose = do
    checkStatement 1 (compose emptySubst substAA) substAA
    checkStatement 2 (compose substAA substBC) (mkSubst [(nameB, tpeC), (nameA, tpeA)])
    checkStatement 3 (compose substCC substAA) (mkSubst [(nameA, tpeA), (nameC, tpeC)])
    checkStatement 4 (compose substEF (compose substDE substHF)) (mkSubst [(nameH, tpeF), (nameD, tpeF), (nameE, tpeF)])
    checkStatement 5 (compose substGF (compose substComp2 substComp1)) (mkSubst [(nameG, tpeF), (nameE, TArr tpeF tpeF), (nameH, TArr tpeF (TArr tpeF tpeF))])