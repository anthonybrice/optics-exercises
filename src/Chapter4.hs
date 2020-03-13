{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter4 where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Lib

-- just to be clear, the second version is only to illustrate that we can
-- declare the types easily. Both versions have all the type safety you'd get in
-- OOP code. Re the first version, what could it mean for a function to expect a
-- Dog instead of a Cat when there is no Dog or Cat type?

data Animal = Animal { emit :: String }

data Cat = Cat { emit' :: String }
type Dog = Animal

cat :: Cat
cat = Cat { emit' = "meow" }

dog :: Dog
dog = Animal { emit = "woof" }

animalFromCat :: Cat -> Animal
animalFromCat x = Animal { emit = emit' x }

-- animalFromDog :: Dog -> Animal
-- animalFromDog x = Animal { emit = x }

data Vorpal x = Vorpal x

vorpalChanger :: Lens (Vorpal a) (Vorpal b) a b
vorpalChanger = undefined

data Preferences a b = Preferences
  { _best :: a
  , _worst :: b
  } deriving (Show)

best :: Lens (Preferences a b) (Preferences c b) a c
best = lens getter setter where
  getter = _best
  setter s x = s { _best = x }

data Result e = Result
  { _lineNumber :: Int
  , _result :: Either e String
  } deriving (Show)

result :: Lens (Result e) (Result f) (Either e String) (Either f String)
result = undefined

-- 4: yes

data Predicate a = Predicate (a -> Bool)

pred :: Lens (Predicate a) (Predicate b) (a -> Bool) (b -> Bool)
pred = lens getter setter where
  getter (Predicate f) = f
  setter _ x = Predicate x

waldo :: String
waldo = view (_2 . _1 . _2) ("Ginerva", (("Galileo", "Waldo"), "Malfoy"))

-- mysteryDomino :: Lens' Eight Two

-- bar :: Lens Platypus BabySloth Armadillo Hedgehog

-- snajubjumwoxk . boowockugwuo . gruggazinkoom . zinkattumblezz
--   . spuzorktrowmble . gazorlgesnatchka . banderyakoobog
