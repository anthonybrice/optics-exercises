{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter3 where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Lib

main :: IO ()
main = someFunc

data Ship = Ship
  { _name :: String
  , _numCrew :: Int
  } deriving (Show)
makeLenses ''Ship

getNumCrew :: Ship -> Int
getNumCrew = _numCrew

setNumCrew :: Ship -> Int -> Ship
setNumCrew ship newNumCrew = ship { _numCrew = newNumCrew }

data Inventory = Inventory
  { _wand :: String
  , _book :: String
  , _potions :: [String]
  }
makeLenses ''Inventory

second :: Lens' (a, b, c) b
second = _2

inMaybe :: Lens' (Maybe a) a
inMaybe = error "MethodNotImplementedException"

left :: Lens' (Either a b) a
left = error "MethodNotImplementedException"

listThird :: Lens' [a] a
listThird = error "MethodNotImplementedException"

conditional :: Lens' (Bool, a, a) a
conditional = lens getter setter where
  getter (True, x, _) = x
  getter (False, _, x) = x
  setter (True, _, x) y = (True, y, x)
  setter (False, x, _) y = (False, x, y)

data Err =
  ReallyBadError { _msg :: String }
  | ExitCode { _code :: Int }
  deriving (Show)

-- | Passes get-set and set-set. Fails set-get.
msg :: Lens' Err String
msg = lens getMsg setMsg where
  getMsg (ReallyBadError message) = message
  -- Hrmm, I guess we just return ""?
  getMsg (ExitCode _) = ""
  -- Nowhere to set it, I guess we do nothing?
  setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
  setMsg (ExitCode n) _ = ExitCode n

-- | Passes set-get and set-set. Fails get-set.
msg' :: Lens' Err String
msg' = lens getMsg setMsg where
  getMsg (ReallyBadError x) = x
  getMsg (ExitCode _) = ""
  setMsg (ReallyBadError _) x = ReallyBadError x
  setMsg (ExitCode _) x = ReallyBadError x

-- a lens that breaks get-set and set-set
headLens :: Lens' [a] a
headLens = lens getter setter where
  getter xs = head xs
  setter xs x = x:xs

lastLens :: Lens' [a] a
lastLens = lens getter setter where
  getter xs = last xs
  setter xs x = xs ++ [x]

-- | Fails get-set, set-get, and set-set
badLens :: Lens' [String] String
badLens = lens getter setter where
  getter _ = ""
  setter x y = y:x

data Builder = Builder
  { _context :: [String]
  , _build :: [String] -> String
  } deriving Show

instance Show ([String] -> String) where
  show _ = "function"

context :: Lens' Builder String
context = lens getter setter where
  getter x = concat $ reverse $ _context x
  setter x y =
    if y `elem` _context x
    then x { _context = y : _context x }
    else x

data User = User
  { _firstName :: String
  , _lastName :: String
  --, _username :: String
  , _email :: String
  } deriving (Show)
makeLenses ''User

username :: Lens' User String
username = email

fullName :: Lens' User String
fullName = lens getter setter where
  getter u = view firstName u <> " " <> view lastName u
  setter u x = u { _firstName = f, _lastName = l } where [f, l] = words x

data ProducePrices = ProducePrices
  { _limePrice :: Float
  , _lemonPrice :: Float
  } deriving Show

limePrice :: Lens' ProducePrices Float
limePrice = lens getter setter where
  getter = _limePrice
  setter p x = ProducePrices limePrice' lemonPrice' where
    limePrice' = max x 0
    lemonPrice'
      | view lemonPrice p > limePrice' + 0.5 = limePrice' + 0.5
      | view lemonPrice p < limePrice' - 0.5 = limePrice' - 0.5
      | otherwise = view lemonPrice p

lemonPrice :: Lens' ProducePrices Float
lemonPrice = lens getter setter where
  getter = _lemonPrice
  setter p x = p { _lemonPrice = max x 0 }
