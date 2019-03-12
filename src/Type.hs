{-# OPTIONS_GHC -Wno-orphans #-} -- Show IO
{-# LANGUAGE ViewPatterns #-}

module Type(
    Rule(..), RuleName(..),
    Value(..), fromVListString,
    lookup_,
    mkRuleName, mkRuleDep
    ) where

import Text.Show.Functions()
import Data.Maybe


data RuleName = RuleName String String
    deriving (Eq,Ord)

instance Show RuleName where
    show (RuleName a b) = "//" ++ a ++ ":" ++ b


data Rule = Rule
    {ruleName :: RuleName
    ,ruleDepends :: [RuleName]
    ,fileDepends :: [FilePath]
    ,ruleAction :: (RuleName -> Rule) -> IO ()
    } deriving Show

mkRuleName :: FilePath -> String -> RuleName
mkRuleName x y = RuleName x y

mkRuleDep :: FilePath -> String -> RuleName
mkRuleDep x (':':y) = RuleName x y
mkRuleDep x ('/':'/':(break (== ':') -> (a,_:b))) = RuleName a b

instance Show (IO a) where
    show _ = "<IO>"

data Value
    = VUnit
    | VTrue
    | VFalse
    | VString {fromVString :: String}
    | VInt Int
    | VList {fromVList :: [Value]}
    | VFun {fromVFun :: [(Maybe String, Value)] -> IO Value}
    | VRule Rule
      deriving Show

fromVListString = map fromVString . fromVList

lookup_ x ys = fromJust $ lookup x ys
