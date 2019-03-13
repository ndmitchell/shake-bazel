{-# LANGUAGE ViewPatterns, TypeFamilies, DeriveAnyClass, DeriveGeneric #-}

module Type(
    Rule(..), RuleName(..),
    Value(..), fromVListString,
    lookup_,
    mkRuleName, mkRuleDep,
    BuiltinFunc, BuiltinRule,
    (//:), (+/+),
    Build(..),
    needRule
    ) where

import Development.Shake
import Development.Shake.Classes
import Data.Maybe
import Data.Functor
import GHC.Generics


data RuleName = RuleName String String
    deriving (Eq,Ord,Hashable,Generic,Binary,NFData)

instance Show RuleName where
    show (RuleName a b) = "//" ++ a ++ ":" ++ b

(//:) = RuleName
(+/+) x y = x ++ "/" ++ y

data Rule = Rule
    {ruleType :: String
    ,ruleName :: RuleName
    ,ruleArgs :: [(String, Value)]
    } deriving (Eq,Ord,Hashable,Generic,Binary,NFData,Show)

mkRuleName :: FilePath -> String -> RuleName
mkRuleName x y = RuleName x y

mkRuleDep :: FilePath -> String -> RuleName
mkRuleDep x (':':y) = RuleName x y
mkRuleDep x ('/':'/':(break (== ':') -> (a,_:b))) = RuleName a b

data Value
    = VUnit
    | VTrue
    | VFalse
    | VString {fromVString :: String}
    | VInt Int
    | VList {fromVList :: [Value]}
    | VRule Rule
      deriving (Eq,Ord,Hashable,Generic,Binary,NFData,Show)

fromVListString = map fromVString . fromVList

lookup_ x ys = fromJust $ lookup x ys


type BuiltinFunc = FilePath -> [(Maybe String, Value)] -> IO Value

type BuiltinRule = Rule -> Action ()


needRule :: [RuleName] -> Action ()
needRule = void . parallel . map askOracle

newtype Build = Build FilePath
    deriving (Eq,Ord,Hashable,Generic,Binary,NFData,Show)

type instance RuleResult RuleName = ()
type instance RuleResult Build = [Rule]
