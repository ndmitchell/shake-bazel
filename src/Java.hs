{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Java(builtins) where

import Type


builtins = let (*) = (,) in
    ["java_binary" * java_binary
    ]

java_binary :: FilePath -> [(Maybe String, Value)] -> IO Value
java_binary = undefined
