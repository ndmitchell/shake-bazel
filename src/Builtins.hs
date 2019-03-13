
module Builtins(builtinFuncs) where

import Type
import System.FilePattern.Directory


builtinFuncs :: [(String, BuiltinFunc)]
builtinFuncs = let (*) = (,) in
    ["glob" * glob
    ]

glob :: FilePath -> [(Maybe String, Value)] -> IO Value
glob dir [(Nothing, x)] = VList . map VString <$> getDirectoryFiles dir (fromVListString x)
