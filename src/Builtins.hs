
module Builtins(builtins) where

import Type
import System.FilePattern.Directory


builtins = let (*) = (,) in
    ["glob" * glob
    ]

glob :: FilePath -> [(Maybe String, Value)] -> IO Value
glob dir [(Nothing, x)] = VList . map VString <$> getDirectoryFiles dir (fromVListString x)
