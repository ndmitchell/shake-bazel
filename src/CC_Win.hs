{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module CC_Win(builtins) where

import Type
import System.Directory
import Control.Monad
import Data.List
import System.FilePath
import Development.Shake.Command


builtins = let (*) = (,) in
    ["cc_binary" * cc Binary
    ,"cc_library" * cc Library
    ]

data Mode = Binary | Library

cc :: Mode -> FilePath -> [(Maybe String, Value)] -> IO Value
cc mode dir args = return $ VRule $ Rule (mkRuleName dir name) (map (mkRuleDep dir) deps) (srcs ++ hdrs) action
    where
        name = fromVString $ lookup_ (Just "name") args
        srcs = fromVListString $ lookup_ (Just "srcs") args
        hdrs = maybe [] fromVListString $ lookup (Just "hdrs") args
        deps = maybe [] fromVListString $ lookup (Just "deps") args

        action = do
            objs <- forM srcs $ \src -> do
                let obj = "sazel-out/x64_windows-fastbuild/bin/" ++ dir ++ "/_objs/" ++ name ++ "/" ++ dropExtension src ++ ".obj"
                createDirectoryIfMissing True $ takeDirectory obj
                putStrLn $ "Compiling: " ++ src
                cmd_ (AddEnv "PATH" cc_path) (AddEnv "INCLUDE" cc_include)
                    "cl /nologo /DCOMPILER_MSVC /DNOMINMAX /D_WIN32_WINNT=0x0601 /D_CRT_SECURE_NO_DEPRECATE /D_CRT_SECURE_NO_WARNINGS /bigobj /Zm500 /EHsc"
                    "/wd4351 /wd4291 /wd4250 /wd4996 /I."
                    "/Isazel-out/x64_windows-fastbuild/genfiles /Isazel-out/x64_windows-fastbuild/bin /Iexternal/bazel_tools /Isazel-out/x64_windows-fastbuild/genfiles/external/bazel_tools /Isazel-out/x64_windows-fastbuild/bin/external/bazel_tools /MD /Od /Z7 /wd4117"
                    "-D__DATE__=redacted -D__TIMESTAMP__=redacted -D__TIME__=redacted"
                    ["/Fo" ++ obj,"/c",dir ++ "/" ++ src]
                return obj
            putStrLn $ "Linking: " ++ name
            case mode of
                Binary -> cmd_ (AddEnv "PATH" cc_path) (AddEnv "LIB" cc_lib)
                    ("link.exe /nologo /OUT:sazel-out/x64_windows-fastbuild/bin/" ++ dir ++ "/" ++ name ++ ".exe /SUBSYSTEM:CONSOLE /MACHINE:X64 /DEBUG:FASTLINK /INCREMENTAL:NO /DEFAULTLIB:msvcrt.lib")
                    (map (toLib . mkRuleDep dir) deps)
                    objs
                Library -> cmd_ (AddEnv "PATH" cc_path) (AddEnv "LIB" cc_lib)
                    "lib.exe /nologo /ignore:4221"
                    ("/OUT:sazel-out/x64_windows-fastbuild/bin/" ++ dir ++ "/" ++ name ++ ".lib")
                    objs

toLib :: RuleName -> String
toLib (RuleName dir name) = "sazel-out/x64_windows-fastbuild/bin/" ++ dir ++ "/" ++ name ++ ".lib"


cc_lib = intercalate [searchPathSeparator]
    ["C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC\\14.13.26128\\lib\\x64"
    ,"C:\\Program Files (x86)\\Windows Kits\\NETFXSDK\\4.6.1\\lib\\um\\x64"
    ,"C:\\Program Files (x86)\\Windows Kits\\10\\lib\\10.0.16299.0\\ucrt\\x64"
    ,"C:\\Program Files (x86)\\Windows Kits\\10\\lib\\10.0.16299.0\\um\\x64"
    ]

cc_include = intercalate [searchPathSeparator]
    ["C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC\\14.13.26128\\include"
    ,"C:\\Program Files (x86)\\Windows Kits\\NETFXSDK\\4.6.1\\include\\um"
    ,"C:\\Program Files (x86)\\Windows Kits\\10\\include\\10.0.16299.0\\ucrt"
    ,"C:\\Program Files (x86)\\Windows Kits\\10\\include\\10.0.16299.0\\shared"
    ,"C:\\Program Files (x86)\\Windows Kits\\10\\include\\10.0.16299.0\\um"
    ,"C:\\Program Files (x86)\\Windows Kits\\10\\include\\10.0.16299.0\\winrt"
    ,"C:\\Program Files (x86)\\Windows Kits\\10\\include\\10.0.16299.0\\cppwinrt"
    ]

cc_path = intercalate [searchPathSeparator]
    ["C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC\\14.13.26128\\bin\\HostX64\\x64"
    ,"C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\Common7\\IDE\\VC\\VCPackages"
    ,"C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\Common7\\IDE\\CommonExtensions\\Microsoft\\TestWindow"
    ,"C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\Common7\\IDE\\CommonExtensions\\Microsoft\\TeamFoundation\\Team Explorer"
    ,"C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\MSBuild\\15.0\\bin\\Roslyn"
    ,"C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\Team Tools\\Performance Tools\\x64"
    ,"C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\Team Tools\\Performance Tools"
    ,"C:\\Program Files (x86)\\Microsoft Visual Studio\\Shared\\Common\\VSPerfCollectionTools\\\\x64"
    ,"C:\\Program Files (x86)\\Microsoft Visual Studio\\Shared\\Common\\VSPerfCollectionTools\\"
    ,"C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v10.0A\\bin\\NETFX 4.6.1 Tools\\x64\\"
    ,"C:\\Program Files (x86)\\Microsoft SDKs\\F#\\10.1\\Framework\\v4.0\\"
    ,"C:\\Program Files (x86)\\Windows Kits\\10\\bin\\10.0.16299.0\\x64"
    ,"C:\\Program Files (x86)\\Windows Kits\\10\\bin\\x64"
    ,"C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\\\MSBuild\\15.0\\bin"
    ,"C:\\Windows\\Microsoft.NET\\Framework64\\v4.0.30319"
    ,"C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\Common7\\IDE\\"
    ,"C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\Common7\\Tools\\"
    ,"C:\\WINDOWS\\system32"
    ]
