{-# LANGUAGE RecordWildCards, TupleSections #-}

module Main(main) where

import Type
import qualified Builtins
import qualified CC_Win

import Language.Python.Common
import Language.Python.Version3
import Development.Shake
import System.Directory.Extra
import System.FilePath
import Control.Monad.Extra
import Data.List


main = withCurrentDirectory "examples/cpp-tutorial/stage3" $ do
    shakeArgs shakeOptions $ do
        addOracle $ \(Build dir) -> do
            let file = dir </> "BUILD"
            src <- readFile' file
            Right (tree, _) <- return $ parseModule src file
            liftIO $ runModule dir tree

        addOracle $ \o@(RuleName dir x) -> do
            rules <- askOracle $ Build dir
            case filter (\x -> ruleName x == o) rules of
                [] -> error $ "Rule not found, " ++ show o
                _:_:_ -> error "Multiple rules found"
                [r@Rule{..}] -> lookup_ ruleType builtinRules r

        action $ do
            let ignore x = "bazel-" `isPrefixOf` x || "sazel-" `isPrefixOf` x || "." `isPrefixOf` x
            builds <- liftIO $ filter (\x -> takeFileName x == "BUILD") . map (drop 2) <$> listFilesInside (return . not . ignore . takeFileName . drop 2) "."
            forP builds $ \x -> do
                rules <- askOracle $ Build $ takeDirectory x
                forP rules $ askOracle . ruleName


builtinFuncs = Builtins.builtinFuncs
builtinRules = CC_Win.builtinRules


runModule :: FilePath -> Module SrcSpan -> IO [Rule]
runModule dir (Module xs) = concatMapM (runStmt dir) xs

runStmt :: FilePath -> Statement SrcSpan -> IO [Rule]
runStmt dir (StmtExpr x _) = do
    VRule x <- runExpr dir x
    return [x]

runExpr :: FilePath -> Expr SrcSpan -> IO Value
runExpr dir Call{..}
    | Var{..} <- call_fun
    , Just x <- lookup (ident_string var_ident) builtinRules
    = do
        args <- runArgs dir call_args
        return $ VRule $ Rule
            (ident_string var_ident)
            (dir //: fromVString (lookup_ (Just "name") args))
            [(x, y) | (Just x, y) <- args]
runExpr dir Call{..}
    | Var{..} <- call_fun
    , Just x <- lookup (ident_string var_ident) builtinFuncs
    = error "here"

{-
 = do
    runExpr dir call_fun
    xs <- forM call_args $ \x -> do
        let name = case x of
                ArgKeyword{..} -> Just $ ident_string arg_keyword
                _ -> Nothing
        (name,) <$> runExpr dir (arg_expr x)
    f xs
runExpr dir Var{..} | Just x <- lookup (ident_string var_ident) builtins = return $ VFun $ x dir
-}

runExpr dir Strings{..} = return $ VString $ concatMap read strings_strings
runExpr dir List{..} = do
    xs <- mapM (runExpr dir) list_exprs
    return $ VList xs
runExpr dir x = error $ show x


runArgs :: FilePath -> [Argument SrcSpan] -> IO [(Maybe String, Value)]
runArgs dir args = forM args $ \x -> do
    let name = case x of
            ArgKeyword{..} -> Just $ ident_string arg_keyword
            _ -> Nothing
    (name,) <$> runExpr dir (arg_expr x)
