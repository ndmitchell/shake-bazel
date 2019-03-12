{-# LANGUAGE RecordWildCards, TupleSections #-}

module Main(main) where

import Type
import qualified Builtins
import qualified CC_Win
import qualified Java

import Language.Python.Common
import Language.Python.Version3
import System.Directory.Extra
import System.FilePath
import Control.Monad.Extra
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Algebra.Graph.AdjacencyMap.Algorithm
import Algebra.Graph.AdjacencyMap.Internal


main = withCurrentDirectory "examples/cpp-tutorial/stage3" $ do
    let ignore x = "bazel-" `isPrefixOf` x || "sazel-" `isPrefixOf` x || "." `isPrefixOf` x
    builds <- filter (\x -> takeFileName x == "BUILD") . map (drop 2) <$> listFilesInside (return . not . ignore . takeFileName . drop 2) "."
    rules <- fmap concat $ forM builds $ \file -> do
        src <- readFile file
        Right (tree, _) <- return $ parseModule src file
        runModule (takeDirectory file) tree
    let mp = Map.fromList [(ruleName x, x) | x <- rules]
    Just order <- return $ topSort $ AM $ Map.map (Set.fromList . ruleDepends) mp
    forM_ (reverse order) $ \x -> ruleAction (mp Map.! x) (mp Map.!)


builtins = CC_Win.builtins ++ Java.builtins ++ Builtins.builtins


runModule :: FilePath -> Module SrcSpan -> IO [Rule]
runModule dir (Module xs) = concatMapM (runStmt dir) xs

runStmt :: FilePath -> Statement SrcSpan -> IO [Rule]
runStmt dir (StmtExpr x _) = do
    VRule x <- runExpr dir x
    return [x]

runExpr :: FilePath -> Expr SrcSpan -> IO Value
runExpr dir Call{..} = do
    VFun f <- runExpr dir call_fun
    xs <- forM call_args $ \x -> do
        let name = case x of
                ArgKeyword{..} -> Just $ ident_string arg_keyword
                _ -> Nothing
        (name,) <$> runExpr dir (arg_expr x)
    f xs
runExpr dir Var{..} | Just x <- lookup (ident_string var_ident) builtins = return $ VFun $ x dir
runExpr dir Strings{..} = return $ VString $ concatMap read strings_strings
runExpr dir List{..} = do
    xs <- mapM (runExpr dir) list_exprs
    return $ VList xs
runExpr dir x = error $ show x
