module Compilers where

-------------------------------------------------------------------
import Hakyll
import System.FilePath ((</>))

import Config (config)
-------------------------------------------------------------------

compressScssCompiler :: FilePath -> Compiler (Item String)
compressScssCompiler loadPath =
    getResourceString
        >>= withItemBody
            (unixFilter "sass"
                [ "--stdin"
                , "--style", "compressed"
                , "--load-path", loadPath
                ])
        >>= return . fmap compressCss

scssRules :: FilePath -> FilePath -> FilePath -> Rules ()
scssRules srcDir mainFile destFile = do
    deps <- makePatternDependency (fromGlob $ srcDir </> "**/*")
    rulesExtraDependencies [deps] $
        match (fromGlob $ srcDir </> mainFile) $ do
            route   $ constRoute destFile
            compile $ compressScssCompiler (providerDirectory config </> srcDir)
