{-# LANGUAGE OverloadedStrings #-}

module Compilerender where

-------------------------------------------------------------------
import Hakyll
import System.FilePath ((</>))
import Control.Monad ((>=>))

import Contexts(postCtx)
import Config (config, handleTocSetting)
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

renderPost :: Tags -> Compiler (Item String)
renderPost tags = do
    ident <- getUnderlying
    toc   <- getMetadataField ident "toc"

    let ctx = postCtx tags

    pandocCompilerWith
        defaultHakyllReaderOptions
        (handleTocSetting toc)
        >>= saveSnapshot "content"
        >>= renderPage "templates/post.html" ctx

renderDefault :: Context String -> Item String -> Compiler (Item String)
renderDefault ctx =
    loadAndApplyTemplate "templates/default.html" ctx
        >=> relativizeUrls

renderPage :: FilePath -> Context String -> Item String -> Compiler (Item String)
renderPage template ctx =
    loadAndApplyTemplate (fromFilePath template) ctx
        >=> renderDefault ctx
-------------------------------------------------------------------
