{-# LANGUAGE OverloadedStrings #-}

module Main where

----------------------------------------------------------------
import           Data.List   (intercalate, intersperse)
import           Data.Monoid ((<>))
import           Data.Ord    (comparing)
import           Hakyll
import           Text.Pandoc.Class (runPure)
import           Text.Pandoc.Options (WriterOptions(..))
import           Text.Pandoc.Templates (compileTemplate, runWithDefaultPartials)
import           System.FilePath (takeFileName)

import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import Config (root, config, myFeedConfiguration)
import Compilers (compressScssCompiler, scssRules)
----------------------------------------------------------------

main :: IO ()
main = hakyllWith config $ do
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  -- root --------------------------------------------------------------
  match ("CNAME" .||. "robots.txt") $ do
    route (customRoute (takeFileName . toFilePath))
    compile copyFileCompiler

  match "404.org" $ do
    route $ setExtension "html"
    let ctx =
          constField "root" root <>
          defaultContext
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls

  -- resources ------------------------------------------------------------
  match ("fonts/*" .||. "images/*" .||. "js/*") $ do
    route idRoute
    compile copyFileCompiler

  scssRules "scss" "main.scss" "css/main.css"
  
  -- posts --------------------------------------------------------------
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ do
      ident <- getUnderlying
      toc   <- getMetadataField ident "toc"

      let writerSettings = case toc of
            Just _  -> withToc
            Nothing -> defaultHakyllWriterOptions

      pandocCompilerWith defaultHakyllReaderOptions writerSettings
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
        >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
        >>= relativizeUrls

  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged \"" ++ tag ++ "\""
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx =
            constField "title" title <>
            listField "posts" (postCtxWithTags tags) (return posts) <>
            constField "root" root <>
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  create ["blog.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" (postCtxWithTags tags) (return posts) <>
            constField "title" "Archives" <>
            constField "root" root <>
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- loadAll "posts/*" >>= recentFirst >>= return . take 5

      let indexCtx =
            listField "posts" (postCtxWithTags tags) (return posts) <>
            constField "description" "Developer and programming language enthusiast! Always looking to learn more." <>
            constField "root" root <>
            constField "url" "/" <>
            indexTagsCtx tags <>
            defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  -- xml ----------------------------------------------------------------
  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = (postCtxWithTags tags)
      posts <- fmap (take 10) . recentFirst =<<
        loadAllSnapshots "posts/*" "content"
      renderRss myFeedConfiguration feedCtx posts
      
  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      singlePages <- loadAll (fromList ["archive.html", "lists.org", "rss.xml"])
      let pages = posts <> singlePages
          sitemapCtx =
            constField "root" root <>
            listField "pages" (postCtxWithTags tags) (return pages)
           
      makeItem ""
        >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
  
  -- templates ----------------------------------------------------------
  match "templates/*" $ compile templateBodyCompiler
  
--------------------------------------------------------------------------------

withToc :: WriterOptions
withToc = defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerNumberSections = True
        , writerTOCDepth = 3
        , writerTemplate = Just (
            either error id $ either (error . show) id
          $ runPure
          $ runWithDefaultPartials
          $ compileTemplate "" "<div id=\"toc\">$toc$</div>\n$body$"
          )
        }

--------------------------------------------------------------------------------

indexRenderTags :: Tags -> Compiler String
indexRenderTags tags =
  renderTags
  (\tag url count _ _ ->
      "<li class=\"" ++ tag ++ "\"> <a href=\"" ++ url ++ "\">"
      ++ tag ++
      "</a>" ++ "<i> " ++ (show count) ++ " </i>" ++ "</li>")
  unlines
  (sortTagsBy postNumTagSort tags)

jrromRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
jrromRenderLink _ Nothing = Nothing
jrromRenderLink tag (Just filePath) = Just $
  H.li ! A.class_ (toValue tag) $
      H.a ! A.href (toValue $ toUrl filePath) $
          toHtml tag

indexTagsCtx :: Tags -> Context String
indexTagsCtx tags =
  field "tags" $ \_ ->
                   indexRenderTags tags

postCtxWithTags tags =
  constField "root" root <>
  dateField "date" "%e %B, %Y" <>
  tagsFieldWith getTags jrromRenderLink (mconcat . intersperse "\n") "tags" tags <>
  defaultContext

--------------------------------------------------------------------------------

postNumTagSort :: (String, [Identifier]) -> (String, [Identifier]) -> Ordering
postNumTagSort a b = comparing (length . snd) b a
        
--------------------------------------------------------------------------------
