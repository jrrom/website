{-# LANGUAGE OverloadedStrings #-}

module Main where

----------------------------------------------------------------
import           Data.Ord    (comparing)
import           Hakyll
import           System.FilePath (takeFileName)

import           Text.Blaze.Html                 (toHtml, toValue, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import Config    (config, myFeedConfiguration)
import Contexts  (siteCtx, postCtx, postListCtx)
import Compilerender (scssRules, renderPost, renderDefault, renderPage)
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
    compile $ pandocCompiler >>= renderDefault siteCtx

  -- resources ------------------------------------------------------------
  match ("fonts/*" .||. "images/*" .||. "js/*") $ do
    route idRoute
    compile copyFileCompiler

  scssRules "scss" "main.scss" "css/main.css"
  
  -- post ----------------------------------------------------------------
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ renderPost tags

  tagsRules tags $ \tag pattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx = postListCtx tags posts ("Posts tagged \"" ++ tag ++ "\"")
      makeItem "" >>= renderPage "templates/tag.html" ctx

  create ["blog.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let ctx = postListCtx tags posts "Blog Archives"
      makeItem "" >>= renderPage "templates/blog.html" ctx

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- loadAll "posts/*" >>= recentFirst >>= return . take 5

      let ctx =
            postListCtx tags posts "Home"
            <> constField "description" "Developer and programming language enthusiast! Always looking to learn more."
            <> constField "url" "/"
            <> indexTagsCtx tags

      getResourceBody
        >>= applyAsTemplate ctx
        >>= renderDefault ctx

  -- xml ----------------------------------------------------------------
  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      posts <- fmap (take 10) . recentFirst =<<
        loadAllSnapshots "posts/*" "content"
      renderRss myFeedConfiguration (postCtx tags) posts
      
  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      singlePages <- loadAll (fromList ["blog.html", "rss.xml"])
      let pages = posts <> singlePages
          ctx =
            listField "pages" siteCtx (pure pages)
            <> dateField "lastmod" "%Y-%m-%d"
            <> siteCtx
           
      makeItem ""
        >>= loadAndApplyTemplate "templates/sitemap.xml" ctx
  
  -- templates ----------------------------------------------------------
  match "templates/*" $ compile templateBodyCompiler
  
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

--------------------------------------------------------------------------------

postNumTagSort :: (String, [Identifier]) -> (String, [Identifier]) -> Ordering
postNumTagSort a b = comparing (length . snd) b a
        
--------------------------------------------------------------------------------
