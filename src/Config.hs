{-# LANGUAGE OverloadedStrings #-}

module Config where

----------------------------------------------------------------
import Hakyll
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Options (WriterOptions(..))
import Text.Pandoc.Templates (compileTemplate, runWithDefaultPartials)
----------------------------------------------------------------

root :: String
root = "https://jrrom.com"

config :: Configuration
config = defaultConfiguration
  {
    destinationDirectory = "docs"
  , storeDirectory       = "dist-newstyle/hakyll-store"
  , tmpDirectory         = "dist-newstyle/hakyll-tmp"
  , providerDirectory    = "content"
  }

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration =
    FeedConfiguration
        { feedTitle       = "jrrom's Blog"
        , feedDescription = "Posts about general topics and development."
        , feedAuthorName  = "jrrom"
        , feedAuthorEmail = "web@jrrom.com"
        , feedRoot        = root
        }

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

handleTocSetting :: Maybe String -> WriterOptions
handleTocSetting Nothing  = defaultHakyllWriterOptions
handleTocSetting (Just _) = withToc
