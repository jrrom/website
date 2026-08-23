module Config where

----------------------------------------------------------------
import Hakyll
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

----------------------------------------------------------------
