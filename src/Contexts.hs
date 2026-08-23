{-# LANGUAGE OverloadedStrings #-}

module Contexts where

-------------------------------------------------------------
import Data.List (intersperse)
import Hakyll

import Config (root)
import Tags (jrromRenderLink)
-------------------------------------------------------------

siteCtx :: Context String
siteCtx =
    constField "root" root
    <> defaultContext

postCtx :: Tags -> Context String
postCtx tags =
    dateField "date" "%e %B, %Y"
    <> tagsFieldWith getTags jrromRenderLink (mconcat . intersperse "\n") "tags" tags
    <> siteCtx

postListCtx :: Tags -> [Item String] -> String -> Context String
postListCtx tags posts title =
    listField "posts" (postCtx tags) (return posts)
    <> constField "title" title
    <> siteCtx

-------------------------------------------------------------
