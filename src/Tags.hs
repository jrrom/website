module Tags where

--------------------------------------------------------------
import Data.Ord (comparing)
import Hakyll

import Text.Blaze.Html (toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
-------------------------------------------------------------

indexRenderTags :: Tags -> Compiler String
indexRenderTags tags =
    renderTags
        (\tag url count _ _ ->
            "<li class=\"" ++ tag ++ "\"> <a href=\"" ++ url ++ "\">"
            ++ tag
            ++ "</a><i> " ++ show count ++ " </i></li>")
        unlines
        (sortTagsBy postNumTagSort tags)

jrromRenderLink :: String -> Maybe FilePath -> Maybe H.Html
jrromRenderLink _ Nothing = Nothing
jrromRenderLink tag (Just filePath) =
    Just $
        H.li ! A.class_ (toValue tag) $
            H.a ! A.href (toValue $ toUrl filePath) $
                toHtml tag

indexTagsCtx :: Tags -> Context String
indexTagsCtx tags =
    field "tags" $ \_ ->
        indexRenderTags tags

postNumTagSort
    :: (String, [Identifier])
    -> (String, [Identifier])
    -> Ordering
postNumTagSort a b =
    comparing (length . snd) b a
