{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import Hakyll

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
  { feedTitle = "Журнал Дмитрия Джуса"
  , feedDescription = "Дмитрий Джус"
  , feedAuthorName = "Дмитрий Джус"
  , feedAuthorEmail = "dima@dzhus.org"
  , feedRoot = "http://dzhus.org"
  }

main :: IO ()
main =
  hakyll $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match "templates/*" $ compile templateBodyCompiler

    match "pages/*" $ do
      route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension ""
      compile $
        pandocCompiler >>=
        loadAndApplyTemplate "templates/default.html" defaultContext >>=
        relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tag/*")

    -- Paginate the whole history by pages of 1 to provide links to
    -- previous/next post
    allPosts <- sortChronological =<< getMatches "posts/*"
    postStream <- buildPaginateWith (return . paginateEvery 1)
                  "posts/*"
                  -- Make page identifiers equal to file identifiers
                  (\n -> allPosts !! (n - 1))

    paginateRules postStream $ \pn _ ->
      let
        loadRaw = load $ setVersion (Just "raw") (paginateMakeId postStream pn)
        postCtx' =
          listField "alternates" postCtx (return <$> loadRaw) <>
          paginateContext postStream pn <>
          postCtxWithTags tags
      in do
        route $ setExtension ""
        compile $
          pandocCompiler >>=
          saveSnapshot "html" >>=
          loadAndApplyTemplate "templates/post.html" postCtx' >>=
          loadAndApplyTemplate "templates/default.html" postCtx' >>=
          relativizeUrls

    let allPosts =
          "posts/*" .&&.
          hasNoVersion .&&.
          complement "posts/index.html"

    match "posts/*" $ version "raw" $ do
      route idRoute
      compile getResourceString

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 20) . recentFirst =<<
                 loadAllSnapshots allPosts "html"
        renderAtom feedConfiguration feedCtx posts

    create ["posts/index.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll allPosts
        let ctx = listField "posts" postCtx (return posts) <>
                  defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/post-list.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    tagsRules tags $ \tag pat -> do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pat
        let ctx = constField "title" tag <>
                  listField "posts" postCtx (return posts) <>
                  defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/post-list.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

postCtx :: Context String
postCtx = dateField "date" "%d.%m.%Y" <> defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags =
  tagsField "tags" tags <> postCtx
