{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import Hakyll

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
    allPosts <- getMatches "posts/*"
    paginate <- buildPaginateWith (return . paginateEvery 1)
                "posts/*"
                -- Make page identifiers equal to file identifiers
                (\n -> allPosts !! (n - 1))

    paginateRules paginate $ \pn _ ->
      let
        loadRaw = load $ setVersion (Just "raw") (paginateMakeId paginate pn)
        postCtx' =
          listField "alternates" postCtx (return <$> loadRaw) <>
          paginateContext paginate pn <>
          postCtxWithTags tags
      in do
        route $ setExtension ""
        compile $
          pandocCompiler >>=
          loadAndApplyTemplate "templates/post.html" postCtx' >>=
          loadAndApplyTemplate "templates/default.html" postCtx' >>=
          relativizeUrls

    match "posts/*" $ version "raw" $ do
      route idRoute
      compile getResourceString

    create ["posts/index.html"] $ do
      route idRoute
      compile $ do
        posts <- loadAll $ "posts/*" .&&.
                 hasNoVersion .&&.
                 complement "posts/index.html"
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
