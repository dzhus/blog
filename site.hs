{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Data.Monoid
import Hakyll hiding (fromList)
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Skylighting.Format.HTML
import Skylighting.Styles

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
  { feedTitle = "Журнал Дмитрия Джуса"
  , feedDescription = "Дмитрий Джус"
  , feedAuthorName = "Дмитрий Джус"
  , feedAuthorEmail = "dima@dzhus.org"
  , feedRoot = "http://dzhus.org"
  }

-- | If there's a leading level-1 heading in the document, split it
-- from the document and return it separately as a string along with
-- the document sans the heading. Otherwise, return the document
-- unchanged.
extractLeadingH1
  :: Pandoc
  -> (Maybe String, Pandoc)
extractLeadingH1 (Pandoc m (Header 1 _ text:rest)) =
  (Just $ writePlain def $ doc $ fromList [Plain text], Pandoc m rest)
extractLeadingH1 d = (Nothing, d)

-- | Try 'extractLeadingH1' and add a new context field with the
-- extracted heading text. Otherwise, return empty context and the
-- document unchanged.
extractLeadingH1Context
  :: String
  -- ^ New context field name.
  -> Item Pandoc
  -> (Context a, Item Pandoc)
extractLeadingH1Context fieldName doc =
  case extractLeadingH1 <$> doc of
    Item _ (Nothing, _)     -> (mempty, doc)
    Item _ (Just h, newDoc) -> (newCtx, itemSetBody newDoc doc)
      where
        newCtx = field fieldName $ const $ return h

main :: IO ()
main =
  hakyll $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    create ["css/syntax.css"] $ do
      route idRoute
      compile $ makeItem $ styleToCss tango

    match "templates/*" $ compile templateBodyCompiler

    match "pages/*" $ do
      route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension ""
      compile $
        pandocCompiler >>=
        loadAndApplyTemplate "templates/single-page.html" defaultContext >>=
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

    -- Single post rendering rule
    paginateRules postStream $ \pn _ ->
      let
        thisPostId = paginateMakeId postStream pn
        loadRaw = load $ setVersion (Just "raw") thisPostId
      in do
        hasTags <- getMetadataField thisPostId "tags"
        route $ setExtension ""
        compile $ do
          post <- readPandoc =<< getResourceBody
          let (postTitleCtx, post') =
                extractLeadingH1Context "title" post
              postCtx' =
                listField "alternates" postCtx (return <$> loadRaw) <>
                paginateContext postStream pn <>
                boolField "hasTags" (const $ isJust hasTags) <>
                tagsField "tags" tags <>
                postTitleCtx <>
                postCtx

          saveSnapshot "html" (writePandoc post') >>=
            loadAndApplyTemplate "templates/post.html" postCtx' >>=
            saveSnapshot "post" >>=
            loadAndApplyTemplate "templates/single-page.html" postCtx' >>=
            loadAndApplyTemplate "templates/page-navigation.html" postCtx' >>=
            loadAndApplyTemplate "templates/default.html" postCtx' >>=
            relativizeUrls

    match "posts/*" $ version "raw" $ do
      route idRoute
      compile getResourceString

    postPages <- buildPaginateWith (return . paginateEvery 10)
                 "posts/*"
                 (\n -> fromFilePath $ "page-" <> show n <> ".html")

    paginateRules postPages $ \pn pagePat ->
      let pageCtx =
            listField "posts" postCtx
            (recentFirst =<< loadAllSnapshots pagePat "post") <>
            paginateContext postPages pn <>
            defaultContext
      in do
        route idRoute
        compile $
          makeItem "" >>=
          loadAndApplyTemplate "templates/blog-page.html" pageCtx >>=
          loadAndApplyTemplate "templates/default.html" pageCtx >>=
          relativizeUrls

    let renderedPosts =
          "posts/*" .&&.
          hasNoVersion .&&.
          complement "posts/index.html"

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 20) . recentFirst =<<
                 loadAllSnapshots renderedPosts "html"
        renderAtom feedConfiguration feedCtx posts

    create ["posts/index.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll renderedPosts
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
