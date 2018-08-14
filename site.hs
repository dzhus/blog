import ClassyPrelude
import Data.Digest.Pure.MD5
import Data.Time (iso8601DateFormat)
import Hakyll hiding (defaultContext)
import Text.Pandoc
import Skylighting.Format.HTML
import Skylighting.Styles

import qualified Hakyll as H

creator :: String
creator = "Дмитрий Джус"

email :: String
email = "dima@dzhus.org"

siteTitle :: String
siteTitle = "Журнал Дмитрия Джуса"

rootUrl :: String
rootUrl = "http://dzhus.org"

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
  { feedTitle = siteTitle
  , feedDescription = ""
  , feedAuthorName = creator
  , feedAuthorEmail = email
  , feedRoot = rootUrl
  }

-- | If there's a leading level-1 heading in the document, split it
-- from the document and return it separately as a string along with
-- the document sans the heading. Otherwise, return the document
-- unchanged.
extractLeadingH1
  :: Pandoc
  -> (Maybe Text, Pandoc)
extractLeadingH1 (Pandoc m (Header 1 _ text:rest)) =
  ( headMay $ rights [runPure $ writePlain def $ Pandoc nullMeta [Plain text]]
  , Pandoc m rest
  )
extractLeadingH1 d = (Nothing, d)

-- | Try 'extractLeadingH1' and add a new context field with the
-- extracted heading text. Otherwise, return empty context and the
-- document unchanged.
leadingH1Context
  :: Context String
leadingH1Context =
  field "title" $ \i -> do
     pd <- readPandoc =<< load (setVersion (Just "raw") $ itemIdentifier i)
     case fst $ extractLeadingH1 $ itemBody pd of
       Just s -> return $ unpack s
       Nothing -> empty

pandocWithoutLeadingH1 :: Item String -> Compiler (Item String)
pandocWithoutLeadingH1 s = do
  p <- readPandoc s
  (return . writePandoc) $ snd . extractLeadingH1 <$> p

mkDefaultContext :: IO (Context String)
mkDefaultContext = do
  now <- ClassyPrelude.getCurrentTime
  return $
    constField "creator" creator <>
    constField "gravatar"
    ("https://www.gravatar.com/avatar/" <>
     show (md5 $ fromString email) <> "?s=200") <>
    constField "siteTitle" siteTitle <>
    constField "thisYear" (formatTime defaultTimeLocale "%Y" now) <>
    H.defaultContext

finishTemplating :: Context a -> Item a -> Compiler (Item String)
finishTemplating ctx i =
  loadAndApplyTemplate "templates/default.html" ctx i >>= relativizeUrls

filterBy :: MonadMetadata m => String -> Maybe String -> [Item a] -> m [Item a]
filterBy field val =
  filterM (\Item{..} -> (== val) <$> getMetadataField itemIdentifier field)

main :: IO ()
main = do
  defaultContext <- mkDefaultContext
  let
    postCtx :: Context String
    postCtx = mconcat
      [ leadingH1Context
      , constField "rootUrl" rootUrl
      , dateField "date" "%d.%m.%Y"
      , dateField "isoDate" "%F"
      , modificationTimeField "modificationDate" "%F"
      , modificationTimeField "updated" (iso8601DateFormat (Just "%T%z"))
      , defaultContext
      ]

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
      route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
      compile $
        pandocCompiler >>=
        loadAndApplyTemplate "templates/single-page.html" defaultContext >>=
        loadAndApplyTemplate "templates/default.html" defaultContext

    -- Snapshot raw body of each resource, this is needed for `title`
    -- field in postCtx to work (populated via leadingH1Context that
    -- uses snapshots)
    match "posts/*" $ version "raw" $ compile $ do
      raw <- getResourceBody
      saveSnapshot "raw" raw

    let renderedPosts =
          "posts/*" .&&.
          hasNoVersion .&&.
          complement "posts/index.html"

    tags <- buildTags renderedPosts (fromCapture "tag/*")

    -- Paginate the whole history by pages of 1 to provide links to
    -- previous/next post
    allPosts <- sortChronological =<< getMatches renderedPosts
    postStream <- buildPaginateWith (fmap (paginateEvery 1) . sortChronological)
                  renderedPosts
                  -- Make page identifiers equal to file identifiers
                  (\n -> fromMaybe "?" $ index allPosts (n - 1))

    paginateRules postStream $ \pn _ -> do
      hasTags <- getMetadataField (paginateMakeId postStream pn) "tags"
      route $ setExtension "html"

      compile $ do
        html <- getResourceBody >>= pandocWithoutLeadingH1

        -- Populate $description$ from rendered post body
        let postCtx' =
              paginateContext postStream pn <>
              boolField "hasTags" (const $ isJust hasTags) <>
              tagsField "tags" tags <>
              constField "description"
              ((<> "…") $ take 190 $ stripTags $ itemBody html) <>
              postCtx

        saveSnapshot "html" html >>=
          loadAndApplyTemplate "templates/post.html" postCtx' >>=
          saveSnapshot "post" >>=
          loadAndApplyTemplate "templates/single-page.html" postCtx' >>=
          loadAndApplyTemplate "templates/page-navigation.html" postCtx' >>=
          finishTemplating postCtx'

    create ["index.html"] $ do
      route idRoute
      compile $ do
        posts <- fmap (take 5) . recentFirst =<< loadAll renderedPosts
        let ctx = listField "posts" postCtx (return posts) <>
                  constField "pageTitle" siteTitle <>
                  defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/index.html" ctx
          >>= finishTemplating ctx

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx <> bodyField "description"
        posts <- fmap (take 20) . recentFirst =<<
                 filterBy "lang" Nothing =<<
                 loadAllSnapshots renderedPosts "html"
        renderAtom feedConfiguration feedCtx posts

    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll renderedPosts
        let sitemapCtx = listField "entries" postCtx (return posts)
        makeItem ("" :: String)
          >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    create ["posts/index.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll renderedPosts
        let ctx = listField "posts" postCtx (return posts) <>
                  defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/post-list.html" ctx
          >>= finishTemplating ctx

    create ["tag/index.html"] $ do
      route idRoute
      compile $
        renderTagCloud 100 150 tags
        >>= makeItem
        >>= loadAndApplyTemplate "templates/single-page.html" defaultContext
        >>= finishTemplating defaultContext

    tagsRules tags $ \tag pat -> do
      route $ setExtension "html"
      compile $ do
        posts <- recentFirst =<< loadAll pat
        let ctx = constField "title" tag <>
                  listField "posts" postCtx (return posts) <>
                  defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/post-list.html" ctx
          >>= finishTemplating defaultContext
