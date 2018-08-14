import ClassyPrelude
import Data.Digest.Pure.MD5
import Data.Time (iso8601DateFormat)
import Hakyll hiding (defaultContext)
import Text.Pandoc
import Skylighting.Format.HTML
import Skylighting.Styles

import qualified Hakyll as H

defaultCreator :: String
defaultCreator = "Дмитрий Джус"

defaultTitle :: String
defaultTitle = "Журнал Дмитрия Джуса"

email :: String
email = "dima@dzhus.org"

rootUrl :: String
rootUrl = "http://dzhus.org"

defaultLang :: String
defaultLang = "ru"

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
  { feedTitle = "Dmitry's journal"
  , feedDescription = ""
  , feedAuthorName = "Dmitry Dzhus"
  , feedAuthorEmail = email
  , feedRoot = rootUrl
  }

-- Language-specific context, populated from item metadata or an
-- external language field.
langCtx :: Maybe String -> Context a
langCtx extLang = mconcat
  [ langField "creator"
    [ (Just "en", "Dmitry Dzhus")
    , (Nothing, defaultCreator)
    ]
  , langField "siteTitle"
    [ (Just "en", "Dmitry's journal")
    , (Nothing, defaultTitle)
    ]
  ]
  where
    langField key lkp = field key $ \Item{..} -> do
      lang <- getMetadataField itemIdentifier "lang"
      return $ fromMaybe "" $ lookup (lang <|> extLang) lkp

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
-- document unchanged. Uses @raw@ version of the item snapshot.
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
    constField "gravatar"
    ("https://www.gravatar.com/avatar/" <>
     show (md5 $ fromString email) <> "?s=200") <>
    constField "thisYear" (formatTime defaultTimeLocale "%Y" now) <>
    H.defaultContext

finishTemplating :: Context a -> Item a -> Compiler (Item String)
finishTemplating ctx i =
  loadAndApplyTemplate "templates/default.html" ctx i >>= relativizeUrls

filterBy :: MonadMetadata m
         => String -> Maybe String -> [Identifier] -> m [Identifier]
filterBy f val = filterM (fmap (== val) . flip getMetadataField f)

main :: IO ()
main = do
  defaultContext <- mkDefaultContext
  let
    fullCtx :: Context String
    fullCtx = mconcat
      [ leadingH1Context
      , constField "rootUrl" rootUrl
      , constField "lang" defaultLang
      , constField "langPrefix" ""
      , dateField "date" "%d.%m.%Y"
      , dateField "isoDate" "%F"
      , modificationTimeField "modificationDate" "%F"
      , modificationTimeField "updated" (iso8601DateFormat (Just "%T%z"))
      , langCtx Nothing
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

    match "templates/**" $ compile templateBodyCompiler

    match "pages/**" $ do
      route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
      compile $
        pandocCompiler >>=
        loadAndApplyTemplate "templates/single-page.html" fullCtx >>=
        loadAndApplyTemplate "templates/default.html" fullCtx

    -- Snapshot raw body of each resource, this is needed for `title`
    -- field in fullCtx to work (populated via leadingH1Context that
    -- uses snapshots)
    match "posts/*" $ version "raw" $ compile $ do
      raw <- getResourceBody
      saveSnapshot "raw" raw

    let renderedPosts =
          "posts/*" .&&.
          hasNoVersion .&&.
          complement "posts/index.html"

    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll renderedPosts
        let sitemapCtx = listField "entries" fullCtx (return posts)
        makeItem ("" :: String)
          >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    -- Combined Atom feed
    create ["atom-all.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = fullCtx <> bodyField "description"
        posts <- fmap (take 20) . recentFirst =<<
                 loadAllSnapshots renderedPosts "html"
        renderAtom feedConfiguration feedCtx posts

    -- Tags are not split by languages
    tags <- buildTags renderedPosts (fromCapture "tag/*")

    create ["tag/index.html"] $ do
      route idRoute
      compile $
        renderTagCloud 100 150 tags
        >>= makeItem
        >>= loadAndApplyTemplate "templates/single-page.html" fullCtx
        >>= finishTemplating fullCtx

    tagsRules tags $ \tag pat -> do
      route $ setExtension "html"
      compile $ do
        posts <- recentFirst =<< loadAll pat
        let ctx = constField "title" tag <>
                  listField "posts" fullCtx (return posts) <>
                  fullCtx
        makeItem ""
          >>= loadAndApplyTemplate "templates/post-list.html" ctx
          >>= finishTemplating fullCtx

    -- Language-specific stuff below
    forM_ [Nothing, Just "en"] $ \lang -> do
      allPosts <- sortChronological =<<
                  filterBy "lang" lang =<<
                  getMatches renderedPosts

      let langPrefix = maybe "" (<> "/") lang
          fullCtx' = constField "lang" (fromMaybe defaultLang lang) <>
                     constField "langPrefix" langPrefix <>
                     langCtx lang <>
                     fullCtx

      -- Home page
      create [fromString $ langPrefix <> "index.html"] $ do
        route idRoute
        compile $ do
          posts <- fmap (take 5) . recentFirst =<<
                   loadAll (H.fromList allPosts)
          let ctx = listField "posts" fullCtx' (return posts) <>
                    fullCtx'
          makeItem ""
            >>= loadAndApplyTemplate
                (fromString $ "templates/" <> langPrefix <> "index.html") ctx
            >>= finishTemplating ctx

      -- Post list
      create [fromString $ langPrefix <> "posts/index.html"] $ do
        route idRoute
        compile $ do
          posts <- recentFirst =<< loadAll (H.fromList allPosts)
          let ctx = listField "posts" fullCtx' (return posts) <>
                    fullCtx'
          makeItem ""
            >>= loadAndApplyTemplate "templates/post-list.html" ctx
            >>= finishTemplating ctx

      -- Language-specific Atom feed
      create [fromString $ langPrefix <> "atom.xml"] $ do
        route idRoute
        compile $ do
          let feedCtx = fullCtx' <> bodyField "description"
          posts <- fmap (take 20) . recentFirst =<<
                   loadAllSnapshots (H.fromList allPosts) "html"
          renderAtom feedConfiguration feedCtx posts

      -- Paginate the whole history by pages of 1 to provide links to
      -- previous/next post
      postStream <- buildPaginateWith
                    (fmap (paginateEvery 1) . sortChronological)
                    (H.fromList allPosts)
                    -- Make page identifiers equal to file identifiers
                    (\n -> fromMaybe "?" $ index allPosts (n - 1))

      paginateRules postStream $ \pn _ -> do
        route $ composeRoutes
          (setExtension "html")
          (gsubRoute "posts" $ const (langPrefix <> "posts"))

        compile $ do
          html <- getResourceBody >>= pandocWithoutLeadingH1

          -- Populate $description$ from rendered post body
          let postCtx =
                paginateContext postStream pn <>
                constField "description"
                ((<> "…") $ take 190 $ stripTags $ itemBody html) <>
                fullCtx'

          saveSnapshot "html" html >>=
            loadAndApplyTemplate "templates/post.html" postCtx >>=
            saveSnapshot "post" >>=
            loadAndApplyTemplate "templates/single-page.html" postCtx >>=
            loadAndApplyTemplate "templates/page-navigation.html" postCtx >>=
            finishTemplating postCtx
