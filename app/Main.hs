module Main (main) where

import Control.Monad.Trans.Class (lift)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as TL
import Data.Time (Day, fromGregorian, UTCTime(..))
import Data.Time.Format (defaultTimeLocale, parseTimeM, formatTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUIDV5
import Development.Shake (putInfo)
import qualified Development.Shake.FilePath as FilePath
import Development.Shake.Plus hiding ((-<.>))
import Lucid
import Lucid.Base (makeAttributes)
import Main.Path.Combinators
import Main.Path.Relative
import Network.URI (URI (..), escapeURIString, isUnescapedInURIComponent, uriToString)
import Network.URI.Static (uri)
import Text.Pandoc (enableExtension, pandocExtensions)
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Class (PandocPure, runPure)
import Text.Pandoc.Definition (Pandoc (..), lookupMeta)
import Text.Pandoc.Highlighting (pygments)
import Text.Pandoc.Shared (stringify)
import Text.Atom.Feed (Entry(..), TextContent (..), Feed (..))
import qualified Text.Atom.Feed.Export as Atom
import Web.Sitemap.Gen (Sitemap (..), SitemapUrl (..), renderSitemap)
import Control.Monad.Trans.State (StateT (..), gets, modify)

-- Configuration
-----------------------------------------------------------------------

-- | The name of the website.
siteName :: Text
siteName = "jtm.cx"

-- | The base URI for the website.
baseUri :: URI
baseUri = [uri|https://jtm.cx|]

-- | The directory to place the generated HTML.
htmlDir :: Path Rel Dir
htmlDir = [reldir|_site/html|]

-- | The directory for storing shake metadata files.
shakeDir :: Path Rel Dir
shakeDir = [reldir|_build|]


-- Utilities
-----------------------------------------------------------------------

-- | Parse a date in YYYY-MM-DD format.
parseDay :: MonadFail m => Text -> m Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack

-- | Format a date in YYYY-MM-DD format.
formatDay :: Day -> Text
formatDay = T.pack . formatTime defaultTimeLocale "%Y-%m-%d" 

-- | A dummy date.
epochDay :: Day
epochDay = fromGregorian 1970 01 01

-- | Parse a UUID.
parseUuid :: MonadFail m => Text -> m UUID
parseUuid s =
  case UUID.fromText s of
    Just uuid -> pure uuid
    Nothing   -> fail $ "invalid uuid: " ++ T.unpack s

-- | URI-escape a given 'PagePath'.
escapeAbsPath :: Path Abs t -> Text
escapeAbsPath path =
    let segments = drop 1 (FilePath.splitDirectories (toFilePath path)) in
    "/" <> T.intercalate "/" (map escapeSegment segments)
  where
    escapeSegment = T.pack . escapeURIString isUnescapedInURIComponent

-- | Convert a URI to a string.
uriString :: URI -> String 
uriString x = uriToString id x ""

-- | Convert a URI to text.
uriText :: URI -> Text
uriText = T.pack . uriString

-- | Generate the fully-qualified URI to a given page.
qualifyWith :: URI -> Path Abs t -> URI
qualifyWith base path = base { uriPath = T.unpack (escapeAbsPath path) }


-- Post Front-matter
-----------------------------------------------------------------------

-- | Post front-matter.
data PostMeta = PostMeta
  { postTitle :: Text
    -- ^ The title of the post.
  , postSummary :: Maybe Text
    -- ^ Summary of the article contents. Optional.
  , postDate :: Day
    -- ^ The day the article was published.
  , postUpdated :: Maybe Day
    -- ^ The date of the most recent update.
  , postUuid :: Maybe UUID
    -- ^ Every post has a unique identifier. This identifier is permanent.
    -- It's used to identify the post in Atom feeds, and allows us to
    -- identify posts even if the title, url, or contents change.
    --
    -- This field is optional. If it's not provided, the UUID will be
    -- calculated deterministically using the title, publish date, and
    -- the global Atom feed id. If the title needs to be updated for a
    -- published article, then the generated Uuid *must* be copied into
    -- the post's metadata in order to preserve it.
  }

-- | Parse a post's front-matter.
parsePostMeta :: MonadFail m => Pandoc -> m PostMeta
parsePostMeta (Pandoc meta _) = do
    title   <- pure $ field "title"
    summary <- pure $ field "summary"
    date    <- mapM parseDay (field "date")
    updated <- mapM parseDay (field "updated")
    uuid    <- mapM parseUuid (field "uuid")
    pure $ PostMeta 
      { postTitle   = title `orElse` "Untitled"
      , postSummary = summary
      , postDate    = date `orElse` epochDay
      , postUpdated = updated
      , postUuid    = uuid
      }
  where
    -- todo: make sure that meta value is plain text.
    field :: Text -> Maybe Text
    field key = fmap stringify $ lookupMeta key meta

    -- todo: this isn't somewhere in a core library?
    orElse :: Maybe a -> a -> a
    orElse = flip fromMaybe

-- | The day of the most recent update.
lastUpdate :: PostMeta -> Day
lastUpdate meta = fromMaybe (postDate meta) (postUpdated meta)


-- Atom Feed
-----------------------------------------------------------------------

-- | The global atom feed id.
atomFeedId :: UUID
atomFeedId = 
  let bytes = map (fromIntegral . fromEnum) (T.unpack siteName) in
  UUIDV5.generateNamed UUIDV5.namespaceDNS bytes

-- | The atom id for a given post.
atomEntryId :: PostMeta -> UUID
atomEntryId meta = 
  case postUuid meta of
    Just uuid -> uuid
    Nothing -> UUIDV5.generateNamed atomFeedId bytes
  where
    -- Hash is calculated deterministically from the date and title.
    -- If either are updated, the original UUID needs to be preserved
    -- in the post front-matter.
    bytes = map (fromIntegral . fromEnum) $
      T.unpack $ formatDay (postDate meta) <> postTitle meta

atomFeed :: [PostMeta] -> Feed
atomFeed posts = Feed
    { feedId = "urn:uuid:" <> UUID.toText atomFeedId
    , feedTitle = TextString siteName
    , feedUpdated = formatDay mostRecentUpdate
    , feedAuthors = []
    , feedCategories = []
    , feedContributors = []
    , feedGenerator = Nothing
    , feedIcon = Nothing
    , feedLinks = []
    , feedLogo = Nothing
    , feedRights = Nothing
    , feedSubtitle = Nothing
    , feedEntries = map atomPostEntry posts
    , feedAttrs = []
    , feedOther = []
    }
  where
    mostRecentUpdate :: Day
    mostRecentUpdate = 
      case sortOn Down $ map lastUpdate posts of
        day : _ -> day
        []      -> epochDay  -- No posts; use dummy 1970-01-01.

atomPostEntry :: PostMeta -> Entry
atomPostEntry meta = Entry
  { entryId = "urn:uuid:" <> UUID.toText (atomEntryId meta)
  , entryTitle = TextString $ postTitle meta
  , entryUpdated = formatDay (lastUpdate meta)
  , entryAuthors = []
  , entryCategories = []
  , entryContent = Nothing
  , entryContributor = []
  , entryLinks = []
  , entryPublished = Just $ formatDay (postDate meta)
  , entryRights = Nothing
  , entrySource = Nothing
  , entrySummary = TextString <$> postSummary meta
  , entryInReplyTo = Nothing
  , entryInReplyTotal = Nothing
  , entryAttrs = []
  , entryOther = []
  }


-- Sitemap
-----------------------------------------------------------------------

-- | Midnight UTC on the given day.
dayToUTCTime :: Day -> UTCTime
dayToUTCTime day = UTCTime day 0

-- | The sitemap entry for a given page.
sitemapUrl :: URI -> Maybe Day -> SitemapUrl
sitemapUrl target updated = SitemapUrl
  { sitemapLocation = uriText target
  , sitemapLastModified = dayToUTCTime <$> updated
  , sitemapChangeFrequency = Nothing
  , sitemapPriority = Nothing
  }

-- | The sitemap entry for a given post.
sitemapPostUrl :: (Path Rel File, PostMeta) -> Action SitemapUrl
sitemapPostUrl (src, meta) = do
  file <- exnFail $ [absdir|/|] </$ (src -<.> ".html")
  pure $ sitemapUrl (qualifyWith baseUri file) (Just (lastUpdate meta))

-- | The full sitemap for the home page and all posts.
sitemap :: [(Path Rel File, PostMeta)] -> Action Sitemap
sitemap posts = do
  postUrls <- mapM sitemapPostUrl posts
  let homeUrl = sitemapUrl (qualifyWith baseUri [absfile|/index.html|]) Nothing
  pure $ Sitemap (homeUrl : postUrls)


-- Actions
-----------------------------------------------------------------------

-- | Unwrap an 'Either', running 'fail' if 'Left'.
exnFail :: (MonadFail m, Show e) => Either e a -> m a
exnFail = either (fail . show) pure

-- | Run a pandoc monad as an action. Fails on error.
runPandoc :: PandocPure a -> Action a
runPandoc m = do
  case runPure m of
    Right x -> pure x
    Left err -> fail ("pandoc: " ++ show err)

-- | Read and parse a markdown file.
readMarkdown :: Path a File -> Action Pandoc
readMarkdown path = do
    contents <- readFile' path
    runPandoc $ Pandoc.readMarkdown readerOptions contents
  where
    readerOptions :: Pandoc.ReaderOptions
    readerOptions = Pandoc.def
      { Pandoc.readerExtensions = 
          -- Enable extension to read yaml front-matter.
          enableExtension Pandoc.Ext_yaml_metadata_block pandocExtensions
      }

-- | Convert a pandoc document to an html string.
documentHtml :: Pandoc -> Action Text
documentHtml doc = runPandoc $ Pandoc.writeHtml5String writerOptions doc
  where
    writerOptions :: Pandoc.WriterOptions
    writerOptions = Pandoc.def
      { Pandoc.writerHighlightStyle = Just pygments }

-- | Read and parse the front-matter of a post.
readPostMeta :: Path a File -> Action PostMeta
readPostMeta path = readMarkdown path >>= parsePostMeta

-- | Read and parse the front-matter for all posts. Note that posts
-- are not sorted in any way.
readAllPostMetas :: Action [(Path Rel File, PostMeta)]
readAllPostMetas = do
  posts <- getDirectoryFiles [reldir|.|] ["posts/*.md"]
  need $ map toFilePath posts
  zip posts <$> mapM readPostMeta posts


-- HTML Builders
-----------------------------------------------------------------------

-- | The HTML builder monad.
type Builder = HtmlT (StateT BuildState Action)

data BuildState = BuildState
  { pagePath :: Path Abs File 
    -- ^ The target location of the page we're building.
  , pageReferences :: Set (Path Abs File)
    -- ^ Set of internal files that this page links to.
  }

-- | Run a shake action in a builder.
action :: Action a -> Builder a
action = lift . lift

-- | Return the fully-qualified URI for this page.
-- For example: @ https://example.com/path/to/page.html @
pageUri :: Builder URI
pageUri = do
  path <- lift $ gets pagePath
  pure $ qualifyWith baseUri path

addReference :: Path Abs File -> Builder ()
addReference f = lift $ modify $ \s ->
  s { pageReferences = Set.insert f (pageReferences s) }

-- | Calculate the path relative to the current page.
withRelative :: Path Abs File -> (Text -> Builder a) -> Builder a
withRelative target f = do
  addReference target
  src <- lift $ gets pagePath
  f (T.pack $ relativeFile (parent src) target)

-- | Generate HTML and write it to a file.
runBuilder :: Path Rel File -> Builder () -> Action BuildState
runBuilder out builder = do
  path <- exnFail $ [absdir|/|] </$ stripProperPrefix htmlDir out
  (content, st) <- runStateT (renderTextT builder) (BuildState path Set.empty)
  writeFile' out (TL.toStrict content)
  pure st


-- Base HTML

-- | Build a @<title>@.
buildTitle :: Text -> Builder ()
buildTitle title = do
  title_ $ toHtml (siteName <> " | " <> title)

-- | Build the base @<head>@.
buildBaseHead :: Builder ()
buildBaseHead = do
  meta_ [charset_ "UTF-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
  withRelative [absfile|/favicon.ico|] $ \path -> 
    link_ [rel_ "icon", type_ "image/x-icon", href_ path]
  withRelative [absfile|/static/fonts.css|] $ \path -> 
    link_ [rel_ "stylesheet", href_ path]
  withRelative [absfile|/static/style.css|] $ \path -> 
    link_ [rel_ "stylesheet", href_ path]

-- | Build the base @<header>@ and @<nav>@.
buildBaseHeader :: Builder ()
buildBaseHeader = do
  header_ [class_ "main"] $ do
    nav_ $ do
      ul_ $ do
        li_ $ withRelative [absfile|/index.html|] $ \path -> 
          a_ [href_ path] "Home"
        li_ $ withRelative [absfile|/atom.xml|] $ \path -> 
          a_ [href_ path] "Feed"
        li_ $ a_ [href_ "https://github.com/jtmcx"] "GitHub"
    span_ [class_ "mark"] (toHtml siteName)

-- | Build the base @<footer>@.
buildBaseFooter :: Builder ()
buildBaseFooter = do
  footer_ [class_ "main"] $ do
    span_ $ toHtmlRaw ("&copy; 2026 jtm" :: String)
    span_ $ do
      "Website "; a_ [href_ "https://github.com/jtmcx/blog"] "source"; " licensed under "
      a_ [href_ "https://github.com/jtmcx/blog/tree/master/LICENSE"] "ISC"; "."
    span_ $ do
      "Content licensed under "
      a_ [href_ "https://creativecommons.org/licenses/by-sa/4.0/"] "CC-BY-SA"; "."

-- Post HTML

-- | Build OpenGraph metadata for this post. See https://ogp.me/
buildPostMeta :: PostMeta -> Builder ()
buildPostMeta meta = do
    prop "og:site_name" siteName
    prop "og:type" "article"
    prop "og:title" (postTitle meta)
    prop "og:description" `mapM_` postSummary meta
    prop "og:url" . uriText =<< pageUri
    prop "article:published_time" $ formatDay (postDate meta)
  where
    -- https://github.com/chrisdone/lucid/pull/168
    -- | The @property@ attribute.
    property_ :: Text -> Attributes
    property_ = makeAttributes "property"

    prop :: Text -> Text -> Builder ()
    prop k v = meta_ [property_ k, content_ v]

-- | Build the article @<header>@.
buildPostHeader :: PostMeta -> Builder ()
buildPostHeader meta = 
  header_ $ do
    p_ [class_ "meta"] (toHtml $ "Published · " ++ date)
    h1_ [class_ "title"] (toHtml $ postTitle meta)
  where
    date :: String 
    date = formatTime defaultTimeLocale "%b %d %Y" (postDate meta)

buildPost :: Pandoc -> Builder ()
buildPost doc = do
  meta <- action $ parsePostMeta doc
  doctypehtml_ $ do
    head_ $ do
      buildTitle (postTitle meta)
      buildBaseHead
      buildPostMeta meta
    body_ $ do
      buildBaseHeader
      main_ $ do
        article_ $ do
          buildPostHeader meta
          (toHtmlRaw =<< action (documentHtml doc))
      p_ [class_ "back-to-top"] $
        a_ [href_ "#"] "↑ Back to top ↑"
      buildBaseFooter

-- Home HTML

buildAvatar :: Builder ()
buildAvatar = do
  div_ [class_ "avatar"] $ do
    withRelative [absfile|/static/images/jtm.gif|] $ \path ->
      -- todo: determine image size automatically?
      img_ [src_ path, width_ "155", height_ "159", alt_ "A drawing of myself"]

-- | Construct the home page bio.
buildHomeBio :: Builder ()
buildHomeBio = do
  bio <- action $ readMarkdown [relfile|partials/bio.md|] >>= documentHtml 
  div_ [class_ "bio"] $ toHtmlRaw bio

-- | Construct the list of blog entries on the home page.
buildPostList :: Builder ()
buildPostList = do
  posts <- action readAllPostMetas
  section_ $ do
    h2_ "Posts"
    case posts of
      [] -> 
        p_ [class_ "empty-post-list"] "(Nothing here yet!)"
      _  -> do
        ul_ [class_ "post-list"] $ do
          let sorted = sortOn (Down . postDate . snd) posts
          mconcat $ map (uncurry buildPostListEntry) sorted

-- | Construct an individual blog entry on the home page.
buildPostListEntry :: Path Rel File -> PostMeta -> Builder ()
buildPostListEntry src meta = do
  li_ [class_ "post-entry"] $ do
    span_ [class_ "post-date"] (toHtml date)
    span_ [class_ "post-title"] link
  where
    link = do
      file <- action $ exnFail $ [absdir|/|] </$ (src -<.> ".html")
      withRelative file $ \path ->
        a_ [href_ path] (toHtml (postTitle meta))

    date = formatTime defaultTimeLocale "%b %d %Y" (postDate meta)

buildHome :: Builder ()
buildHome = do
  doctypehtml_ $ do
    head_ $ do
      buildTitle "Home"
      buildBaseHead 
      withRelative [absfile|/static/home.css|] $ \path ->
        link_ [rel_ "stylesheet", href_ path]
    body_ $ do
      buildBaseHeader 
      main_ $ do
        div_ [class_ "about"] $ do
          buildAvatar
          buildHomeBio
        buildPostList
      buildBaseFooter


-- Shake rules
-----------------------------------------------------------------------

-- | Prefix a file pattern with the site output directory.
sitePattern :: FilePattern -> FilePattern
sitePattern pat = (toFilePath htmlDir FilePath.</> pat)

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = toFilePath shakeDir} $ runShakePlus () $ do
  want ["all"]

  phony "all" $ do
    need [sitePattern "index.html"]
    need [sitePattern "atom.xml"]
    need [sitePattern "sitemap.xml"]
    need [sitePattern "favicon.ico"]

    -- Copy everything in static.
    files <- getDirectoryFiles [reldir|.|] ["static//*"]
    needP $ map (htmlDir </>) files
    -- Build all the posts.
    posts <- getDirectoryFiles [reldir|.|] ["posts/*.md"]
    needP =<< mapM ((htmlDir </$) . (-<.> ".html")) posts

  sitePattern "index.html" %> \out -> liftAction $ do
    _ <- runBuilder out buildHome
    putInfo $ "Generated " ++ (toFilePath out)

  -- Generate a post from a markdown file
  sitePattern "posts/*.html" %> \out -> liftAction $ do
    src <- exnFail $ stripProperPrefix htmlDir =<< out -<.> ".md"
    doc <- readMarkdown src
    _ <- runBuilder out $ buildPost doc
    putInfo $ "Generated " ++ (toFilePath out)

  sitePattern "atom.xml" %> \out -> liftAction $ do
    posts <- readAllPostMetas
    case Atom.textFeed (atomFeed (map snd posts)) of
      Just xml -> do
        writeFile' out (TL.toStrict xml)
        putInfo $ "Generated " ++ (toFilePath out)
      Nothing -> fail "Failed to generate Atom feed"

  sitePattern "sitemap.xml" %> \out -> liftAction $ do
    posts <- readAllPostMetas
    xml <- renderSitemap <$> sitemap posts
    writeFile' out (decodeUtf8 xml)
    putInfo $ "Generated " ++ (toFilePath out)

  sitePattern "static//*" %> \out -> do
    src <- stripProperPrefix htmlDir out
    copyFileChanged src out
    liftAction $ putInfo $ "Copied " ++ (toFilePath out)

  sitePattern "favicon.ico" %> \out -> do
    copyFileChanged [relfile|static/favicon.ico|] out
    liftAction $ putInfo $ "Copied " ++ (toFilePath out)