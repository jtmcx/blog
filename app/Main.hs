module Main (main) where

import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (..), gets, modify)
import Data.Functor (($>))
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (Down))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time (Day, UTCTime (..), fromGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Development.Shake hiding (action)
import qualified Development.Shake.FilePath as FilePath
import Lucid
import Lucid.Base (makeAttributes)
import Main.Path
import Main.Path.Extra
import Network.URI (URI (..), escapeURIString, isUnreserved, nullURI, parseURIReference, unEscapeString, uriIsRelative, uriToString)
import qualified Network.URI as URI
import Network.URI.Static (uri)
import Text.Atom.Feed (Entry (..), Feed (..), TextContent (..))
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Atom
import Text.Pandoc (Meta, MetaValue (..), enableExtension, nullMeta, pandocExtensions)
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Class (PandocPure, runPure)
import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc (..))
import Text.Pandoc.Highlighting (pygments)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (walkM)
import Web.Sitemap.Gen (Sitemap (..), SitemapUrl (..), renderSitemap)

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

-- | The directory to place the internal link cache.
linksDir :: Path Rel Dir
linksDir = shakeDir </> [reldir|links|]

-- Utilities
-----------------------------------------------------------------------

-- | Parse a date in YYYY-MM-DD format.
parseDay :: (MonadFail m) => Text -> m Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack

-- | Format a date in YYYY-MM-DD format.
formatDay :: Day -> Text
formatDay = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

-- | A dummy date.
epochDay :: Day
epochDay = fromGregorian 1970 01 01

-- | Convert a URI to a string.
uriString :: URI -> String
uriString x = uriToString id x ""

-- | Convert a URI to text.
uriText :: URI -> Text
uriText = T.pack . uriString

-- | URI-escape a given 'FilePath'.
escapeFilePath :: FilePath -> String
escapeFilePath = escapeURIString (\c -> isUnreserved c || c == '/')

-- | URI-escape a given 'Path'.
escapePath :: Path b t -> String
escapePath p = escapeFilePath (toFilePath p)

-- | Convert a 'FilePath' to a relative 'URI'
filePathToUri :: FilePath -> URI
filePathToUri p = nullURI {uriPath = escapeFilePath p}

-- | Convert a 'Path' to a relative 'URI'
pathToUri :: Path b t -> URI
pathToUri p = nullURI {uriPath = escapePath p}

-- | Treat a relative path as if it's absolute.
asAbsolute :: Path Rel t -> Path Abs t
asAbsolute p = [absdir|/|] </> p

-- | Treat an absolute path as if it's relative.
asRelative :: Path Abs t -> Path Rel t
asRelative p = fromMaybe beleiveMe $ stripProperPrefix [absdir|/|] p
  where
    beleiveMe = error "internal error: 'stripProperPreifx /' failed"

-- | Re-root an absolute path on to a given base directory.
rootTo :: Path Abs t -> Path b Dir -> Path b t
rootTo p newRoot = newRoot </> asRelative p

-- Post Front-matter
-----------------------------------------------------------------------

-- | Post front-matter.
data PostMeta = PostMeta
  { -- | The title of the post.
    postTitle :: [Inline],
    -- | Summary of the article contents. Optional.
    postSummary :: Maybe [Block],
    -- | The day the article was published.
    postDate :: Day,
    -- | The date of the most recent update.
    postUpdated :: Maybe Day
  }

-- | The day of the most recent update.
lastUpdate :: PostMeta -> Day
lastUpdate meta = fromMaybe (postDate meta) (postUpdated meta)

-- | Get the post title as plain text.
postTitleText :: PostMeta -> Text
postTitleText = stringify . postTitle

-- | Get the post title as HTML.
postTitleHtml :: (MonadFail m) => PostMeta -> m Text
postTitleHtml = writeHtml . inlinesToDoc . postTitle
  where
    writeHtml = runPandoc . Pandoc.writeHtml5String Pandoc.def

-- | Get the post summary as plain text.
postSummaryText :: PostMeta -> Maybe Text
postSummaryText meta = stringify <$> postSummary meta

-- | Get the post summary as HTML.
postSummaryHtml :: (MonadFail m) => PostMeta -> m (Maybe Text)
postSummaryHtml = mapM (writeHtml . Pandoc nullMeta) . postSummary
  where
    writeHtml = runPandoc . Pandoc.writeHtml5String Pandoc.def

-- | Convert some inline text to a document.
inlinesToDoc :: [Inline] -> Pandoc
inlinesToDoc x = Pandoc nullMeta [Plain x]

-- | Parse a meta value as text.
metaToText :: (MonadFail m) => MetaValue -> m Text
metaToText = \case
  MetaString s -> pure s
  MetaInlines x -> pure (stringify x)
  _ -> fail $ "meta value is not text"

-- | Parse a meta value as inline text.
metaToInlines :: (MonadFail m) => MetaValue -> m [Inline]
metaToInlines = \case
  MetaInlines x -> pure x
  MetaString s -> pure [Pandoc.Str s]
  _ -> fail $ "meta value is not inline text"

-- | Parse a meta value as inline text.
metaToBlocks :: (MonadFail m) => MetaValue -> m [Block]
metaToBlocks = \case
  MetaInlines x -> pure [Para x]
  MetaString s -> pure [Para [Pandoc.Str s]]
  MetaBlocks x -> pure x
  _ -> fail $ "meta value is not block text"

parseMetaTitle :: (MonadFail m) => Meta -> m [Inline]
parseMetaTitle meta = do
  case Pandoc.lookupMeta "title" meta of
    Just val -> metaToInlines val
    Nothing -> pure [Pandoc.Str "Untitled"]

parseMetaSummary :: (MonadFail m) => Meta -> m (Maybe [Block])
parseMetaSummary meta = do
  case Pandoc.lookupMeta "summary" meta of
    Just val -> Just <$> metaToBlocks val
    Nothing -> pure Nothing

parseMetaDate :: (MonadFail m) => Meta -> m Day
parseMetaDate meta = do
  case Pandoc.lookupMeta "date" meta of
    Just val -> metaToText val >>= parseDay
    Nothing -> pure epochDay

parseMetaUpdated :: (MonadFail m) => Meta -> m (Maybe Day)
parseMetaUpdated meta = do
  case Pandoc.lookupMeta "updated" meta of
    Just val -> parseDay <$> metaToText val
    Nothing -> pure Nothing

-- | Parse a post's front-matter.
parsePostMeta :: (MonadFail m) => Pandoc -> m PostMeta
parsePostMeta (Pandoc meta _) = do
  title <- parseMetaTitle meta
  summary <- parseMetaSummary meta
  date <- parseMetaDate meta
  updated <- parseMetaUpdated meta
  pure $
    PostMeta
      { postTitle = title,
        postSummary = summary,
        postDate = date,
        postUpdated = updated
      }

-- Atom Feed
-----------------------------------------------------------------------

-- | An empty Atom feed.
emptyFeed :: Feed
emptyFeed = Atom.nullFeed "" (TextString "") ""

-- | An empty Atom feed entry.
emptyEntry :: Entry
emptyEntry = Atom.nullEntry "" (TextString "") ""

-- | Contruct an Atom feed from a list of posts.
atomFeed :: (MonadFail m) => [(Path Rel File, PostMeta)] -> m Feed
atomFeed posts = do
  entries <- mapM (uncurry atomPostEntry) posts
  pure $
    emptyFeed
      { feedId = uriText $ permalink [absdir|/posts|],
        feedTitle = TextString siteName,
        feedUpdated = formatDay mostRecentUpdate,
        feedEntries = entries,
        feedIcon = Just (uriText $ permalink [absfile|/favicon.ico|])
      }
  where
    mostRecentUpdate :: Day
    mostRecentUpdate =
      case sortOn Down $ map (lastUpdate . snd) posts of
        day : _ -> day
        [] -> epochDay -- No posts; use dummy 1970-01-01.

-- | Contruct an Atom feed entry for a given post.
atomPostEntry :: (MonadFail m) => Path Rel File -> PostMeta -> m Entry
atomPostEntry src meta = do
  title <- postTitleHtml meta
  summary <- postSummaryHtml meta
  let url = uriText $ permalink (asAbsolute (src -<.>! ".html"))
  pure $
    emptyEntry
      { entryId = url,
        entryTitle = HTMLString title,
        entryUpdated = formatDay (lastUpdate meta),
        entryPublished = Just $ formatDay (postDate meta),
        entrySummary = HTMLString <$> summary,
        entryLinks = [Atom.nullLink url]
      }

-- Sitemap
-----------------------------------------------------------------------

-- | Midnight UTC on the given day.
dayToUTCTime :: Day -> UTCTime
dayToUTCTime day = UTCTime day 0

-- | The full sitemap for the home page and all posts.
sitemap :: [(Path Rel File, PostMeta)] -> Sitemap
sitemap posts = do
  Sitemap (homeEntry : map (uncurry postEntry) posts)
  where
    homeEntry :: SitemapUrl
    homeEntry =
      SitemapUrl
        { sitemapLocation = uriText $ permalink [absfile|/index.html|],
          sitemapLastModified = Nothing,
          sitemapChangeFrequency = Nothing,
          sitemapPriority = Nothing
        }

    postEntry :: Path Rel File -> PostMeta -> SitemapUrl
    postEntry src meta =
      SitemapUrl
        { sitemapLocation = uriText $ permalink (asAbsolute (src -<.>! ".html")),
          sitemapLastModified = Just $ dayToUTCTime (lastUpdate meta),
          sitemapChangeFrequency = Nothing,
          sitemapPriority = Nothing
        }

-- Actions
-----------------------------------------------------------------------

-- | Run a pandoc monad as an action. Fails on error.
runPandoc :: (MonadFail m) => PandocPure a -> m a
runPandoc m = do
  case runPure m of
    Right x -> pure x
    Left err -> fail ("pandoc: " ++ show err)

-- | Read and parse a markdown file.
readMarkdown :: Path a File -> Action Pandoc
readMarkdown path = do
  contents <- readFile' (toFilePath path)
  runPandoc $ Pandoc.readMarkdown readerOptions (T.pack contents)
  where
    readerOptions :: Pandoc.ReaderOptions
    readerOptions =
      Pandoc.def
        { Pandoc.readerExtensions =
            -- Enable extension to read yaml front-matter.
            enableExtension Pandoc.Ext_yaml_metadata_block pandocExtensions
        }

-- | Convert a pandoc document to an html string.
docToHtml :: Pandoc -> Action Text
docToHtml doc = runPandoc $ Pandoc.writeHtml5String writerOptions doc
  where
    writerOptions :: Pandoc.WriterOptions
    writerOptions =
      Pandoc.def
        { Pandoc.writerHighlightStyle = Just pygments
        }

-- | Read and parse the front-matter of a post.
readPostMeta :: Path a File -> Action PostMeta
readPostMeta path = readMarkdown path >>= parsePostMeta

-- | Read and parse the front-matter for all posts.
readAllPostMetas :: Action [(Path Rel File, PostMeta)]
readAllPostMetas = do
  posts <- getDirectoryFilesP "" ["posts/*.md"]
  need $ map toFilePath posts
  zip posts <$> mapM readPostMeta posts

-- | Read a page's tracked internal links from a file.
readLinks :: FilePath -> Action [URI]
readLinks path = do
  contents <- readFile' path
  mapM parseLine (lines contents)
  where
    parseLine :: String -> Action URI
    parseLine x =
      case parseURIReference x of
        Just url -> pure url
        Nothing -> fail $ "failed to parse uri: " ++ x

-- | Write a page's tracked internal links to a file.
writeLinks :: FilePath -> [URI] -> Action ()
writeLinks out links =
  writeFile' out $ unlines (map uriString links)

-- HTML Builders
-----------------------------------------------------------------------

type SitePath t = Path Abs t

-- | The HTML builder monad.
type Builder = HtmlT (StateT BuildState Action)

data BuildState = BuildState
  { -- | The target location of the page we're building.
    pagePath :: SitePath File,
    -- | Collected set of links in the page.
    trackedLinks :: [URI]
  }

-- | Run a shake action in a builder.
action :: Action a -> Builder a
action = lift . lift

-- | Get the target location of the current page.
getPagePath :: Builder (SitePath File)
getPagePath = lift $ gets pagePath

-- | Get the target directory of the current page.
getPageDir :: Builder (SitePath Dir)
getPageDir = parent <$> getPagePath

-- | Convert an internal path to an absolute URI.
permalink :: SitePath a -> URI
permalink path = pathToUri path `URI.relativeTo` baseUri

-- | Return the absolute URI for this page.
pagePermalink :: Builder URI
pagePermalink = permalink <$> getPagePath

-- | Track a link to another page.
trackLink :: URI -> Builder ()
trackLink x = lift $ modify $ \s ->
  s {trackedLinks = x : trackedLinks s}

-- | todo: explain ...
--
-- >>> resolveSiteFile [absdir|/posts/|] [uri|../static/cat.gif]
-- Just "/static/cat.gif"
-- >>> resolveSiteFile [absdir|/posts/|] [uri|other.md#heading]
-- Just "/posts/other.md"
-- >>> resolveSiteFile [absdir|/posts/|] [uri|https//example.com]
-- Nothing
resolveSiteFile :: SitePath Dir -> URI -> Maybe (SitePath File)
resolveSiteFile cwd url = do
  guard (uriIsRelative url)
  case unEscapeString (uriPath url) of
    p@('/' : _) -> parseAbsFile p
    p -> resolveAgainst cwd p

-- | Rewrite all links in a Pandoc document monadically.
rewriteLinksM :: (Monad m) => (Text -> m Text) -> Pandoc -> m Pandoc
rewriteLinksM f = walkM $ \case
  Link attr content (url, title) -> do
    url' <- f url
    pure (Link attr content (url', title))
  Image attr content (url, title) -> do
    url' <- f url
    pure (Image attr content (url', title))
  inline -> pure inline

-- | Resolve an internal link inside a document, relative to a given
-- working directory.
--
-- >>> resolveDocumentLink [absdir|/posts/|] [uri|other.md#heading]
-- Just "/posts/other.html"
resolveDocumentLink :: SitePath Dir -> URI -> Maybe (SitePath File)
resolveDocumentLink cwd url = do
  p <- resolveSiteFile cwd url
  case fileExtension p of
    -- todo: should probably be a little more careful about matching the
    -- shake rules, so that we transform file extensions in the right
    -- places (e.g. restrict '.md' -> '.html' to '/posts', not '/static').
    Just ".md" -> p -<.> ".html"
    _ -> Just p

-- | Make all internal links relative and track them.
processDocumentLinks :: Pandoc -> Builder Pandoc
processDocumentLinks = rewriteLinksM (resolve . T.unpack)
  where
    resolve :: String -> Builder Text
    resolve text = do
      case parseURIReference text of
        Just url -> do
          cwd <- getPageDir
          case resolveDocumentLink cwd url of
            Just internal -> withRelative internal pure
            Nothing -> trackLink url $> uriText url
        Nothing ->
          action $ fail $ "Failed to parse uri: " ++ text

-- | Calculate the path relative to the current page.
withRelative :: SitePath File -> (Text -> Builder a) -> Builder a
withRelative target f = do
  dir <- getPageDir
  let rel = relativeFile dir target
  trackLink (filePathToUri rel)
  f (T.pack $ relativeFile dir target)

-- | Generate HTML and write it to a file.
runBuilder :: Path Rel File -> Builder () -> Action BuildState
runBuilder out builder = do
  path <- replaceProperPrefix htmlDir root out
  (content, st) <- runStateT (renderTextT builder) (BuildState path [])
  writeFile' (toFilePath out) (TL.unpack content)
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
        li_ $ withRelative [absfile|/feed.atom|] $ \path ->
          a_ [href_ path] "Feed"
        li_ $ a_ [href_ "https://github.com/jtmcx"] "GitHub"
    span_ [class_ "mark"] (toHtml siteName)

-- | Build the base @<footer>@.
buildBaseFooter :: Builder ()
buildBaseFooter = do
  footer_ [class_ "main"] $ do
    span_ $ toHtmlRaw ("&copy; 2026 jtm" :: String)
    span_ $ do
      "Website "
      a_ [href_ "https://github.com/jtmcx/blog"] "source"
      " licensed under "
      a_ [href_ "https://github.com/jtmcx/blog/tree/master/LICENSE"] "ISC"
      "."
    span_ $ do
      "Content licensed under "
      a_ [href_ "https://creativecommons.org/licenses/by-sa/4.0/"] "CC-BY-SA"
      "."

-- Post HTML

-- | Build OpenGraph metadata for this post. See https://ogp.me/
buildPostMeta :: PostMeta -> Builder ()
buildPostMeta meta = do
  prop "og:site_name" siteName
  prop "og:type" "article"
  prop "og:title" (postTitleText meta)
  prop "og:description" `mapM_` postSummaryText meta
  prop "og:url" . uriText =<< pagePermalink
  prop "article:published_time" $ formatDay (postDate meta)
  where
    -- https://github.com/chrisdone/lucid/pull/168
    -- \| The @property@ attribute.
    property_ :: Text -> Attributes
    property_ = makeAttributes "property"

    prop :: Text -> Text -> Builder ()
    prop k v = meta_ [property_ k, content_ v]

-- | Build the article @<header>@.
buildPostHeader :: PostMeta -> Builder ()
buildPostHeader meta = do
  titleHtml <- action $ postTitleHtml meta
  header_ $ do
    p_ [class_ "meta"] (toHtml $ "Published · " ++ date)
    h1_ [class_ "title"] (toHtmlRaw titleHtml)
  where
    date :: String
    date = formatTime defaultTimeLocale "%b %d %Y" (postDate meta)

buildPost :: Pandoc -> Builder ()
buildPost unprocessedDoc = do
  doc <- processDocumentLinks unprocessedDoc
  meta <- action $ parsePostMeta doc
  doctypehtml_ $ do
    head_ $ do
      buildTitle (postTitleText meta)
      buildBaseHead
      buildPostMeta meta
    body_ $ do
      buildBaseHeader
      main_ $ do
        article_ $ do
          buildPostHeader meta
          (toHtmlRaw =<< action (docToHtml doc))
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
  bio <- action $ readMarkdown [relfile|partials/bio.md|] >>= docToHtml
  div_ [class_ "bio"] $ toHtmlRaw bio

-- | Construct the list of blog entries on the home page.
buildPostList :: Builder ()
buildPostList = do
  posts <- action readAllPostMetas
  section_ $ do
    h2_ "Recent Posts"
    case posts of
      [] ->
        p_ [class_ "empty-post-list"] "(Nothing here yet!)"
      _ -> do
        ul_ [class_ "post-list"] $ do
          let sorted = sortOn (Down . postDate . snd) posts
          mconcat $ map (uncurry buildPostListEntry) sorted

-- | Construct the list of blog entries on the home page.
buildProjectList :: Builder ()
buildProjectList = do
  h2_ "Personal Projects"
  html <- action $ readMarkdown [relfile|partials/projects.md|] >>= docToHtml
  div_ [class_ "projects"] $ toHtmlRaw html

-- | Construct an individual blog entry on the home page.
buildPostListEntry :: Path Rel File -> PostMeta -> Builder ()
buildPostListEntry src meta = do
  li_ [class_ "post-entry"] $ do
    span_ [class_ "post-date"] (toHtml date)
    span_ [class_ "post-title"] link
  where
    link = do
      file <- action $ root </$> (src -<.> ".html")
      withRelative file $ \path -> do
        titleHtml <- action $ postTitleHtml meta
        a_ [href_ path] (toHtmlRaw titleHtml)

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
        buildProjectList
      buildBaseFooter

-- Shake rules
-----------------------------------------------------------------------

-- | Prefix 'FilePattern's with a 'Dir'.
(</?>) :: Path a Dir -> FilePattern -> FilePattern
p </?> q = (toFilePath p) FilePath.</> q

-- | Any file in the 'posts' directory that's not an .html file.
-- These files are copied from posts to _site/posts/ if they're
-- referenced within a document.
postAssetPattern :: FilePath -> Bool
postAssetPattern out =
  (htmlDir </?> "posts//*") ?== out
    && FilePath.takeExtension out /= ".html"

-- | Same as 'getDirectoryFiles', but return a 'Path' list.
getDirectoryFilesP :: FilePath -> [FilePattern] -> Action [Path Rel File]
getDirectoryFilesP dir ps = getDirectoryFiles dir ps >>= mapM parseRelFile

-- | Add a shake dependency on site pages.
needSiteFiles :: [SitePath t] -> Action ()
needSiteFiles fs = need $ map (toFilePath . (`rootTo` htmlDir)) fs

-- | Add a shake dependency on a site page.
needSiteFile :: SitePath File -> Action ()
needSiteFile f = needSiteFiles [f]

-- | Add dependencies on all internal link references.
needLinkDependencies :: SitePath File -> Action ()
needLinkDependencies file = do
  linkFile <- addExtension ".links" (file `rootTo` linksDir)
  links <- readLinks (toFilePath linkFile)
  needSiteFiles $ mapMaybe (resolveSiteFile (parent file)) links

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = toFilePath shakeDir} $ do
  want ["all"]

  phony "all" $ do
    needSiteFile [absfile|/index.html|]
    needLinkDependencies [absfile|/index.html|]
    needSiteFile [absfile|/feed.atom|]
    needSiteFile [absfile|/sitemap.xml|]
    needSiteFile [absfile|/favicon.ico|]
    need ["all-static", "all-posts"]

  phony "all-static" $ do
    sources <- getDirectoryFilesP "" ["static//*"]
    let targets = map asAbsolute sources
    needSiteFiles targets

  phony "all-posts" $ do
    sources <- getDirectoryFilesP "" ["posts/*.md"]
    targets <- mapM (\f -> asAbsolute <$> (f -<.> ".html")) sources
    needSiteFiles targets
    mapM_ needLinkDependencies targets

  [ htmlDir </?> "index.html",
    linksDir </?> "index.html.links"
    ]
    &%> \case
      [htmlOut, linksOut] -> do
        out <- parseRelFile htmlOut
        st <- runBuilder out buildHome
        putInfo $ "Generated " ++ htmlOut
        writeLinks linksOut (trackedLinks st)
        putInfo $ "Generated " ++ linksOut
      _ -> undefined

  [ htmlDir </?> "posts/*.html",
    linksDir </?> "posts/*.html.links"
    ]
    &%> \case
      [htmlOut, linksOut] -> do
        out <- parseRelFile htmlOut
        src <- out -<.> ".md" >>= stripProperPrefix htmlDir
        doc <- readMarkdown src
        st <- runBuilder out $ buildPost doc
        putInfo $ "Generated " ++ htmlOut
        writeLinks linksOut (trackedLinks st)
        putInfo $ "Generated " ++ linksOut
      _ -> undefined

  htmlDir </?> "feed.atom" %> \out -> do
    posts <- readAllPostMetas
    feed <- atomFeed posts
    case Atom.textFeed feed of
      Just xml -> do
        writeFile' out (TL.unpack xml)
        putInfo $ "Generated " ++ out
      Nothing -> fail "Failed to generate Atom feed"

  htmlDir </?> "sitemap.xml" %> \out -> do
    posts <- readAllPostMetas
    let xml = decodeUtf8 (renderSitemap (sitemap posts))
    writeFile' out (TL.unpack xml)
    putInfo $ "Generated " ++ out

  htmlDir </?> "static//*" %> \out -> do
    src <- parseRelFile out >>= stripProperPrefix htmlDir
    copyFileChanged (toFilePath src) out
    putInfo $ "Copied " ++ out

  postAssetPattern ?> \out -> do
    src <- parseRelFile out >>= stripProperPrefix htmlDir
    copyFileChanged (toFilePath src) out
    putInfo $ "Copied " ++ out

  htmlDir </?> "favicon.ico" %> \out -> do
    copyFileChanged "static/favicon.ico" out
    putInfo $ "Copied " ++ out
