module Main (main) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (..), gets, modify)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time (Day, UTCTime (..), fromGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUIDV5
import Development.Shake hiding (action)
import qualified Development.Shake.FilePath as FilePath
import Lucid
import Lucid.Base (makeAttributes)
import Main.Path
import Main.Path.Extra
import Network.URI (URI (..), escapeURIString, isUnescapedInURIComponent, parseRelativeReference, uriToString)
import Network.URI.Static (uri)
import Text.Atom.Feed (Entry (..), Feed (..), TextContent (..))
import qualified Text.Atom.Feed.Export as Atom
import Text.Pandoc (enableExtension, pandocExtensions)
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Class (PandocPure, runPure)
import Text.Pandoc.Definition (Inline (..), Pandoc (..), lookupMeta)
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

-- | Parse a UUID.
parseUuid :: (MonadFail m) => Text -> m UUID
parseUuid s =
  case UUID.fromText s of
    Just uuid -> pure uuid
    Nothing -> fail $ "invalid uuid: " ++ T.unpack s

-- | URI-escape a given 'PagePath'.
escapeAbsPath :: Path Abs t -> Text
escapeAbsPath path =
  let segments = drop 1 (FilePath.splitDirectories (toFilePath path))
   in "/" <> T.intercalate "/" (map escapeSegment segments)
  where
    escapeSegment = T.pack . escapeURIString isUnescapedInURIComponent

-- | Convert a URI to a string.
uriString :: URI -> String
uriString x = uriToString id x ""

-- | Convert a URI to text.
uriText :: URI -> Text
uriText = T.pack . uriString

-- | Generate the fully-qualified URI to a given page.
-- todo: use relativeTo?
qualifyWith :: URI -> Path Abs t -> URI
qualifyWith base path = base {uriPath = T.unpack (escapeAbsPath path)}

-- Post Front-matter
-----------------------------------------------------------------------

-- | Post front-matter.
data PostMeta = PostMeta
  { -- | The title of the post.
    postTitle :: Text,
    -- | Summary of the article contents. Optional.
    postSummary :: Maybe Text,
    -- | The day the article was published.
    postDate :: Day,
    -- | The date of the most recent update.
    postUpdated :: Maybe Day,
    -- | Every post has a unique identifier. This identifier is permanent.
    -- It's used to identify the post in Atom feeds, and allows us to
    -- identify posts even if the title, url, or contents change.
    --
    -- This field is optional. If it's not provided, the UUID will be
    -- calculated deterministically using the title, publish date, and
    -- the global Atom feed id. If the title needs to be updated for a
    -- published article, then the generated Uuid *must* be copied into
    -- the post's metadata in order to preserve it.
    postUuid :: Maybe UUID
  }

-- | Parse a post's front-matter.
parsePostMeta :: (MonadFail m) => Pandoc -> m PostMeta
parsePostMeta (Pandoc meta _) = do
  title <- pure $ field "title"
  summary <- pure $ field "summary"
  date <- mapM parseDay (field "date")
  updated <- mapM parseDay (field "updated")
  uuid <- mapM parseUuid (field "uuid")
  pure $
    PostMeta
      { postTitle = title `orElse` "Untitled",
        postSummary = summary,
        postDate = date `orElse` epochDay,
        postUpdated = updated,
        postUuid = uuid
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
  let bytes = map (fromIntegral . fromEnum) (T.unpack siteName)
   in UUIDV5.generateNamed UUIDV5.namespaceDNS bytes

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
    bytes =
      map (fromIntegral . fromEnum) $
        T.unpack $
          formatDay (postDate meta) <> postTitle meta

atomFeed :: [PostMeta] -> Feed
atomFeed posts =
  Feed
    { feedId = "urn:uuid:" <> UUID.toText atomFeedId,
      feedTitle = TextString siteName,
      feedUpdated = formatDay mostRecentUpdate,
      feedAuthors = [],
      feedCategories = [],
      feedContributors = [],
      feedGenerator = Nothing,
      feedIcon = Nothing,
      feedLinks = [],
      feedLogo = Nothing,
      feedRights = Nothing,
      feedSubtitle = Nothing,
      feedEntries = map atomPostEntry posts,
      feedAttrs = [],
      feedOther = []
    }
  where
    mostRecentUpdate :: Day
    mostRecentUpdate =
      case sortOn Down $ map lastUpdate posts of
        day : _ -> day
        [] -> epochDay -- No posts; use dummy 1970-01-01.

atomPostEntry :: PostMeta -> Entry
atomPostEntry meta =
  Entry
    { entryId = "urn:uuid:" <> UUID.toText (atomEntryId meta),
      entryTitle = TextString $ postTitle meta,
      entryUpdated = formatDay (lastUpdate meta),
      entryAuthors = [],
      entryCategories = [],
      entryContent = Nothing,
      entryContributor = [],
      entryLinks = [],
      entryPublished = Just $ formatDay (postDate meta),
      entryRights = Nothing,
      entrySource = Nothing,
      entrySummary = TextString <$> postSummary meta,
      entryInReplyTo = Nothing,
      entryInReplyTotal = Nothing,
      entryAttrs = [],
      entryOther = []
    }

-- Sitemap
-----------------------------------------------------------------------

-- | Midnight UTC on the given day.
dayToUTCTime :: Day -> UTCTime
dayToUTCTime day = UTCTime day 0

-- | The sitemap entry for a given page.
sitemapUrl :: URI -> Maybe Day -> SitemapUrl
sitemapUrl target updated =
  SitemapUrl
    { sitemapLocation = uriText target,
      sitemapLastModified = dayToUTCTime <$> updated,
      sitemapChangeFrequency = Nothing,
      sitemapPriority = Nothing
    }

-- | The sitemap entry for a given post.
sitemapPostUrl :: (Path Rel File, PostMeta) -> Action SitemapUrl
sitemapPostUrl (src, meta) = do
  file <- root </$> (src -<.> ".html")
  pure $ sitemapUrl (qualifyWith baseUri file) (Just (lastUpdate meta))

-- | The full sitemap for the home page and all posts.
sitemap :: [(Path Rel File, PostMeta)] -> Action Sitemap
sitemap posts = do
  postUrls <- mapM sitemapPostUrl posts
  let homeUrl = sitemapUrl (qualifyWith baseUri [absfile|/index.html|]) Nothing
  pure $ Sitemap (homeUrl : postUrls)

-- Actions
-----------------------------------------------------------------------

-- | Run a pandoc monad as an action. Fails on error.
runPandoc :: PandocPure a -> Action a
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
documentHtml :: Pandoc -> Action Text
documentHtml doc = runPandoc $ Pandoc.writeHtml5String writerOptions doc
  where
    writerOptions :: Pandoc.WriterOptions
    writerOptions =
      Pandoc.def
        { Pandoc.writerHighlightStyle = Just pygments
        }

-- | Read and parse the front-matter of a post.
readPostMeta :: Path a File -> Action PostMeta
readPostMeta path = readMarkdown path >>= parsePostMeta

-- | Read and parse the front-matter for all posts. Note that posts
-- are not sorted in any way.
readAllPostMetas :: Action [(Path Rel File, PostMeta)]
readAllPostMetas = do
  posts <- getDirectoryFiles "" ["posts/*.md"] >>= mapM parseRelFile
  need $ map toFilePath posts
  zip posts <$> mapM readPostMeta posts

-- | Read a page's tracked internal links from a file.
readLinks :: FilePath -> Action (Set (Path Abs File))
readLinks path = do
  contents <- readFile' path
  Set.fromList <$> mapM parseAbsFile (lines contents)

-- | Write a page's tracked internal links to a file.
writeLinks :: FilePath -> Set (Path Abs File) -> Action ()
writeLinks out links = writeFile' out content
  where
    content = unlines (map toFilePath (Set.toList links))

-- HTML Builders
-----------------------------------------------------------------------

-- | The HTML builder monad.
type Builder = HtmlT (StateT BuildState Action)

data BuildState = BuildState
  { -- | The target location of the page we're building.
    pagePath :: Path Abs File,
    -- | Collected set of links to internal pages.
    pageInternalLinks :: Set (Path Abs File)
  }

-- | Run a shake action in a builder.
action :: Action a -> Builder a
action = lift . lift

-- | Get the target location of the page we're building.
getPagePath :: Builder (Path Abs File)
getPagePath = lift $ gets pagePath

-- | Get the target directory of the page we're building.
getPageDir :: Builder (Path Abs Dir)
getPageDir = parent <$> getPagePath

-- | Return the fully-qualified URI for this page.
getPageUri :: Builder URI
getPageUri = qualifyWith baseUri <$> getPagePath

-- | Track a link to an internal page.
trackInternalLink :: Path Abs File -> Builder ()
trackInternalLink f = lift $ modify $ \s ->
  s {pageInternalLinks = Set.insert f (pageInternalLinks s)}

-- | Resolve an internal link inside a document, relative to a given
-- directory in the site.
--
-- >>> resolveInternalLink [absdir|/posts/|] "../static/cat.gif"
-- Just "/static/cat.gif"
-- >>> resolveInternalLink [absdir|/posts/|] "other.md#heading"
-- Just "/posts/other.html"
resolveInternalLink :: Path Abs Dir -> Text -> Maybe (Path Abs File)
resolveInternalLink base link = do
  p <- resolvedPath
  case fileExtension p of
    -- todo: should probably be a little more careful about matching the
    -- shake rules, so that we transform file extensions in the right
    -- places (e.g. restrict '.md' -> '.html' to '/posts', not '/static').
    Just ".md" -> p -<.> ".html"
    _ -> Just p
  where
    resolvedPath :: Maybe (Path Abs File)
    resolvedPath =
      case uriPath <$> parseRelativeReference (T.unpack link) of
        Just p@('/' : _) -> parseAbsFile p
        Just p -> resolveAgainst base p
        Nothing -> Nothing

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

-- | Make all internal links relative and track them.
processDocumentLinks :: Pandoc -> Builder Pandoc
processDocumentLinks = rewriteLinksM resolve
  where
    resolve :: Text -> Builder Text
    resolve link = do
      dir <- getPageDir
      case resolveInternalLink dir link of
        Just internal -> do
          trackInternalLink internal
          withRelative internal pure
        Nothing -> pure link -- external link, leave untouched.

-- | Calculate the path relative to the current page.
withRelative :: Path Abs File -> (Text -> Builder a) -> Builder a
withRelative target f = do
  trackInternalLink target
  dir <- getPageDir
  f (T.pack $ relativeFile dir target)

-- | Generate HTML and write it to a file.
runBuilder :: Path Rel File -> Builder () -> Action BuildState
runBuilder out builder = do
  path <- replaceProperPrefix htmlDir root out
  (content, st) <- runStateT (renderTextT builder) (BuildState path Set.empty)
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
  prop "og:title" (postTitle meta)
  prop "og:description" `mapM_` postSummary meta
  prop "og:url" . uriText =<< getPageUri
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
buildPostHeader meta =
  header_ $ do
    p_ [class_ "meta"] (toHtml $ "Published · " ++ date)
    h1_ [class_ "title"] (toHtml $ postTitle meta)
  where
    date :: String
    date = formatTime defaultTimeLocale "%b %d %Y" (postDate meta)

buildPost :: Pandoc -> Builder ()
buildPost unprocessedDoc = do
  doc <- processDocumentLinks unprocessedDoc
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
      _ -> do
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
      file <- action $ root </$> (src -<.> ".html")
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

-- | Prefix 'FilePattern's with a 'Dir'.
(</?>) :: Path a Dir -> FilePattern -> FilePattern
p </?> q = (toFilePath p) FilePath.</> q

-- | Any file in the 'posts' directory that's not an .html file.
-- These files are copied from posts to _site/posts/ if they're
-- referenced in a document.
postAssetPattern :: FilePath -> Bool
postAssetPattern out =
  (htmlDir </?> "posts//*") ?== out
    && FilePath.takeExtension out /= ".html"

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = toFilePath shakeDir} $ do
  want ["all"]

  phony "all" $ do
    need [htmlDir </?> "index.html"]
    need [htmlDir </?> "atom.xml"]
    need [htmlDir </?> "sitemap.xml"]
    need [htmlDir </?> "favicon.ico"]

    -- Copy everything in static.
    files <- getDirectoryFiles "" ["static//*"] >>= mapM parseRelFile
    need $ map (toFilePath . (htmlDir </>)) files

    -- Build all the posts.
    posts <- getDirectoryFiles "" ["posts/*.md"] >>= mapM parseRelFile
    replacedExt <- (mapM ((-<.> ".html")) posts)
    need $ map (toFilePath . (htmlDir </>)) replacedExt

    -- Check that every internally-linked target actually gets built.
    postLinks <- mapM (addExtension ".links") replacedExt
    let linkFiles = map (toFilePath . (linksDir </>)) ([relfile|index.html.links|] : postLinks)
    need linkFiles
    links <- Set.unions <$> mapM readLinks linkFiles
    targets <- mapM (replaceProperPrefix root htmlDir) (Set.toList links)
    need $ map toFilePath targets

  [ htmlDir </?> "index.html",
    linksDir </?> "index.html.links"
    ]
    &%> \case
      [htmlOut, linksOut] -> do
        out <- parseRelFile htmlOut
        st <- runBuilder out buildHome
        putInfo $ "Generated " ++ htmlOut
        writeLinks linksOut (pageInternalLinks st)
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
        writeLinks linksOut (pageInternalLinks st)
        putInfo $ "Generated " ++ linksOut
      _ -> undefined

  htmlDir </?> "atom.xml" %> \out -> do
    posts <- readAllPostMetas
    case Atom.textFeed (atomFeed (map snd posts)) of
      Just xml -> do
        writeFile' out (TL.unpack xml)
        putInfo $ "Generated " ++ out
      Nothing -> fail "Failed to generate Atom feed"

  htmlDir </?> "sitemap.xml" %> \out -> do
    posts <- readAllPostMetas
    xml <- decodeUtf8 . renderSitemap <$> sitemap posts
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
