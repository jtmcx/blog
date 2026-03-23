package main

import (
	"encoding/xml"
	"fmt"
	"html/template"
	htmltmpl "html/template"
	"io"
	"log"
	"net/url"
	"os"
	"path"
	"path/filepath"
	texttmpl "text/template"
	"time"

	"github.com/beevik/etree"
	"github.com/google/uuid"
	"jtm.cx/src/site/tool/atom"
)

type Config struct {
	// The name of the site.
	SiteName string

	// The fully-qualified base URL of the site.
	SiteUrl *url.URL

	// Output directory for the site, relative to the repository root.
	DestDir string

	// The default author for posts.
	DefaultAuthor string

	// Options specific to atom feeds.
	Atom AtomConfig
}

type AtomConfig struct {
	// The static id of the feed.
	FeedId uuid.UUID

	// The name of the atom feed.
	FeedTitle string

	// URL path to the atom feed.
	FeedPath PagePath
}

var (
	// Global, static configuration.
	config *Config

	// Context specific to this execution
	execution *Execution

	// Cache of scanned posts.
	posts []PostEntry
)

// A PagePath is a clean, absolute, slash-delimited, URL encoded path.
// For example: "/posts/example%20post.html"
type PagePath string

// Return the path to the page on disk.
// For example: "output/posts/example post.html"
func (p PagePath) FilePath() string {
	return filepath.Join(config.DestDir, p.Localize())
}

// Return the fully qualified URL to the page.
// For example: "https://example.com/posts/example%20post.html"
func (p PagePath) QualifiedURL() *url.URL {
	return config.SiteUrl.JoinPath(string(p))
}

// Return the unescaped, os-localized path to the page.
// For example: "/posts/example post.html"
func (p PagePath) Localize() string {
	path, err := url.PathUnescape(string(p))
	if err != nil {
		log.Fatalf("Failed to localize path %s: %v", p, err)
	}
	return filepath.FromSlash(path)
}

// Return the relative path from this PagePath to the target PagePath.
// The relative path returned is slash-delimited and URL encoded.
func (p PagePath) Rel(target PagePath) (string, error) {
	source := filepath.FromSlash(path.Dir(string(p)))
	rel, err := filepath.Rel(source, filepath.FromSlash(string(target)))
	if err != nil {
		return "", err
	}
	return filepath.ToSlash(rel), nil
}

// Information common to all pages.
type Page struct {
	// The title of this page.
	Title string

	// The path to this page. For example: "/posts/example%20post.html".
	Path PagePath

	// True if the page uses KaTeX.
	HasMath bool
}

// Location of the generated page on disk.
func (p Page) FilePath() string {
	return p.Path.FilePath()
}

// Return the full qualified URL for this page.
func (p Page) QualifiedURL() *url.URL {
	return p.Path.QualifiedURL()
}

// Return the relative path to the target page.
func (p Page) Rel(target PagePath) (string, error) {
	return p.Path.Rel(target)
}

// Metadata stored at the top of each post.
type PostMeta struct {
	Title string

	// The date of initial publication. Required.
	Published time.Time

	// The date of the most recent update. Set to zero if unspecified.
	Updated time.Time

	// Summary of the article contents. Optional.
	Summary string

	// Every post has a unique identifier. This identifier is permanent.
	// It's used to identify the post in Atom feeds, and allows us to
	// identify posts even if the title, url, or contents change.
	//
	// This field is optional. If it's not provided, the UUID will be
	// calculated deterministically using the title, publish date, and
	// the global Atom feed id. If the title needs to be updated for a
	// published article, then the generated Uuid must be copied into
	// the post's metadata in order to preserve it.
	Uuid uuid.UUID
}

// High-level information about a post scanned from disk.
type PostEntry struct {
	Meta PostMeta

	// The location of the post on disk. This format is OS-specific.
	// The path is relative to the root of the source repository.
	SourcePath string
}

// Return the name of the directory
func (e PostEntry) Slug() string {
	return filepath.Base(filepath.Dir(e.SourcePath))
}

// Return the page path for the given post.
func (e PostEntry) Path() PagePath {
	return PagePath(path.Join("/posts", url.PathEscape(e.Slug()+".html")))
}

type Execution struct {
	// Time of execution
	Time time.Time
}

type TemplateData struct {
	// Page-level information.
	Page Page

	// Pointer to global, static config.
	Config *Config

	// Runtime related information.
	Exec *Execution

	// Page-specific data.
	Data any
}

func initConfig() {
	siteUrl, err := url.Parse("https://jtm.cx/")
	if err != nil {
		log.Fatalf("Failed to parse url: %v", err)
	}

	config = &Config{
		SiteName:      siteUrl.Hostname(),
		SiteUrl:       siteUrl,
		DestDir:       "output",
		DefaultAuthor: "jtm",
		Atom: AtomConfig{
			FeedId:    uuid.NewSHA1(uuid.NameSpaceDNS, []byte(siteUrl.Hostname())),
			FeedTitle: siteUrl.Hostname(),
			FeedPath:  PagePath("/feed.atom"),
		},
	}

	execution = &Execution{
		Time: time.Now(),
	}
}

func main() {
	initConfig()
	scanPosts()
	setupOutput()
	writeHomePage()
	writeSiteMap()
	writeRobotsTxt()
	writeAtomFeed()
	for _, p := range posts {
		writePost(p)
	}
}

func setupOutput() {
	// Create the output directory.
	if err := os.MkdirAll(config.DestDir, 0755); err != nil {
		log.Fatalf("Failed to create output directory: %v", err)
	}

	// Symlink to assets, instead of copying.
	assetsLink := filepath.Join(config.DestDir, "assets")
	log.Printf("Linking => %s", assetsLink)
	if _, err := os.Lstat(assetsLink); os.IsNotExist(err) {
		if err := os.Symlink("../assets", assetsLink); err != nil {
			log.Fatalf("Failed to symlink assets: %v", err)
		}
	}
}

func writeHomePage() {
	page := Page{
		Title: "Home",
		Path:  PagePath("/index.html"),
	}
	renderHtml("templates/home.html", page, posts)
}

func writeRobotsTxt() {
	page := Page{
		Path: PagePath("/robots.txt"),
	}
	renderText("templates/robots.txt", page, true)
}

func writeSiteMap() {
	page := Page{
		Path: PagePath("/sitemap.xml"),
	}
	renderText("templates/sitemap.xml", page, posts)
}

func writePost(post PostEntry) {
	out := post.Path().FilePath()

	// Read, validate, and transform the post into HTML.
	doc := etree.NewDocument()
	if err := doc.ReadFromFile(post.SourcePath); err != nil {
		log.Fatalf("Failed to generate %s: %v", out, err)
	}
	if err := ValidatePost(doc); err != nil {
		log.Fatalf("Failed to generate %s: %v", out, err)
	}
	content, err := articleHtml(doc)
	if err != nil {
		log.Fatalf("Failed to generate %s: %v", out, err)
	}

	// Render the post.
	page := Page{
		Title: post.Meta.Title,
		Path:  post.Path(),
	}
	var data struct {
		Meta    PostMeta
		Content template.HTML
	}
	data.Meta = post.Meta
	data.Content = content
	renderHtml("templates/post.html", page, data)
}

func writeAtomFeed() {
	path := config.Atom.FeedPath
	log.Printf("Generating => %s", path.FilePath())
	f := open(path.FilePath())
	defer f.Close()

	var entries []atom.Entry
	for _, p := range posts {
		entries = append(entries, atom.Entry{
			Id:        "urn:uuid:" + p.Meta.Uuid.String(),
			Title:     p.Meta.Title,
			Summary:   p.Meta.Summary,
			Published: atom.Time(p.Meta.Published),
			Updated:   atom.Time(p.Meta.Updated),
			Links: []atom.Link{
				{
					Rel:  "alternate",
					Href: p.Path().QualifiedURL().String(),
				},
			},
		})
	}
	feed := atom.Feed{
		Id:    "urn:uuid:" + config.Atom.FeedId.String(),
		Title: config.Atom.FeedTitle,
		Links: []atom.Link{
			{
				Rel:  "self",
				Href: config.Atom.FeedPath.QualifiedURL().String(),
				Type: "application/atom+xml",
			},
			{
				Rel:  "alternate",
				Href: config.SiteUrl.String(),
				Type: "text/html",
			},
		},
		Entries: entries,
	}
	if err := atom.WriteFeed(f, feed); err != nil {
		log.Fatalf("Failed to generate %s: %v", path.FilePath(), err)
	}
}

func open(path string) *os.File {
	if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
		log.Fatalf("Failed to make directory %s: %v", filepath.Dir(path), err)
	}
	f, err := os.Create(path)
	if err != nil {
		log.Fatalf("Failed to open file %s: %v", path, err)
	}
	return f
}

func renderHtml(template string, page Page, data any) {
	out := page.Path.FilePath()
	log.Printf("Generating => %s", out)
	f := open(out)
	defer f.Close()

	tmpl, err := htmltmpl.ParseFiles("templates/base.html", template)
	if err != nil {
		log.Fatalf("Failed to generate %s: %v", out, err)
	}
	err = tmpl.Execute(f, TemplateData{
		Config: config,
		Exec:   execution,
		Page:   page,
		Data:   data,
	})
	if err != nil {
		log.Fatalf("Failed to generate %s: %v", out, err)
	}
}

func renderText(template string, page Page, data any) {
	out := page.Path.FilePath()
	log.Printf("Generating => %s", out)
	f := open(out)

	tmpl, err := texttmpl.ParseFiles(template)
	if err != nil {
		log.Fatalf("Failed to generate %s: %v", out, err)
	}
	err = tmpl.Execute(f, TemplateData{
		Config: config,
		Exec:   execution,
		Page:   page,
		Data:   data,
	})
	if err != nil {
		log.Fatalf("Failed to generate %s: %v", out, err)
	}
}

// Scan for posts. Populate a global list of metadata for all posts.
// This function is best-effort: if a post fails to parse, it's logged
// and skipped. The list is not sorted in any way.
func scanPosts() {
	glob := filepath.Join("content", "posts", "*", "post.xml")
	paths, err := filepath.Glob(glob)
	if err != nil {
		log.Fatalf("Failed to scan posts: %v", err)
	}
	var entries []PostEntry
	for _, p := range paths {
		f, err := os.Open(p)
		if err != nil {
			log.Printf("WARNING: Error scanning %s: %v", p, err)
			continue
		}
		defer f.Close()
		meta, err := parseMetadata(f)
		if err != nil {
			log.Printf("WARNING: Error scanning %s: metadata: %v", p, err)
			continue
		}
		entries = append(entries, PostEntry{SourcePath: p, Meta: *meta})
	}
	// Set the global variable.
	posts = entries
}

// Parse post metadata. This function does not validate the structure
// of posts. It simply scans for the <meta> element and parses it.
// Parsing stops as soon as the <meta> element is decoded.
func parseMetadata(r io.Reader) (*PostMeta, error) {
	dec := xml.NewDecoder(r)
	for {
		// Search for the <meta> tag.
		tok, err := dec.Token()
		if err == io.EOF {
			return nil, fmt.Errorf("missing <meta>")
		} else if err != nil {
			return nil, err
		}
		start, ok := tok.(xml.StartElement)
		if !ok || start.Name.Local != "meta" {
			continue
		}

		// encoding/xml can't handle time.Time fields, so we unmarshal
		// into a "raw" structure and then parse the date fields.
		var raw struct {
			Title     string `xml:"title"`
			Published string `xml:"published"`
			Updated   string `xml:"updated"`
			Summary   string `xml:"summary"`
			Uuid      string `xml:"uuid"`
		}
		if err := dec.DecodeElement(&raw, &start); err != nil {
			return nil, err
		}

		parseDate := func(s string) (time.Time, error) {
			if s == "" {
				return time.Time{}, nil
			}
			return time.Parse("2006-01-02", s)
		}

		if raw.Title == "" {
			return nil, fmt.Errorf("missing <title>")
		}
		published, err := parseDate(raw.Published)
		if err != nil {
			return nil, fmt.Errorf("<published>: %w", err)
		}
		if published.IsZero() {
			return nil, fmt.Errorf("missing <published>")
		}
		updated, err := parseDate(raw.Updated)
		if err != nil {
			return nil, fmt.Errorf("<updated>: %w", err)
		}

		var id uuid.UUID
		if raw.Uuid == "" {
			// No <uuid> was provided. Calculate one deterministically.
			key := fmt.Sprintf("%s,%s", published.Format("2006-01-02"), raw.Title)
			id = uuid.NewSHA1(config.Atom.FeedId, []byte(key))
		} else {
			id, err = uuid.Parse(raw.Uuid)
			if err != nil {
				return nil, err
			}
		}

		return &PostMeta{
			Title:     raw.Title,
			Published: published,
			Updated:   updated,
			Summary:   raw.Summary,
			Uuid:      id,
		}, nil
	}
}
