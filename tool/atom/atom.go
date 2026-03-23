package atom

import (
	"encoding/xml"
	"fmt"
	"io"
	"net/url"
	"strings"
	"time"
)

// https://validator.w3.org/feed/docs/atom.html
type Feed struct {
	XMLName xml.Name `xml:"http://www.w3.org/2005/Atom feed"`
	Id      string   `xml:"id"`
	Title   string   `xml:"title"`
	Updated Time     `xml:"updated"`
	Authors []Person `xml:"author"`
	Links   []Link   `xml:"link"`
	Entries []Entry  `xml:"entry"`
}

type Entry struct {
	Id        string   `xml:"id"`
	Title     string   `xml:"title"`
	Published Time     `xml:"published"`
	Updated   Time     `xml:"updated"`
	Authors   []Person `xml:"author"`
	Links     []Link   `xml:"link"`
	Summary   string   `xml:"summary,omitempty"`
}

const (
	RelAlternate = "alternate"
	RelEnclosure = "enclosure"
	RelRelated   = "related"
	RelSelf      = "self"
	RelVia       = "via"
)

// https://validator.w3.org/feed/docs/atom.html#link
type Link struct {
	Rel      string `xml:"rel,attr,omitempty"`
	Href     string `xml:"href,attr"`
	HrefLang string `xml:"hreflang,attr,omitempty"`
	Type     string `xml:"type,attr,omitempty"`
	Title    string `xml:"title,attr,omitempty"`
	Length   int    `xml:"length,attr,omitempty"`
}

// https://validator.w3.org/feed/docs/atom.html#person
type Person struct {
	Name  string `xml:"name"`
	Email string `xml:"email,omitempty"`
	Uri   string `xml:"uri,omitempty"`
}

type Time time.Time

// Marshal timestamps using RFC 3339
func (t Time) MarshalText() ([]byte, error) {
	if time.Time(t).IsZero() {
		return nil, nil // omitempty
	}
	return []byte(time.Time(t).UTC().Format(time.RFC3339)), nil
}

// Unmarshal timestamps using RFC 3339
func (t *Time) UnmarshalText(b []byte) error {
	x, err := time.Parse(time.RFC3339, string(b))
	if err != nil {
		return err
	}
	*t = Time(x)
	return nil
}

// Return the timestamp of the most recent update.
func (e Entry) LastUpdate() Time {
	if !time.Time(e.Updated).IsZero() {
		return e.Updated
	} else {
		return e.Published
	}
}

// Find the most recent update among all entries.
func mostRecentUpdate(entries []Entry) Time {
	var mostRecent time.Time
	for _, e := range entries {
		t := time.Time(e.LastUpdate())
		if t.After(mostRecent) {
			mostRecent = t
		}
	}
	return Time(mostRecent)
}

func Validate(feed Feed) error {
	if feed.Id == "" {
		return fmt.Errorf("feed id cannot by empty")
	}
	if _, err := url.Parse(feed.Id); err != nil {
		return fmt.Errorf("feed id must be a URI: %w", err)
	}
	if strings.TrimSpace(feed.Title) == "" {
		return fmt.Errorf("feed title cannot by empty")
	}
	if time.Time(feed.Updated).IsZero() {
		return fmt.Errorf("feed timestamp cannot be empty")
	}

	// Keep track of entry ids to make sure they're unique
	idmap := make(map[string]bool)

	for _, e := range feed.Entries {
		if _, err := url.Parse(e.Id); err != nil {
			return fmt.Errorf("entry id must be a URI: %w", err)
		}
		if _, ok := idmap[e.Id]; ok {
			return fmt.Errorf("duplicate entry id: %s", e.Id)
		}
		idmap[e.Id] = true

		published := time.Time(e.Published)
		if published.IsZero() {
			return fmt.Errorf("entry %s: invalid 'published' timestamp", e.Id)
		}
		updated := time.Time(e.Updated)
		if !updated.IsZero() && updated.Before(published) {
			return fmt.Errorf("entry %s: 'updated' timestamp older than 'published' timestamp", e.Id)
		}
		if time.Time(feed.Updated).Before(time.Time(e.LastUpdate())) {
			return fmt.Errorf("entry %s: feed timestamp older than most recent update", e.Id)
		}
	}
	return nil
}

func WriteFeed(w io.Writer, feed Feed) error {
	// Set the most recent update timestamp for the feed if not set.
	if time.Time(feed.Updated).IsZero() {
		feed.Updated = mostRecentUpdate(feed.Entries)
	}
	if err := Validate(feed); err != nil {
		return err
	}
	if _, err := io.WriteString(w, xml.Header); err != nil {
		return err
	}
	enc := xml.NewEncoder(w)
	enc.Indent("", "  ")
	return enc.Encode(feed)
}
