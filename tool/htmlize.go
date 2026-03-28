package main

import (
	"fmt"
	"html/template"
	"strings"
	"time"

	"github.com/beevik/etree"
)

// This ugly collection of functions converts an XML post to HTML.

func articleHtml(doc *etree.Document) (template.HTML, error) {
	article := doc.FindElement("article")
	if article == nil {
		return "", fmt.Errorf("missing <article>")
	}

	title := article.FindElement("meta/title").Text()
	date, err := time.Parse("2006-01-02", article.FindElement("meta/published").Text())
	if err != nil {
		return "", fmt.Errorf("Invalid publish date: %v", err)
	}

	// Walk through all the block elements and convert each one to HTML.
	// The overall shape of the document is preserved, except for the
	// 'meta' block, which is dropped.
	blocks := etree.NewDocument()
	for _, el := range article.ChildElements() {
		if el.Tag == "meta" {
			continue
		}
		block, err := blockHtml(el)
		if err != nil {
			return "", err
		}
		blocks.AddChild(block)
	}

	// Now wrap every <h2> element and its contents in a <section>.
	content := groupSections(blocks)

	// Prepend a <header> containing the article title.
	out := etree.NewDocument()
	header := out.CreateElement("header")
	headingHtml(header, title, date)
	for _, el := range content.ChildElements() {
		out.AddChild(el.Copy())
	}

	// Now render the HTML string.
	out.WriteSettings = etree.WriteSettings{
		CanonicalEndTags: true, // Don't use self-closing tags.
		CanonicalText:    true, // Render text as plain text, not CDATA.
		CanonicalAttrVal: true, // Always quote attribute values.
	}
	str, err := out.WriteToString()
	if err != nil {
		return "", err
	}
	return template.HTML(str), nil
}

func headingHtml(out *etree.Element, title string, date time.Time) {
	out.CreateChild("hgroup", func(e *etree.Element) {
		e.CreateChild("h1", func(e *etree.Element) {
			e.CreateChild("a", func(e *etree.Element) {
				e.CreateAttr("class", "h-anchor")
				e.SetText(title)
			})
		})
		e.CreateChild("span", func(e *etree.Element) {
			e.CreateAttr("id", "publish-date")
			e.SetText("Published on ")
			time := e.CreateElement("time")
			time.CreateAttr("datetime", date.Format("2006-01-02"))
			time.SetText(date.Format("January 2, 2006"))
		})
	})
}

// Convert a top-level element to HTML.
func blockHtml(e *etree.Element) (*etree.Element, error) {
	switch e.Tag {
	case "h":
		h2 := etree.NewElement("h2")
		h2.SetText(e.Text())
		return h2, nil
	case "p", "blockquote":
		return inlineHtml(e.Tag, e)
	case "code":
		return codeHtml(e)
	default:
		return nil, fmt.Errorf("unknown block element: <%s>", e.Tag)
	}
}

func inlineHtml(tag string, e *etree.Element) (*etree.Element, error) {
	out := etree.NewElement(tag)
	for _, token := range e.Child {
		switch t := token.(type) {
		case *etree.CharData:
			out.CreateText(t.Data)
		case *etree.Element:
			switch t.Tag {
			case "i":
				out.CreateElement("em").SetText(t.Text())
			case "m":
				span := out.CreateElement("span")
				span.CreateAttr("class", "math")
				span.SetText(t.Text())
			case "a":
				a := out.CreateElement("a")
				a.CreateAttr("href", t.SelectAttrValue("href", "#"))
				a.SetText(t.Text())
			default:
				return nil, fmt.Errorf("unknown inline element: <%s>", t.Tag)
			}
		}
	}
	return out, nil
}

// Convert <code> blocks to HTML.
func codeHtml(e *etree.Element) (*etree.Element, error) {
	pre := etree.NewElement("pre")
	code := pre.CreateElement("code")
	if lang := e.SelectAttrValue("lang", ""); lang != "" {
		code.CreateAttr("class", "language-"+lang)
	}
	code.SetText(strings.TrimSpace(e.Text()))
	return pre, nil
}

func groupSections(doc *etree.Document) *etree.Document {
	result := etree.NewDocument()
	section := result.CreateElement("section")
	for _, e := range doc.ChildElements() {
		switch e.Tag {
		case "h2":
			section = result.CreateElement("section")
			section.AddChild(e.Copy())
		default:
			section.AddChild(e.Copy())
		}
	}
	return result
}
