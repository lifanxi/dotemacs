(setq muse-journal-html-entry-template "<div class=\"entry\">
  <div class=\"entry-body\">
    <div class=\"entry-head\">
      <div class=\"entry-title\">
        <h2>%title%</h2>
      </div>
      <div class=\"entry-date\">
        <span class=\"date\">%date%</span>
      </div>
    </div>
    <div class=\"entry-text\">
%text% 
    </div>
  </div>
</div>
")
(setq muse-journal-rss-entry-template "
    <item>
      <title>%title%</title>
      <link>%link%</link>
      <guid>%link%</guid>
      <author>Li Fanxi&lt;lifanxi@freemindworld.com&gt;</author>
      <description>%desc%</description>
      <pubDate>%date%</pubDate>
    </item>
")
(setq muse-xhtml-charset-default "utf-8")
(setq muse-xhtml-encoding-default (quote utf-8))
(setq muse-xhtml-header "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
          \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">
  <head>
    <title><lisp>(muse-publishing-directive \"title\")</lisp></title>
    <meta name=\"generator\" content=\"muse.el\" />
    <meta http-equiv=\"<lisp>muse-html-meta-http-equiv</lisp>\"
          content=\"<lisp>muse-html-meta-content-type</lisp>\" />
    <meta name=\"keywords\" content=\"<lisp>(muse-publishing-directive \"keywords\")</lisp>\" />
    <lisp>
      (let ((maintainer (muse-style-element :maintainer)))
        (when maintainer
          (concat \"<link rev=\\\"made\\\" href=\\\"\" maintainer \"\\\" />\")))
    </lisp><lisp>
      (muse-style-element :style-sheet muse-publishing-current-style)
    </lisp>
    <link rel=\"alternate\" type=\"application/rss+xml\" title=\"李凡希的Blog\" href=\"http://www.freemindworld.com/blog/feed.rss/\" />
    <link rel=\"alternate\" type=\"application/rss+xml\" title=\"Fanxi在饭否\" href=\"http://api.fanfou.com/statuses/user_timeline/lifanxi.rss\" />
    <link rel=\"alternate\" type=\"application/rss+xml\" title=\"Li Fanxi的网络相册\" href=\"http://picasaweb.google.com/data/feed/base/user/lifanxi?kind=album&amp;alt=rss&amp;hl=zh_CN&amp;access=public\" />
    <link rel=\"shortcut icon\" href=\"/favicon.ico\" />
    <link rel=\"Bookmark\" href=\"/favicon.ico\" />
    <script type=\"text/javascript\" src=\"/blog/common/script.js\"></script>
  </head>
  <body>
    <div class=\"container\">
    <div class=\"header\">		<div class=\"title\"><h1><lisp>
  (concat (muse-publishing-directive \"title\")
          (let ((author (muse-publishing-directive \"author\")))
            (if (not (string= author (user-full-name)))
                (concat \" (by \" author \")\"))))</lisp></h1></div>
<!--#include virtual=\"/blog/common/menu.shtml\"-->
</div>
<div class=\"main\"><div class=\"left\">
    <!-- Page published by Emacs Muse begins here -->
")
(setq muse-xhtml-markup-strings (quote ((image-with-desc . "<table class=\"image\">
  <tr><td align=\"center\"><img src=\"%1%.%2%\" alt=\"%3%\" width=\"100%%\" /></td></tr>
  <tr><td align=\"center\" class=\"image-caption\">%3%</td></tr>
</table>") (image . "<img src=\"%s.%s\" alt=\"\" width=\"100%%\" />") (image-link . "<a class=\"image-link\" href=\"%s\">
<img src=\"%s.%s\" alt=\"\" /></a>") (rule . "<hr />") (fn-sep . "<hr />
") (begin-underline . "<span style=\"text-decoration: underline;\">") (end-underline . "</span>") (begin-center . "<p style=\"text-align: center;\">
") (end-center . "
</p>") (end-verse-line . "<br />") (end-last-stanza-line . "<br />") (empty-verse-line . "<br />"))))
(setq load-path (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/muse"))

;; muse start here
(require 'muse-mode)
(require 'muse-html)     ; load publishing styles I use
(require 'muse-journal)
(require 'muse-blosxom)
(require 'muse-wiki)
(setq muse-project-alist
      '(("wiki"                      ; my various writings
         ("~/wiki" :default "index")
         )
        ("MyWebPages"
           ("~/web" :default "index")
	       (:base "journal-xhtml" :path "~/blog/")
         )
        ("Blog2007"
           ("~/web/2007" :default "index")
	       (:base "journal-xhtml" :path "~/blog/2007/")
           (:base "journal-rss" :base-url "http://www.freemindworld.com/blog/2007/" :path "~/blog_publish_rss/")
         )
        ("Blog2008"
           ("~/web/2008" :default "index")
	       (:base "journal-xhtml" :path "~/blog/2008/")
           (:base "journal-rss" :base-url "http://www.freemindworld.com/blog/2008/" :path "~/blog_publish_rss/")
         )
        ("Blog2009"
           ("~/web/2009" :default "index")
	       (:base "journal-xhtml" :path "~/blog/2009/")
           (:base "journal-rss" :base-url "http://www.freemindworld.com/blog/2009/" :path "~/blog_publish_rss/")
         )
        ("Blog2010"
           ("~/web/2010" :default "index")
	       (:base "journal-xhtml" :path "~/blog/2010/")
           (:base "journal-rss" :base-url "http://www.freemindworld.com/blog/2010/" :path "~/blog_publish_rss/")
         )
        ("Interest"
           ("~/web/interest" :default "index")
	       (:base "xhtml" :path "~/interest/")
         )
        ("MyWorks"
           ("~/web/works" :default "index")
	       (:base "xhtml" :path "~/works/")
         )

))
(setq muse-xhtml-style-sheet "<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"all\" href=\"/blog/common/style.css\" />")
(setq muse-xhtml-footer "</div>\n<div class=\"right\"><!--#include virtual=\"/blog/common/right.shtml\"--> </div>\n<div class=\"clearer\"><span></span></div></div></div>\n<div class=\"footer\"><!--#include virtual=\"/blog/common/foot.shtml\"--></div>\n</body>\n</html>")
(setq muse-xhtml-extension ".shtml")
(setq muse-html-extension ".shtml")
(setq muse-rss-extension ".shtml")
(setq muse-html-markup-strings (quote ((image-with-desc . "<table class=\"image\" width=\"100%%\">
  <tr><td align=\"center\"><img src=\"%1%.%2%\" alt=\"%3%\"></td></tr>
  <tr><td align=\"center\" class=\"image-caption\">%3%</td></tr>
</table>") (image . "<img src=\"%s.%s\" alt=\"\">") (image-link . "<a class=\"image-link\" href=\"%s\">
<img src=\"%s.%s\"></a>") (anchor-ref . "<a href=\"#%s\">%s</a>") (url . "<a href=\"%s\">%s</a>") (link . "<a href=\"%s\">%s</a>") (link-and-anchor . "<a href=\"%s#%s\">%s</a>") (email-addr . "<a href=\"mailto:%s\">%s</a>") (anchor . "<a name=\"%1%\" id=\"%1%\">") (emdash . "%s&mdash;%s") (comment-begin . "<!-- ") (comment-end . " -->") (rule . "<hr>") (fn-sep . "<hr>
") (no-break-space . "&nbsp;") (enddots . "....") (dots . "...") (section . "<h2>") (section-end . "</h2>") (subsection . "<h3>") (subsection-end . "</h3>") (subsubsection . "<h4>") (subsubsection-end . "</h4>") (section-other . "<h5>") (section-other-end . "</h5>") (begin-underline . "<u>") (end-underline . "</u>") (begin-literal . "<code>") (end-literal . "</code>") (begin-cite . "<span class=\"citation\">") (begin-cite-author . "<span class=\"citation-author\">") (begin-cite-year . "<span class=\"citation-year\">") (end-cite . "</span>") (begin-emph . "<em>") (end-emph . "</em>") (begin-more-emph . "<strong>") (end-more-emph . "</strong>") (begin-most-emph . "<strong><em>") (end-most-emph . "</em></strong>") (begin-verse . "<p class=\"verse\">
") (verse-space . "&nbsp;&nbsp;") (end-verse-line . "<br>") (end-last-stanza-line . "<br>") (empty-verse-line . "<br>") (end-verse . "</p>") (begin-example . "<pre class=\"example\">") (end-example . "</pre>") (begin-center . "<center>
<p>") (end-center . "</p>
</center>") (begin-quote . "<blockquote>
") (end-quote . "
</blockquote>") (begin-quote-item . "<p class=\"quoted\">") (end-quote-item . "</p>") (begin-uli . "<ul>
") (end-uli . "
</ul>") (begin-uli-item . "<li>") (end-uli-item . "</li>") (begin-oli . "<ol>
") (end-oli . "
</ol>") (begin-oli-item . "<li>") (end-oli-item . "</li>") (begin-dl . "<dl>
") (end-dl . "
</dl>") (begin-ddt . "<dt><strong>") (end-ddt . "</strong></dt>") (begin-dde . "<dd>") (end-dde . "</dd>") (begin-table . "<table%s>
") (end-table . "</table>") (begin-table-row . "    <tr>
") (end-table-row . "    </tr>
") (begin-table-entry . "      <%s>") (end-table-entry . "</%s>
"))))
