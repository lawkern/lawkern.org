;; NOTE: This build system is loosely based on the one described here:
;; https://systemcrafters.net/publishing-websites-with-org-mode/building-the-site/

;; TODO: Org-mode tries to publish the author name and date in the postamble by
;; default. Ideally they'd be put at the top or bottom of the content container
;; instead, so that their address/time tags could be enclosed in the main
;; article tags. Is there a good way to do this that doesn't involve fussing
;; with the main content function itself?

(require 'ox-publish)

(setq output-directory "../lawkern.com")

(setq org-publish-project-alist
      (list
       (list "lawkern:html"
             :recursive t
             :base-directory "./"
             :publishing-directory output-directory
             :publishing-function 'org-html-publish-to-html
             :section-numbers nil
             :with-author t
             :with-creator nil
             :time-stamp-file nil
             :validation-link nil
             :html-head-extra "<link type=\"text/css\" rel=\"stylesheet\" href=\"/law.css\" />")

       (list "lawkern:static"
             :base-directory "./assets"
             :recursive t
             :base-extension "ico\\|css\\|png\\|jpg"
             :publishing-directory output-directory
             :publishing-function 'org-publish-attachment)))

;; NOTE: These options should generally hold for any project, so they're
;; declared outside the project-alist.
(setq org-html-doctype "html5")
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)
(setq org-html-htmlize-output-type nil)
(setq org-html-head-include-scripts nil)
(setq org-html-head-include-default-style nil)
(setq org-html-head "<meta name=\"color-scheme\" content=\"light dark\" />")

(setq org-html-container-element "section")

(setq org-html-divs
      '((preamble "div" "preamble")
        (content "article" "content")
        (postamble "footer" "postamble")))

(setq org-html-preamble nil)
(setq org-html-preamble-format
      '(("en" "")))

(setq org-html-postamble t)
(setq org-html-postamble-format
      '(("en" "<footer style=\"display: flex; justify-content: space-between; gap: 0.5rem;\">
<a href=\"/\">Home</a>
<address>%a</address>
<time datetime=\"%d\">%d</time>
</footer>")))

(setq org-html-footnotes-section "<hr>
<section id=\"%s\">
%s
</section>")

;; NOTE: Simplify src_block export to just use <pre><code> tags.
(defun org-html-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((code (org-html-format-code src-block info)))
	(format "<pre><code>%s</code></pre>" code)))

(org-publish-all t)
(message "Build complete!")
