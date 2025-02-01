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
      '(("en" "<a href=\"/\">Home</a>
<address>%a</address>
<time datetime=\"%d\">%d</time>
")))

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

;; NOTE: Output footnotes inline with their backlink.
(defun org-html-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (pcase (org-export-collect-footnote-definitions info)
    (`nil nil)
    (definitions
     (format
      (plist-get info :html-footnotes-section)
      (org-html--translate "Footnotes" info)
      (format
       "\n%s\n"
       (mapconcat
	    (lambda (definition)
	      (pcase definition
	        (`(,n ,label ,def)
             ;; Do not assign number labels as they appear in Org mode
             ;; - the footnotes are re-numbered by
             ;; `org-export-get-footnote-number'.  If the label is not
             ;; a number, keep it.
             (when (and (stringp label)
                        (equal label (number-to-string (string-to-number label))))
               (setq label nil))
	         ;; `org-export-collect-footnote-definitions' can return
	         ;; two kinds of footnote definitions: inline and blocks.
	         ;; Since this should not make any difference in the HTML
	         ;; output, we wrap the inline definitions within
	         ;; a "footpara" class paragraph.
	         (let ((inline? (not (org-element-map def org-element-all-elements
				                   #'identity nil t)))
		           (anchor (org-html--anchor
                            (format "fn.%s" (or label n))
			                n
			                (format " href=\"#fnr.%s\" role=\"doc-backlink\"" (or label n))
			                info))
		           (contents (org-trim (org-export-data def info))))
	           (format "<p role=\"doc-footnote\">\n%s\n%s\n</p>\n"
		               (format (plist-get info :html-footnote-format) anchor)
                       contents)))))
	    definitions
	    "\n"))))))



(org-publish-all t)
(message "Build complete!")
