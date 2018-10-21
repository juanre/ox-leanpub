;;; ox-leanpub.el --- Leanpub Markdown Back-End for Org Export Engine

;; Author: Juan Reyero <juan _at! juanreyero.com>
;; Keywords: org, wp, markdown, leanpub

;;; Commentary:

;;; Small adaptation of ox-md.el to make the exported markdown work
;;; better for Leanpub (http://leanpub.com) publication.  It handles
;;; footnotes, and makes source code separated from its output, and
;;; the output does not display line numbers.  Html blocks are
;;; ignored.  Links with IDs work.  Tables are exported as they are in
;;; orgmode, which is pretty much what Leanpub's markdown accepts.

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-md)

;;; Define Back-End

(org-export-define-derived-backend 'leanpub 'md
;;  :export-block '("leanpub" "LEANPUB")
  :menu-entry
  '(?L "Export to Leanpub Markdown"
       ((?L "To temporary buffer"
	    (lambda (a s v b) (org-leanpub-export-as-markdown a s v)))
	(?l "To file" (lambda (a s v b) (org-leanpub-export-to-markdown a s v)))
	(?o "To file and open"
	    (lambda (a s v b)
	      (if a (org-leanpub-export-to-markdown t s v)
		(org-open-file (org-leanpub-export-to-markdown nil s v)))))))
  :translate-alist '((fixed-width . org-leanpub-fixed-width-block)
                     (example-block . org-leanpub-fixed-width-block)
                     (special-block . org-leanpub-special-block)
                     (src-block . org-leanpub-src-block)
                     (plain-text . org-leanpub-plain-text)
                     (inner-template . org-leanpub-inner-template)
                     (footnote-reference . org-leanpub-footnote-reference)
                     (headline . org-leanpub-headline)
                     (link . org-leanpub-link)
                     (latex-fragment . org-leanpub-latex-fragment)
                     (table . org-leanpub-table)
                     ;; Will not work with leanpub:
                     (export-block . org-leanpub-ignore)))

(defun org-leanpub-table (table contents info)
  "Transcode a table object from Org to Markdown.
CONTENTS is nil.  INFO is a plist holding contextual information.
Add an #+attr_leanpub: line right before the table with the formatting info that you want to pass to markdown, like

#+attr_leanpub: {title=\"Figure 32\",width=\"60%\"}
| a table | second col |
|---------+------------|
| second  | line       |
| Third   | line       |
"
  (replace-regexp-in-string "^\\#\\+attr_leanpub:\s*" ""
                            (buffer-substring (org-element-property :begin table)
                                              (org-element-property :end table))))

(defun org-leanpub-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to Markdown.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "{$$}%s{/$$}"
          ;; Removes the \[, \] and $ that mark latex fragments
          (replace-regexp-in-string
           "\\\\\\[\\|\\\\\\]\\|\\$" ""
           (org-element-property :value latex-fragment))))

(defun org-md-headline-without-anchor (headline contents info)
  "Transcode HEADLINE element into Markdown format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel. This is the same function as
org-md-headline but without inserting the <a> anchors."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
	   (title (org-export-data (org-element-property :title headline) info))
	   (todo (and (plist-get info :with-todo-keywords)
		      (let ((todo (org-element-property :todo-keyword
							headline)))
			(and todo (concat (org-export-data todo info) " ")))))
	   (tags (and (plist-get info :with-tags)
		      (let ((tag-list (org-export-get-tags headline info)))
			(and tag-list
			     (concat "     " (org-make-tag-string tag-list))))))
	   (priority
	    (and (plist-get info :with-priority)
		 (let ((char (org-element-property :priority headline)))
		   (and char (format "[#%c] " char)))))
	   ;; Headline text without tags.
	   (heading (concat todo priority title))
	   (style (plist-get info :md-headline-style)))
      (cond
       ;; Cannot create a headline.  Fall-back to a list.
       ((or (org-export-low-level-p headline info)
	    (not (memq style '(atx setext)))
	    (and (eq style 'atx) (> level 6))
	    (and (eq style 'setext) (> level 2)))
	(let ((bullet
	       (if (not (org-export-numbered-headline-p headline info)) "-"
		 (concat (number-to-string
			  (car (last (org-export-get-headline-number
				      headline info))))
			 "."))))
	  (concat bullet (make-string (- 4 (length bullet)) ?\s) heading tags "\n\n"
		  (and contents (replace-regexp-in-string "^" "    " contents)))))
       (t
	(concat (org-md--headline-title style level heading nil tags)
		        contents))))))

;;; Adding the id so that crosslinks work.
(defun org-leanpub-headline (headline contents info)
  (concat (let ((id (or (org-element-property :ID headline) (org-element-property :CUSTOM_ID headline))))
            (if id
                (format "{#L%s}" id)
              ""))
          (org-md-headline-without-anchor headline contents info)))

(defun org-leanpub-inner-template (contents info)
  "Return complete document string after markdown conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options.  Required in order to add footnote
definitions at the end."
  (concat
   contents
   "\n\n"
   (let ((definitions (org-export-collect-footnote-definitions
		                   info
                       (plist-get info :parse-tree))))
     ;; Looks like leanpub do not like : in labels.
     (mapconcat (lambda (ref)
                  (let ((id (format "[^%s]: " (replace-regexp-in-string
                                               ":" "_"
                                               (let ((label (cadr ref)))
                                                (if label
                                                    label
                                                  (car ref)))))))
                    (let ((def (nth 2 ref)))
                      (concat id (org-export-data def info)))))
                definitions "\n\n"))))

(defun org-leanpub-footnote-reference (footnote contents info)
  ;; Looks like leanpub do not like : in labels.
  (format "[^%s]"
          (replace-regexp-in-string
           ":" "_"
           (let ((label (org-element-property :label footnote)))
             (if label
                 label
               (org-export-get-footnote-number footnote info))))))

(defun org-leanpub-ignore (src-block contents info)
  "")

(defun org-leanpub-plain-text (text info)
  text)

;;; {lang="python"}
;;; ~~~~~~~~
;;; def longitude_circle(diameter):
;;;     return math.pi * diameter
;;; longitude(10)
;;; ~~~~~~~~
(defun org-leanpub-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((lang (org-element-property :language src-block))
        (linenos (org-element-property :number-lines src-block)))
    (format "{lang=\"%s\"%s}\n~~~~~~~~\n%s~~~~~~~~"
            lang
            (if linenos ",linenos=on" "")
            (org-remove-indentation
             (org-element-property :value src-block)))))

;;; > ~~~~~~~~
;;; > 123.0
;;; > ~~~~~~~~
(defun org-leanpub-fixed-width-block (src-block contents info)
  "Transcode FIXED-WIDTH-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((linenos (org-element-property :number-lines src-block)))
    (format "%s~~~~~~~~\n%s~~~~~~~~"
            (if linenos "{linenos=on}\n" "")
            (org-remove-indentation
             (org-element-property :value src-block)))))

;;; Export special blocks, mapping them to corresponding block types according to the LeanPub documentation at https://leanpub.com/help/manual#leanpub-auto-blocks-of-text.
;;; The supported block types and their conversions are listed in lp-block-mappings.
;;; e.g.
;;;     #+begin_tip
;;;     This is a tip
;;;     #+end_tip
;;; gets exported as
;;;     T> This is a tip
(defun org-leanpub-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((type (org-element-property :type special-block))
         (lp-block-mappings
          '(tip "T"
            aside "A"
            warning "W"
            error "E"
            note "I"
            question "Q"
            discussion "D"
            exercise "X"
            center "C"))
         (lp-char (plist-get lp-block-mappings (intern type))))
    (replace-regexp-in-string
     "^" (concat lp-char "> ")
     (org-remove-indentation contents))))

(defun org-leanpub-link (link contents info)
  "Transcode a link object into Markdown format.
CONTENTS is the link's description.  INFO is a plist used as
a communication channel."
  (let ((type (org-element-property :type link)))
    (cond ((member type '("custom-id" "id"))
           (let ((id (org-element-property :path link)))
             (format "[%s](#L%s)" contents id)))
          ((org-export-inline-image-p link org-html-inline-image-rules)
           (let ((path (let ((raw-path (org-element-property :path link)))
                         (if (not (file-name-absolute-p raw-path)) raw-path
                           (expand-file-name raw-path)))))
             (format "![%s](%s)"
                     (let ((caption (org-export-get-caption
                                     (org-export-get-parent-element link))))
                       (if caption
                           (org-export-data caption info)
                         ""))
                     path)))
          (t (let* ((raw-path (org-element-property :path link))
                    (path (if (member type '("http" "https" "ftp"))
                              (concat type ":" raw-path)
                            nil)))
               (if path
                   (if (not contents) (format "<%s>" path)
                     (format "[%s](%s)" contents path))
                 contents))))))

;;; Interactive function

;;;###autoload
(defun org-leanpub-export-as-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org MD Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'leanpub "*Org LEANPUB Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-leanpub-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Leanpub's compatible Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'leanpub outfile async subtreep visible-only)))

;;;###autoload
(defun org-leanpub-publish-to-leanpub (plist filename pub-dir)
  "Publish an org file to leanpub.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'leanpub filename ".md" plist pub-dir))

(provide 'ox-leanpub)

;;; ox-leanpub.el ends here
