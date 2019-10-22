;;; ox-leanpub-markua.el --- Markua Back-End for Org Export Engine

;; Author: Diego Zamboni <diego@zzamboni.org>
;; Keywords: org, wp, markdown, leanpub, markua

;;; Commentary:

;;; Adaptation of ox-leanpub-markdown.el to use Markua markup as appropriate, as
;;; used by Leanpub (https://leanpub.com/markua/read) publication.  It handles
;;; footnotes, and makes source code separated from its output, and
;;; the output does not display line numbers.  Html blocks are
;;; ignored.  Links with IDs work.  Tables are exported as they are in
;;; orgmode, which is pretty much what Leanpub's markdown accepts.
;;; The #+NAME and #+CAPTION attributes of an object are converted to
;;; the LeanPub "id" and "title" attributes. Other attributes
;;; specified in an #+ATTR_LEANPUB line are included as-is. For example:
;;;
;;; #+NAME: some-id
;;; #+CAPTION: Some name
;;; #+ATTR_LEANPUB: :width wide
;;;
;;; Get converted to the following line, included before the corresponding element:
;;;
;;; {id="some-id", title="Some name", width="wide"}

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-md)
(require 'ob-core)
(require 'subr-x)

;;; Define Back-End

(org-export-define-derived-backend 'markua 'leanpub
  :menu-entry
  '(?M "Export to Leanpub Markua"
       ((?M "To temporary buffer"
	          (lambda (a s v b) (org-leanpub-markua-export-as-markua a s v)))
	      (?m "To file" (lambda (a s v b) (org-leanpub-markua-export-to-markua a s v)))
	      (?o "To file and open"
	          (lambda (a s v b)
	            (if a (org-leanpub-markua-export-to-markua t s v)
		            (org-open-file (org-leanpub-markua-export-to-markua nil s v)))))))
  :translate-alist '((fixed-width . org-markua-fixed-width-block)
                     (example-block . org-markua-example-block)
                     (special-block . org-markua-special-block)
                     (src-block . org-markua-src-block)
                     (plain-text . org-markua-plain-text)
                     (inner-template . org-markua-inner-template)
                     (footnote-reference . org-markua-footnote-reference)
                     (headline . org-markua-headline)
                     (item . org-markua-item)
                     (link . org-markua-link)
                     (latex-fragment . org-markua-latex-fragment)
                     (line-break . org-markua-line-break)
                     (paragraph . org-markua-paragraph)
                     (table-cell . org-markua-table-cell)
                     (table-row . org-markua-table-row)
                     (table . org-markua-table)
                     ;; (table . org-markua-table)
                     ;; (table-cell . org-markua-table-cell)
                     ;; (table-row . org-markua-table-row)
                     ;; Will not work with leanpub:
                     (export-block . org-markua-ignore)))

;;; Utility functions

;; Collect #+NAME, #+CAPTION, and any attributes specified as :key
;; value in the #+ATTR_LEANPUB line, and put them all together in a
;; Leanpub-style attribute line of the form {key=value,...}. If an
;; attribute is present in both places (e.g. if both #+CAPTION and
;; :title are specified), then the values from #+ATTR_LEANPUB take
;; precedence.
(defun org-markua-attribute-line (elem info &optional other-attrs nonewline)
  (let* ((init (list (cons :id (or (org-element-property :name elem)
                                   (org-element-property :ID elem)
                                   (org-element-property :CUSTOM_ID elem)))
                     (cons :title (org-export-data (org-element-property :caption elem) info))))
         (lpattr-str (car (org-element-property :attr_leanpub elem)))
         (lpattr (append (org-babel-parse-header-arguments lpattr-str) other-attrs init))
         (oldstyle (string-prefix-p "{" lpattr-str))
         (printed '())
         (lpattr-str-new (mapconcat 'identity
                                    (remove-if 'null
                                               (mapcar (lambda (elem)
                                                         (let* ((keysym (car elem))
                                                                (keystr (apply #'string (cdr (string-to-list (symbol-name keysym)))))
                                                                (val (cdr elem)))
                                                           (when (and (> (length val) 0) (not (plist-member printed keysym)))
                                                             (setq printed (plist-put printed keysym t))
                                                             (format "%s: \"%s\"" keystr val))))
                                                       lpattr)) ", "))
         (output (if oldstyle
                     (format "%s" lpattr-str)
                   (when (> (length lpattr-str-new) 0)
                     (format "{%s}"
                             lpattr-str-new))))
         )
    (when (> (length output) 0)
      (concat
       output
       (unless nonewline "\n")))))

(defun chomp-end (str)
  "Chomp trailing whitespace from STR."
  (replace-regexp-in-string (rx (* (any " \t\n")) eos)
                            ""
                            str))

;;;; Table support, borrowed from ox-gfm (since the Leanpub Markua renderer supports GFM's table format)
;;;; https://github.com/larstvei/ox-gfm

;;;; Table-Common

(defvar width-cookies nil)
(defvar width-cookies-table nil)

(defconst markua-table-left-border "|")
(defconst markua-table-right-border " |")
(defconst markua-table-separator " |")

(defun org-markua-table-col-width (table column info)
  "Return width of TABLE at given COLUMN. INFO is a plist used as
communication channel. Width of a column is determined either by
inquerying `width-cookies' in the column, or by the maximum cell with in
the column."
  (let ((cookie (when (hash-table-p width-cookies)
                  (gethash column width-cookies))))
    (if (and (eq table width-cookies-table)
             (not (eq nil cookie)))
        cookie
      (progn
        (unless (and (eq table width-cookies-table)
                     (hash-table-p width-cookies))
          (setq width-cookies (make-hash-table))
          (setq width-cookies-table table))
        (let ((max-width 0)
              (specialp (org-export-table-has-special-column-p table)))
          (org-element-map
              table
              'table-row
            (lambda (row)
              (setq max-width
                    (max (length
                          (org-export-data
                           (org-element-contents
                            (elt (if specialp (car (org-element-contents row))
                                   (org-element-contents row))
                                 column))
                           info))
                         max-width)))
            info)
          (puthash column max-width width-cookies))))))


(defun org-markua-make-hline-builder (table info char)
  "Return a function to build horizontal line in TABLE with given
CHAR. INFO is a plist used as a communication channel."
  `(lambda (col)
     (let ((max-width (max 3 (org-markua-table-col-width table col info))))
       (when (< max-width 1)
         (setq max-width 1))
       (make-string max-width ,char))))


;;;; Table-Cell

(defun org-markua-table-cell (table-cell contents info)
  "Transcode TABLE-CELL element from Org into GFM. CONTENTS is content
of the cell. INFO is a plist used as a communication channel."
  (let* ((table (org-export-get-parent-table table-cell))
         (column (cdr (org-export-table-cell-address table-cell info)))
         (width (org-markua-table-col-width table column info))
         (left-border (if (org-export-table-cell-starts-colgroup-p table-cell info) "| " " "))
         (right-border " |")
         (data (or contents "")))
    (setq contents
          (concat data
                  (make-string (max 0 (- width (string-width data)))
                               ?\s)))
    (concat left-border contents right-border)))


;;;; Table-Row

(defun org-markua-table-row (table-row contents info)
  "Transcode TABLE-ROW element from Org into GFM. CONTENTS is cell
contents of TABLE-ROW. INFO is a plist used as a communication
channel."
  (let ((table (org-export-get-parent-table table-row)))
    (when (and (eq 'rule (org-element-property :type table-row))
               ;; In GFM, rule is valid only at second row.
               (eq 1 (cl-position
                      table-row
                      (org-element-map table 'table-row 'identity info))))
      (let* ((table (org-export-get-parent-table table-row))
             (header-p (org-export-table-row-starts-header-p table-row info))
             (build-rule (org-markua-make-hline-builder table info ?-))
             (cols (cdr (org-export-table-dimensions table info))))
        (setq contents
              (concat markua-table-left-border
                      (mapconcat (lambda (col) (funcall build-rule col))
                                 (number-sequence 0 (- cols 1))
                                 markua-table-separator)
                      markua-table-right-border))))
    contents))



;;;; Table

(defun org-markua-table (table contents info)
  "Transcode TABLE element into Github Flavored Markdown table.
CONTENTS is the contents of the table. INFO is a plist holding
contextual information."
  (let* ((rows (org-element-map table 'table-row 'identity info))
         (no-header (or (<= (length rows) 1)))
         (cols (cdr (org-export-table-dimensions table info)))
         (build-dummy-header
          (function
           (lambda ()
             (let ((build-empty-cell (org-markua-make-hline-builder table info ?\s))
                   (build-rule (org-markua-make-hline-builder table info ?-))
                   (columns (number-sequence 0 (- cols 1))))
               (concat markua-table-left-border
                       (mapconcat (lambda (col) (funcall build-empty-cell col))
                                  columns
                                  markua-table-separator)
                       markua-table-right-border "\n" markua-table-left-border
                       (mapconcat (lambda (col) (funcall build-rule col))
                                  columns
                                  markua-table-separator)
                       markua-table-right-border "\n"))))))
    (concat (org-markua-attribute-line table info)
            (when no-header (funcall build-dummy-header))
            (replace-regexp-in-string "\n\n" "\n" contents))))

;; (defun org-markua-table (table contents info)
;;   "Transcode a table object from Org to Markua.
;; CONTENTS is nil.  INFO is a plist holding contextual information.

;; | a table | second col |
;; |---------+------------|
;; | second  | line       |
;; | Third   | line       |
;; "
;;   (concat
;;    (org-markua-attribute-line table info)
;;    (replace-regexp-in-string "^\s*\n" "" (org-export-data (org-element-contents table) info))))

;; (defun org-markua-table-row (table-row contents info)
;;   (format "| %s" (org-export-data contents info)))

;; (defun org-markua-table-cell (table-cell contents info)
;;   (format " %s |" (org-export-data contents info)))

(defun org-markua-latex-fragment (latex-fragment contents info)
  "Transcode a LATEX-FRAGMENT object from Org to Markua.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   (format "`%s`$"
           ;; Removes the \[, \] and $ that mark latex fragments
           (replace-regexp-in-string
            "\\\\\\[\\|\\\\\\]\\|\\$" ""
            (org-element-property :value latex-fragment)))))

(defun org-leanpub-markua-headline-without-anchor (headline contents info)
  "Transcode HEADLINE element into Markua format.
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
	        (concat bullet " " heading tags "\n\n"
		              (and contents (replace-regexp-in-string "^" (make-string (1+ (length bullet)) ?\s) contents)))))
       (t
	      (concat (org-md--headline-title style level heading nil tags)
		            contents))))))

;;; Adding the id so that crosslinks work.
(defun org-markua-headline (headline contents info)
  (concat (org-markua-attribute-line headline info)
          (string-trim-left (org-leanpub-markua-headline-without-anchor headline contents info))))

(defun org-markua-item (item contents info)
  "Transcode ITEM element into Markua format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
         (struct (org-element-property :structure item))
         (bullet (if (not (eq type 'ordered)) "*"
                   (concat (number-to-string
                            (car (last (org-list-get-item-number
                                        (org-element-property :begin item)
                                        struct
                                        (org-list-prevs-alist struct)
                                        (org-list-parents-alist struct)))))
                           ".")))
         (tag (org-element-property :tag item)))
    (concat (if tag
                (concat (org-export-data tag info) "\n")
              (concat bullet " "))
            (and contents
                 (concat (and tag ": ")
                  (org-trim (replace-regexp-in-string "^" (make-string (1+ (length bullet)) ?\s) contents)))))))

(defun org-markua-inner-template (contents info)
  "Return complete document string after Markua conversion.
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

(defun org-markua-footnote-reference (footnote contents info)
  ;; Looks like leanpub do not like : in labels.
  (format "[^%s]"
          (replace-regexp-in-string
           ":" "_"
           (let ((label (org-element-property :label footnote)))
             (if label
                 label
               (org-export-get-footnote-number footnote info))))))

(defun org-markua-ignore (src-block contents info)
  "")

(defun org-markua-plain-text (text info)
  text)

;;; EOLs are removed from paragraphs in Markua

(defun org-markua-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to Markua.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  ;;(message (format "Formatting paragraph: %s" contents))
  (concat (org-markua-attribute-line paragraph info)
          (replace-regexp-in-string "{{markua:linebreak}}" "\n"
                                    (replace-regexp-in-string "\n" " " contents)
                                    nil 'literal)))

(defun org-markua-get-header-arg (arg src-block)
  ;(message "src-block=%s arg=%s parameters='%s'" src-block arg (org-element-property :parameters info))
  (alist-get arg (org-babel-parse-header-arguments (org-element-property :parameters src-block))))

;;; {lang="python"}
;;; ~~~~~~~~
;;; def longitude_circle(diameter):
;;;     return math.pi * diameter
;;; longitude(10)
;;; ~~~~~~~~
(defun org-markua-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into Markua format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((noweb-ref (org-markua-get-header-arg :noweb-ref src-block))
         (attrs (list (cons :format (org-element-property :language src-block))
                      (cons :line-numbers (when (org-element-property :number-lines src-block) "true"))
                      (cons :caption (when noweb-ref (format "*«%s»=*" noweb-ref)))))
         (block-value (org-element-property :value src-block)))
;;    (message "SRC block: %s" src-block)
    (concat
     (org-markua-attribute-line src-block info attrs)
     (format "```\n%s%s```"
             (org-remove-indentation block-value)
             ;; Insert a newline if the block doesn't end with one
             (if (string-suffix-p "\n" block-value) "" "\n")))))

;;; > ~~~~~~~~
;;; > 123.0
;;; > ~~~~~~~~
(defun org-markua-example-block (src-block contents info)
  "Transcode FIXED-WIDTH-BLOCK element into Markua format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-markua-src-block src-block contents info))

;;; > ~~~~~~~~
;;; > 123.0
;;; > ~~~~~~~~
(defun org-markua-fixed-width-block (src-block contents info)
  "Transcode FIXED-WIDTH-BLOCK element into Markua format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-markua-src-block src-block contents info))

;;; Export special blocks, mapping them to corresponding block types according to the LeanPub documentation at https://leanpub.com/help/manual#leanpub-auto-blocks-of-text.
;;; The supported block types and their conversions are listed in lp-block-mappings.
;;; e.g.
;;;     #+begin_tip
;;;     This is a tip
;;;     #+end_tip
;;; gets exported as
;;;     T> This is a tip
(defun org-markua-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element into Markua format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((type (org-element-property :type special-block))
         (caption (org-export-data (org-element-property :caption special-block) info))
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
    (concat
     (org-markua-attribute-line special-block info)
     (replace-regexp-in-string
      "^" (concat lp-char "> ")
      (concat
       (when (> (length caption) 0) (format "### %s\n" caption))
       (chomp-end (org-remove-indentation contents)))))))

(defun org-markua-link (link contents info)
  "Transcode a link object into Markua format.
CONTENTS is the link's description.  INFO is a plist used as
a communication channel."
  (let ((type (org-element-property :type link)))
    (cond ((member type '("custom-id" "id"))
           (let ((id (org-element-property :path link)))
             (format "[%s](#%s)" contents id)))
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

;;;; Line Break

(defun org-markua-line-break (_line-break _contents info)
  "Transcode a LINE-BREAK object from Org to Markua.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "{{markua:linebreak}}")

;;; Interactive function

;;;###autoload
(defun org-leanpub-markua-export-as-markua (&optional async subtreep visible-only)
  "Export current buffer to a Markua buffer.

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
  (org-export-to-buffer 'markua "*Org MARKUA Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-leanpub-markua-export-to-markua (&optional async subtreep visible-only)
  "Export current buffer to a Leanpub compatible Markua file.

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
  (let ((outfile (org-export-output-file-name ".markua" subtreep)))
    (org-export-to-file 'markua outfile async subtreep visible-only)))

;;;###autoload
(defun org-markua-publish-to-leanpub (plist filename pub-dir)
  "Publish an org file to leanpub in Markua format.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'markua filename ".markua" plist pub-dir))

(provide 'ox-leanpub-markua)

;;; ox-leanpub-markua.el ends here
