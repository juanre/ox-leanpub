;;; ox-leanpub-book.el --- Export an org-file to LeanPub's multifile setup  -*- lexical-binding: t; -*-

;;; Copyright (C) 2019  Zamboni Diego

;;; Author: Zamboni Diego <diego@zzamboni.org>
;;; Keywords: org, leanpub

;;; Commentary:

;;; (based originally on code by Lakshmi Narasimhan, published at
;;; https://medium.com/@lakshminp/publishing-a-book-using-org-mode-9e817a56d144,
;;; but heavily modified)
;;;
;;; New export backend called leanpub-book, which creates a new
;;; export section called "LeanPub Book" with the following items:
;;;
;;; - "Multifile Export", which exports the whole book using default
;;;   settings (see below);
;;;
;;; - "Multifile Export (subset)", which creates only the Subset.txt file;
;;;
;;; - "Export current chapter" to explicitly export only the current
;;;   chapter to its own file. This also updates Subset.txt, so it can be
;;;   used to preview the current chapter without having to set
;;;   `#+LEANPUB_WRITE_SUBSET: current'.
;;;
;;; The export splits chapters into files by top-level headline, and
;;; automatically populates the `Book.txt', `Sample.txt' and
;;; `Subset.txt' files used by LeanPub.
;;;
;;; The exported files are written to the `manuscript/' subdirectory
;;; by default, which is what LeanPub expects. This default allows
;;; you to keep the book's main `org' file in the top-level directory
;;; of a repository, and to automatically write the output files to
;;; `manuscript/' so that LeanPub can process them. The output
;;; directory can be changed using the `#+LEANPUB_OUTPUT_DIR' file
;;; property, for example if you want to export to the current
;;; directory, you can use:
;;;
;;;   #+LEANPUB_OUTPUT_DIR: .
;;;
;;; The book files are populated as follows:
;;;
;;; - `Book.txt' with all chapters, except those tagged with `noexport'.
;;; - `Sample.txt' with all chapters tagged with `sample'.
;;; - `Subset.txt' with chapters depending on the value of the
;;;   `#+LEANPUB_WRITE_SUBSET' file property (if set):
;;;   - `none' (default): not created.
;;;   - `tagged': use all chapters tagged `subset'.
;;;   - `all': use the same chapters as `Book.txt'.
;;;   - `sample': use same chapters as `Sample.txt'.
;;;   - `current': export the current chapter (where the cursor is at the
;;;     moment of the export) as the contents of `Subset.txt'.
;;;
;;; If a heading has the `frontmatter', `mainmatter' or `backmatter' tags,
;;; the corresponding markup is inserted in the output, before the
;;; headline. This way, you only need to tag the first chapter of the
;;; front, main, and backmatter, respectively.
;;;
;;; Each section's headline is exported as part of its corresponding
;;; output file.

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-leanpub)
(require 'ob-core)

(org-export-define-derived-backend 'leanpub-book 'leanpub
  :menu-entry
  '(?B "LeanPub Book"
       ((?b "Whole book" (lambda (a s v b) (org-leanpub-export-book a s v b)))
        (?s "Subset" (lambda (a s v b) (org-leanpub-export-book a s v b t)))
        (?c "Current chapter" (lambda (a s v b) (org-leanpub-export-book a s v b t "current")))))
  :options-alist
  '((:leanpub-book-output-dir   "LEANPUB_BOOK_OUTPUT_DIR"   nil "manuscript" t)
    (:leanpub-book-write-subset "LEANPUB_BOOK_WRITE_SUBSET" nil "none"       t)))

(defun org-leanpub-export-book (&optional async subtreep visible-only body-only only-subset subset-type)
  "Export buffer to a Leanpub book, splitting by top-level headline and populating the corresponding book-specification files."
  (interactive)
  (let* ((info (org-combine-plists
                (org-export--get-export-attributes
                 'leanpub-book subtreep visible-only)
                (org-export--get-buffer-attributes)
                (org-export-get-environment 'leanpub-book subtreep)))
         (outdir (plist-get info :leanpub-book-output-dir))
         (do-subset (or subset-type (plist-get info :leanpub-book-write-subset)))
         (matter-tags '("frontmatter" "mainmatter" "backmatter"))
         (original-point (point)))
    ;; Relative pathname given the basename of a file, including the correct output dir
    (fset 'outfile (lambda (f) (concat outdir "/" f)))
    ;; delete all these files, they get recreated as needed
    (dolist (fname (mapcar (lambda (s) (concat s ".txt"))
                           (append (if only-subset '("Subset") '("Book" "Sample" "Subset"))
                                   matter-tags)))
      (delete-file (outfile fname)))
    (save-mark-and-excursion
      (org-map-entries
       (lambda ()
         (when (org-at-heading-p)
           (let* ((current-subtree (org-element-at-point))
                  (id (or (org-element-property :name      current-subtree)
                          (org-element-property :ID        current-subtree)
                          (org-element-property :CUSTOM_ID current-subtree)))
                  (level (nth 1 (org-heading-components)))
                  (tags (org-get-tags))
                  (title (or (nth 4 (org-heading-components)) ""))
                  (basename (concat (replace-regexp-in-string " " "-" (downcase (or id title)))
                                    ".md"))
                  (filename (outfile basename))
                  (stored-filename (org-entry-get (point) "EXPORT_FILE_NAME"))
                  (point-in-subtree (<= (org-element-property :begin current-subtree)
                                        original-point
                                        (org-element-property :end current-subtree)))
                  (is-subset (or (equal do-subset "all")
                                 (and (equal do-subset "tagged") (member "subset" tags))
                                 (and (equal do-subset "sample") (member "sample" tags))
                                 (and (equal do-subset "current") point-in-subtree))))
             (fset 'add-to-bookfiles
                   (lambda (line &optional always)
                     (let ((line-n (concat line "\n")))
                       (unless only-subset
                         (append-to-file line-n nil (outfile "Book.txt")))
                       (when (and (not only-subset) (or (member "sample" tags) always))
                         (append-to-file line-n nil (outfile "Sample.txt")))
                       (when (and (not (string= do-subset "none")) (or is-subset always))
                         (append-to-file line-n nil (outfile "Subset.txt"))))))
             (when (= level 1) ;; export only first level entries
               ;; add appropriate tag for front/main/backmatter for tagged headlines
               (dolist (tag matter-tags)
                 (when (member tag tags)
                   (let* ((fname (concat tag ".txt")))
                     (append-to-file (concat "{" tag "}\n") nil (outfile fname))
                     (add-to-bookfiles fname t))))
               ;; add to the filename to Book.txt and to Sample.txt "sample" tag is found.
               (add-to-bookfiles (file-name-nondirectory filename))
               (when (or (not only-subset)
                         is-subset)
                 ;; set filename only if the property is missing.
                 ;; If present, we assume its value is the correct one
                 (or stored-filename
                     (org-entry-put (point) "EXPORT_FILE_NAME" filename))
                 ;; select the subtree so that its headline is also exported
                 ;; (otherwise we get just the body)
                 (org-mark-subtree)
                 (message (format "Exporting %s (%s)" filename title))
                 (org-leanpub-export-to-markdown nil t)))))) "-noexport"))
    (message (format "LeanPub export to %s/ finished" outdir))))

(provide 'ox-leanpub-book)
