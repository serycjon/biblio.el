;;; biblio-dissemin.el --- Lookup and import bibliographic information and open access records from Dissemin -*- lexical-binding: t -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit-Claudel
;; Version: 0.1
;; Package-Requires: ((biblio-core "0.0"))
;; Keywords: bib, tex, convenience, hypermedia
;; URL: http://github.com/cpitclaudel/biblio.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Lookup publication records on Dissemin by DOI using `dissemin-lookup'.
;;
;; This package also plugs into `biblio-selection-mode' (provided by the
;; `biblio-core' package, by adding an entry to the extended actions menu (`o')
;; to quickly locate the Dissemin record of e.g. a CrossRef entry.

;;; Code:

(require 'browse-url)
(require 'biblio-core)

(defun biblio-dissemin--format-author (author)
  "Format a Dissemin AUTHOR entry."
  (let-alist author
    (format "%s %s" .name.first .name.last)))

(defun biblio-dissemin--browse-url (button)
  "Action for a URL BUTTON created by Dissemin."
  (browse-url (button-label button)))

(defun biblio-dissemin--insert-button (url prefix)
  "Insert a button pointing to URL, prefixed by PREFIX."
  (unless (seq-empty-p url)
    (insert "\n" prefix)
    (insert-text-button url
                        'follow-link t
                        'action #'biblio-dissemin--browse-url)))

(defun biblio-dissemin--insert-record (record)
  "Insert a Dissemin RECORD entry into the current buffer."
  (let-alist record
    (insert "\n\n")
    (biblio-with-fontification 'font-lock-preprocessor-face
      (biblio-insert-with-prefix ">> " .identifier))
    (biblio-dissemin--insert-button .pdf_url "   ")
    (biblio-dissemin--insert-button .splash_url "   ")
    (insert "\n")
    (biblio-with-fontification 'font-lock-doc-face
      (biblio-insert-with-prefix "   " .abstract))))

(defun dissemin--translate-classification (classification)
  "Translate Dissemin's CLASSIFICATION for display."
  (pcase classification
    (`"OA" "Available from the publisher")
    (`"OK" "Some versions may be shared by the author")
    (`"UNK" "Sharing policy is unclear")
    (`"CLOSED" "Subject to a restrictive sharing policy")
    (_ classification)))

(defun biblio-dissemin--pretty-print (paper)
  "Pretty-print a Dissemin PAPER entry to current buffer."
  (let-alist paper
    (biblio-insert-result
     (list (cons 'title .title)
           (cons 'authors
                 (apply #'biblio-join ", " "(no authors)"
                        (seq-map #'biblio-dissemin--format-author .authors)))
           (cons 'open-access-status
                 (dissemin--translate-classification .classification)))
     t)
    (biblio-dissemin--insert-button .pdf_url "")
    (if (seq-empty-p .records)
        (insert "\n\n(no records)")
      (seq-do #'biblio-dissemin--insert-record .records))))

(defun biblio-dissemin--print-results (paper)
  "Create a buffer for Dissemin, and print PAPER into it."
  (with-current-buffer (biblio-dissemin--make-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (help-mode)
      (visual-line-mode)
      (biblio-dissemin--pretty-print paper))
    (setq buffer-read-only t)
    (pop-to-buffer (current-buffer))))

(defun biblio-dissemin--make-buffer ()
  "Create a buffer to display Dissemin results in."
  (get-buffer-create "*Dissemin search results*"))

(defun biblio-dissemin--parse-buffer ()
  "Extract search results from DBLP response."
  (set-buffer-multibyte t) ;; URL buffer is unibyte
  (decode-coding-region (point-min) (point-max) 'utf-8)
  (let-alist (json-read)
    (unless (string= .status "ok")
      (error "Dissemin query failed"))
    .paper))

(defun biblio-dissemin--url (doi)
  "Create a DBLP url to look up DOI."
  (format "http://dissem.in/api/%s" (url-encode-url doi)))

(defun biblio-dissemin--callback (_buffer-or-errors)
  "Parse results returned by Dissemin."
  (biblio-dissemin--print-results (biblio-dissemin--parse-buffer)))

;;;###autoload
(defun dissemin-lookup (doi &optional cleanup)
  "Retrieve a record by DOI from Dissemin, and display it.
Interactively, or if CLEANUP is non-nil, pass DOI through
`biblio-cleanup-doi'."
  (interactive "MDOI: \nd")
  (when cleanup
    (setq doi (biblio-cleanup-doi doi)))
  (biblio-url-retrieve (biblio-dissemin--url doi)
                       (biblio-generic-url-callback #'biblio-dissemin--callback)))

(defun biblio-dissemin--lookup-record (record)
  "Retrieve a RECORD from Dissemin, and display it.
RECORD is a formatted record as expected by `biblio-lookup'."
  (let-alist record
    (if .doi (dissemin-lookup .doi)
      (error "No DOI found in this entry"))))

;;;###autoload
(defun biblio-dissemin--register-action ()
  "Add Dissemin to list of `biblio-selection-mode' actions."
  (add-to-list 'biblio-selection-mode-actions-alist
               '("Dissemin" . biblio-dissemin--lookup-record)))

;;;###autoload
(add-hook 'biblio-selection-mode-hook #'biblio-dissemin--register-action)

(provide 'biblio-dissemin)
;;; biblio-dissemin.el ends here
