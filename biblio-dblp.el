;;; biblio-dblp.el --- Lookup and import bibliographic entries from DBLP -*- lexical-binding: t -*-

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
;; Lookup and download bibliographic records from DBLP (a great source of
;; references for Computer Science papers) using `dblp-lookup'.
;;
;; This package uses `biblio-selection-mode', and is part of the more general
;; `biblio' package (which see for more documentation).

;;; Code:

(require 'biblio-core)

(defun biblio-dblp--forward-bibtex (identifier forward-to)
  "Get BibTeX for DBLP entry IDENTIFIER, and pass it to FORWARD-TO."
  (let ((url (replace-regexp-in-string "/rec/" "/rec/bib2/" identifier t t)))
    (biblio-url-retrieve url (biblio-generic-url-callback
                             (lambda (_buffer-or-errors)
                               "Parse DBLP BibTeX results."
                               (funcall forward-to (biblio-response-as-utf-8)))))))

(defun biblio-dblp--extract-interesting-fields (item)
  "Prepare a DBLP search result ITEM for display."
  (let-alist (biblio-alist-get 'info item)
    (list (cons 'identifier (cadr .url))
          (cons 'forward-bibtex-function #'biblio-dblp--forward-bibtex)
          (cons 'title (cadr .title))
          (cons 'authors (apply #'biblio-join ", " "(no authors)"
                                (seq-map #'cl-caddr (cdr .authors))))
          (cons 'container (cadr .venue))
          (cons 'references nil)
          (cons 'type (cadr .type))
          (cons 'url (cadr .url)))))

(defun biblio-dblp--hitp (item)
  "Check if ITEM is a DBLP hit."
  (eq (car-safe item) 'hit))

(defun biblio-dblp--parse-buffer ()
  "Extract search results from DBLP response."
  (set-buffer-multibyte t) ;; URL buffer is unibyte
  (decode-coding-region (point-min) (point-max) 'utf-8)
  (let-alist (car (xml-parse-region (point-min) (point-max)))
    (unless (string= (cadr .status) "OK")
      (error "Query failed with status %S" .status))
    (seq-map #'biblio-dblp--extract-interesting-fields (seq-filter #'biblio-dblp--hitp .hits))))

;;; Searching

(defun biblio-dblp--url (query)
  "Create a DBLP url to look up QUERY."
  (format "http://dblp.uni-trier.de/search/publ/api?q=%s&format=xml" (url-encode-url query)))

;;;###autoload
(defun dblp-lookup (query)
  "Look up QUERY on DBLP."
  (interactive (list (biblio-read-query "DBLP")))
  (biblio-lookup query #'biblio-dblp--url #'biblio-dblp--parse-buffer))

(provide 'biblio-dblp)
;;; biblio-dblp.el ends here
