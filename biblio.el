;;; biblio.el --- Browse and import bibliographic references from CrossRef, DBLP, dx.doi.org, and other sources, by DOI or by keywords -*- lexical-binding: t -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit-Claudel
;; Version: 0.1
;; Package-Requires: ((biblio-core "0.0") (biblio-doi "0.0") (biblio-dblp "0.0") (biblio-crossref "0.0") (biblio-dissemin "0.0"))
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
;; biblio.el
;;
;; Quick start: `M-x crossref-lookup'
;;
;; biblio.el makes it easy to browse and gather bibliographic references and
;; publications from various sources, by keywords or by DOI.  References are
;; automatically fetched from well-curated sources, and formatted as BibTeX.
;;
;; Supported sources:
;; * `dx.doi.org' (to retrieve BibTeX records from DOIs)
;; * `CrossRef' (for general searches)
;; * `DBLP' (specialized in computer science)
;; * `Dissemin' (to gather information about a particular publication, such as
;;   open acces status)
;;
;; Each of these sources can be accessed independently:
;; * `crossref-lookup' to query CrossRef
;; * `dblp-lookup' to query DBLP
;; * `doi-insert' to insert a BibTeX record by DOI
;; * `dissemin-lookup' to show information about the open access status a
;;   particular DOI
;;
;; Most of these commands work together: for example, `crossref-lookup' displays
;; a list of results in `biblio-selection-mode'.  In that mode, use:
;;
;; * `c' or `M-w' to copy a BibTeX record for the current entry
;; * `i' or `C-y' to insert a BibTex record for the current entry in source buffer.
;; * `o' to run an extended action, such as fetching a Dissemin record.

;;; Code:

(require 'biblio-core)
(require 'biblio-doi)
(require 'biblio-crossref)
(require 'biblio-dblp)
(require 'biblio-dissemin)

(provide 'biblio)
;;; biblio.el ends here
