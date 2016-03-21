;;; biblio-tests.el --- Tests for the biblio package -*- lexical-binding: t -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit-Claudel
;; Version: 0.1
;; Package-Requires: ((biblio-core "0.0") (biblio-doi "0.0"))
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

;;; Code:

(when (require 'undercover nil t)
  (undercover "*.el"))

(require 'biblio-core)
(require 'buttercup)

(defconst stallman-bibtex "@ARTIcle{Stallman_1981, title={EMACS the extensible,
customizable self-documenting display editor}, volume={2},
ISSN=\"0737-819X\",
url={http://dx.doi.org/10.1145/1159890.806466},
DOI={10.1145/1159890.806466}, number={1-2}, journal={ACM SIGOA
Newsletter}, publisher={Association for Computing
Machinery (ACM)}, author={Stallman, Richard M.}, year={1981},
month={Apr}, pages={147–156}}")

(defconst stallman-bibtex-clean "
  author       = {Stallman, Richard M.},
  title	       = {EMACS the extensible, customizable self-documenting
                  display editor},
  year	       = 1981,
  volume       = 2,
  number       = {1-2},
  month	       = {Apr},
  pages	       = {147–156},
  issn	       = {0737-819X},
  doi	       = {10.1145/1159890.806466},
  url	       = {http://dx.doi.org/10.1145/1159890.806466},
  journal      = {ACM SIGOA Newsletter},
  publisher    = {Association for Computing Machinery (ACM)}
}")

(describe "In biblio's core"
  (describe "in the compatibility section"
    (let ((alist '((a . 1) (b . 2) (c . 3) (c . 4)))
          (plist '(a  1 b 2 c 3 c 4)))
      (describe "`biblio-alist-get'"
        (it "can read values from alists"
          (expect (biblio-alist-get 'a alist) :to-equal 1)
          (expect (biblio-alist-get 'b alist) :to-equal 2)
          (expect (biblio-alist-get 'c alist) :to-equal 3)))
      (describe "`biblio-plist-to-alist'"
        (it "can convert plists"
          (expect (biblio--plist-to-alist plist) :to-equal alist))))
    (describe "`biblio-join'"
      (it "removes empty entries before joining"
        (expect (biblio-join ", " "a" nil "b" nil "c" '[]) :to-equal "a, b, c")
        (expect (biblio-join-1 ", " '("a" nil "b" nil "c" [])) :to-equal "a, b, c"))))
  (describe "in the utilities section"
    (describe "`biblio-format-bibtex'"
      (xit "does not throw on invalid entries"
        (expect (biblio-format-bibtex "@!!") :to-equal "@!!")
        (expect (biblio-format-bibtex "@article{KEY,}") :to-equal "@article{}"))
      (it "formats a typical example properly"
        (expect (biblio-format-bibtex stallman-bibtex)
                :to-equal (concat "@Article{Stallman_1981," stallman-bibtex-clean)))
      (it "properly creates missing keys"
        (expect (biblio-format-bibtex stallman-bibtex t)
                :to-equal (concat "@Article{stallman81:emacs," stallman-bibtex-clean))))))

(provide 'biblio-tests)
;;; biblio-tests.el ends here
