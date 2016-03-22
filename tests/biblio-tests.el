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

(require 'biblio)
(require 'buttercup)

(defconst stallman-bibtex "Stallman_1981, title={EMACS the extensible,
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

(defun biblio--dummy-backend (&rest args)
  "Dummy backend to work around https://github.com/jorgenschaefer/emacs-buttercup/issues/52.")

(defconst sample-items
  '(((backend . biblio--dummy-backend)
     (title . "Who builds a house without drawing blueprints?")
     (authors "Leslie Lamport") (container . "Commun. ACM") (type . "Journal Articles")
     (url . "http://dblp.org/rec/journals/cacm/Lamport15"))
    ((backend . biblio--dummy-backend)
     (title . "Turing lecture: The computer science of concurrency: the early years.")
     (authors "Leslie Lamport") (container . "Commun. ACM") (type . "Journal Articles")
     (url . "http://dblp.org/rec/journals/cacm/Lamport15a"))
    ((backend . biblio--dummy-backend)
     (title . "An incomplete history of concurrency chapter 1. 1965-1977.")
     (authors "Leslie Lamport") (container . "PODC") (type . "Conference and Workshop Papers")
     (url . "http://dblp.org/rec/conf/podc/Lamport13"))
    ((backend . biblio--dummy-backend)
     (title . "Euclid Writes an Algorithm: A Fairytale.")
     (authors "Leslie Lamport") (container . "Int. J. Software and Informatics") (type . "Journal Articles")
     (url . "http://dblp.org/rec/journals/ijsi/Lamport11"))
    ((backend . biblio-crossref-backend)
     (title . "Fast Paxos") (authors "Leslie Lamport")
     (publisher . "Springer Science + Business Media") (container . ["Distrib. Comput." "Distributed Computing"])
     (references "10.1007/s00446-006-0005-x") (type . "journal-article")
     (doi . "10.1007/s00446-006-0005-x") (url . "http://dx.doi.org/10.1007/s00446-006-0005-x"))
    ((backend . biblio-crossref-backend)
     (title . "Brief Announcement: Leaderless Byzantine Paxos") (authors "Leslie Lamport")
     (publisher . "Springer Science + Business Media")
     (container . ["Lecture Notes in Computer Science" "Distributed Computing"])
     (references "10.1007/978-3-642-24100-0_10")
     (type . "book-chapter") (url . "http://dx.doi.org/10.1007/978-3-642-24100-0_10"))
    ((backend . biblio--dummy-backend)
     (title . "Implementing dataflow with threads.")
     (authors "Leslie Lamport") (container . "Distributed Computing") (type . "Journal Articles")
     (url . nil) (doi . "10.1007/s00446-008-0065-1"))
    ((backend . biblio--dummy-backend)
     (title . "The PlusCal Algorithm Language.")
     (authors "Leslie Lamport") (container . "ICTAC") (type . "Conference and Workshop Papers")
     (url . nil) (doi . nil))))

(describe "Unit tests:"
  (describe "In biblio's core,"
    (describe "in the compatibility section,"
      (let ((alist '((a . 1) (b . 2) (c . 3) (c . 4)))
            (plist '(a  1 b 2 c 3 c 4)))
        (describe "-alist-get"
          (it "can read values from alists"
            (expect (biblio-alist-get 'a alist) :to-equal 1)
            (expect (biblio-alist-get 'b alist) :to-equal 2)
            (expect (biblio-alist-get 'c alist) :to-equal 3)))
        (describe "-plist-to-alist"
          (it "can convert plists"
            (expect (biblio--plist-to-alist plist) :to-equal alist)))))
    (describe "in the utilities section,"
      (describe "-format-bibtex"
        (it "ignores invalid entries"
          (expect (biblio-format-bibtex "@!!") :to-equal "@!!")
          (expect (biblio-format-bibtex "@article{INVALID KEY,}") :to-equal "@article{INVALID KEY,}"))
        (it "formats a typical example properly"
          (expect (biblio-format-bibtex (concat "@ARTIcle{" stallman-bibtex))
                  :to-equal (concat "@Article{Stallman_1981," stallman-bibtex-clean)))
        (it "properly creates keys"
          (expect (biblio-format-bibtex (concat "@article{" stallman-bibtex) t)
                  :to-equal (concat "@Article{stallman81:emacs," stallman-bibtex-clean)))
        (it "replaces the “@data{” header"
          (expect (biblio-format-bibtex (concat "@data{" stallman-bibtex))
                  :to-match "@misc{")))
      (describe "-response-as-utf8"
        (it "decodes Unicode characters properly"
          (let ((unicode-str "É Ç € ← 有"))
            (with-temp-buffer
              (insert unicode-str)
              (goto-char (point-min))
              (set-buffer-multibyte nil)
              (expect (biblio-response-as-utf-8) :to-equal unicode-str)))))
      (describe "-check-for-retrieval-error"
        (let ((http-error '(error http 406))
              (timeout-error '(error url-queue-timeout "Queue timeout exceeded")))
          (it "supports empty lists"
            (expect (biblio-check-for-retrieval-error nil) :to-equal nil))
          (it "supports whitelists"
            (expect (biblio-check-for-retrieval-error `(:error ,http-error) '(http . 406))
                    :to-equal `((http . 406))))
          (it "handles timeouts specially"
            (let ((timeout-error-plist `(:error ,timeout-error)))
              (expect (biblio-check-for-retrieval-error timeout-error-plist)
                      :to-equal '(error . timeout))))
          (it "returns the first error"
            (expect (biblio-check-for-retrieval-error `(:error ,http-error :error ,timeout-error))
                    :to-equal `(error . (http . 406)))
            (expect (biblio-check-for-retrieval-error `(:error ,timeout-error :error ,http-error))
                    :to-equal `(error . timeout)))))
      (describe "-generic-url-callback"
        :var (source-buffer)
        (defun biblio--dummy-cleanup-func ())
        (defun biblio--dummy-callback (&optional arg) arg)
        (spy-on #'biblio--dummy-callback :and-call-through)
        (before-each
          (with-current-buffer (setq source-buffer (get-buffer-create " *url*"))
            (erase-buffer) ;; FIXME use an after-each form instead
            (insert "Some\npretty\nheaders\n\nAnd a response.")))
        (it "calls the cleanup function"
          (spy-on #'biblio--dummy-cleanup-func)
          (with-current-buffer source-buffer
            (funcall (biblio-generic-url-callback
                      #'ignore #'biblio--dummy-cleanup-func)
                     nil))
          (expect #'biblio--dummy-cleanup-func :to-have-been-called))
        (it "invokes its callback in the right buffer"
          (with-current-buffer source-buffer
            (expect (funcall (biblio-generic-url-callback
                              (lambda () (current-buffer)))
                             nil)
                    :to-equal source-buffer)))
        (it "puts the point in the right spot"
          (with-current-buffer source-buffer
            (expect (funcall (biblio-generic-url-callback
                              (lambda () (looking-at-p "And a response.")))
                             nil)
                    :to-be-truthy)))
        (it "Always kills the source buffer"
          (with-current-buffer source-buffer
            (funcall (biblio-generic-url-callback #'ignore) nil))
          (expect (buffer-live-p source-buffer) :not :to-be-truthy))
        (let ((errors '(:error (error http 406))))
          (it "stops when passed unexpected errors"
            (expect (shut-up
                      (funcall (biblio-generic-url-callback
                                #'biblio--dummy-callback)
                               errors)
                      (shut-up-current-output))
                    :to-match "Error")
            (expect #'biblio--dummy-callback :not :to-have-been-called))
          (it "forwards expected errors"
            (expect (funcall (biblio-generic-url-callback
                              #'biblio--dummy-callback #'ignore '(http . 406))
                             errors)
                    :to-equal '((http . 406)))
            (expect #'biblio--dummy-callback :to-have-been-called))))
      (describe "-cleanup-doi"
        (it "Handles prefixes properly"
          (expect (biblio-cleanup-doi "http://dx.doi.org/10.5281/zenodo.44331")
                  :to-equal "10.5281/zenodo.44331")
          (expect (biblio-cleanup-doi "http://doi.org/10.5281/zenodo.44331")
                  :to-equal "10.5281/zenodo.44331"))
        (it "trims spaces"
          (expect (biblio-cleanup-doi "   10.5281/zenodo.44331 \n\t\r ")
                  :to-equal "10.5281/zenodo.44331"))
        (it "doesn't change clean DOIs"
          (expect (biblio-cleanup-doi "10.5281/zenodo.44331")
                  :to-equal "10.5281/zenodo.44331")))
      (describe "-join"
        (it "removes empty entries before joining"
          (expect (biblio-join ", " "a" nil "b" nil "c" '[]) :to-equal "a, b, c")
          (expect (biblio-join-1 ", " '("a" nil "b" nil "c" [])) :to-equal "a, b, c"))))
    (describe "in the major mode help section"
      :var (temp-buf doc-buf)
      (before-each
        (with-current-buffer (setq temp-buf (get-buffer-create " *temp*"))
          (shut-up
            (biblio-selection-mode)
            (setq doc-buf (biblio--selection-help)))))
      (after-each
        (kill-buffer doc-buf)
        (kill-buffer temp-buf))
      (describe "--help-with-major-mode"
        (it "produces a live buffer"
          (expect (buffer-live-p doc-buf) :to-be-truthy))
        (it "shows bindings in order"
          (expect (with-current-buffer doc-buf
                    (and (search-forward "<up>" nil t)
                         (search-forward "<down>" nil t)))
                  :to-be-truthy))))
    (describe "in the interaction section,"
      :var (source-buffer selection-buffer)
      (before-each
        (shut-up
          (with-current-buffer (setq source-buffer (get-buffer-create " *selection*"))
            ;; This should be in an after-each (and it should include
            ;; -kill-buffers), but after-each is broken at the moment
            (erase-buffer))
          (setq selection-buffer (biblio-insert-results source-buffer "B" sample-items))))
      (describe "a motion command"
        (it "can go down"
          (with-current-buffer selection-buffer
            (dotimes (_ 2)
              (expect (point) :not :to-equal (biblio--selection-next)))
            (expect (biblio-alist-get 'title (biblio--selection-metadata-at-point))
                    :to-match "^An incomplete history ")))
        (it "cannot go beyond the end"
          (with-current-buffer selection-buffer
            (dotimes (_ 50)
              (biblio--selection-next))
            (expect (point) :to-equal (biblio--selection-next))))
        (it "can go up"
          (with-current-buffer selection-buffer
            (goto-char (point-max))
            (dotimes (_ (1- (length sample-items)))
              (expect (point) :not :to-equal (biblio--selection-previous))
              (expect (point) :not :to-equal (point-max)))
            (expect (biblio-alist-get 'title (biblio--selection-metadata-at-point))
                    :to-match "^Turing lecture")))
        (it "cannot go beyond the beginning"
          (with-current-buffer selection-buffer
            (goto-char (point-max))
            (dotimes (_ 50)
              (biblio--selection-previous))
            (expect (point) :to-equal 3)
            (expect (point) :to-equal (biblio--selection-previous)))))
      (describe "-get-url"
        (it "works on each item"
          (with-current-buffer selection-buffer
            (dotimes (_ (1- (length sample-items)))
              (expect (biblio-get-url (biblio--selection-metadata-at-point))
                      :to-match "^https?://")
              (expect (point) :not :to-equal (biblio--selection-next)))
            (expect (point) :to-equal (biblio--selection-next))))
        (it "uses DOIs if URLs are unavailable"
          (with-current-buffer selection-buffer
            (goto-char (point-max))
            (dotimes (_ 2) (biblio--selection-previous))
            (expect (biblio-get-url (biblio--selection-metadata-at-point))
                    :to-match "^https://doi.org/"))))
      (describe "a browsing command"
        (spy-on #'browse-url)
        (it "opens the right URL"
          (with-current-buffer selection-buffer
            (biblio--selection-browse)
            (expect 'browse-url
                    :to-have-been-called-with
                    "http://dblp.org/rec/journals/cacm/Lamport15")))
        (it "complains about missing URLs"
          (with-current-buffer selection-buffer
            (goto-char (point-max))
            (expect #'biblio--selection-browse :to-throw 'error)))
        (it "lets users click buttons"
          (with-current-buffer selection-buffer
            (expect (search-forward "http" nil t) :to-be-truthy)
            (push-button (point))
            (expect 'browse-url :to-have-been-called))))
      (describe "a selection command"
        (let ((bibtex "@article{empty}"))
          (spy-on #'biblio--dummy-backend
                  :and-call-fake
                  (lambda (_command _metadata forward-to)
                    (funcall forward-to bibtex)))
          (it "can copy bibtex records"
            (with-current-buffer selection-buffer
              (shut-up (biblio--selection-copy))
              (expect (car kill-ring) :to-equal bibtex)
              (expect #'biblio--dummy-backend :to-have-been-called)))
          (it "can copy bibtex records and quit"
            (with-current-buffer selection-buffer
              (shut-up (biblio--selection-copy-quit))
              (expect (car kill-ring) :to-equal bibtex)
              (expect #'biblio--dummy-backend :to-have-been-called)))
          (it "can insert bibtex records"
            (with-current-buffer selection-buffer
              (shut-up (biblio--selection-insert))
              (with-current-buffer source-buffer
                (expect (buffer-string) :to-equal (concat bibtex "\n\n")))
              (expect #'biblio--dummy-backend :to-have-been-called)))
          (it "can insert bibtex records and quit"
            (with-current-buffer selection-buffer
              (shut-up (biblio--selection-insert-quit))
              (with-current-buffer source-buffer
                (expect (buffer-string) :to-equal (concat bibtex "\n\n")))
              (expect #'biblio--dummy-backend :to-have-been-called)))
          (it "complains about empty entries"
            (with-temp-buffer
              (expect #'biblio--selection-copy :to-throw 'error)))))
      (describe "--selection-extended-action"
        (spy-on 'biblio-completing-read-alist
                :and-return-value #'biblio-dissemin--lookup-record)
        (it "runs an action as expected"
          (spy-on #'biblio-dissemin--lookup-record)
          (with-current-buffer selection-buffer
            (call-interactively #'biblio--selection-extended-action)
            (expect #'biblio-dissemin--lookup-record
                    :to-have-been-called-with
                    (biblio--selection-metadata-at-point))))
        (it "complains about missing entries"
          (with-temp-buffer
            (expect #'biblio--selection-extended-action :to-throw 'error))))
      (dolist (func '(biblio-completing-read biblio-completing-read-alist))
        (describe (format "%S" func)
          (spy-on #'completing-read)
          (it "uses ido by default"
            (let ((completing-read-function #'completing-read-default))
              (funcall func "A" nil)
              (expect #'completing-read :to-have-been-called)
              (expect (biblio--completing-read-function) :to-be #'ido-completing-read)))
          (it "respects users choices"
            (let ((completing-read-function #'ignore))
              (funcall func "A" nil)
              (expect #'completing-read :to-have-been-called)
              (expect (biblio--completing-read-function) :to-be #'ignore)))))
      (describe "-kill-buffers"
        (it "actually kills buffers"
          (biblio-kill-buffers)
          (expect (buffer-live-p selection-buffer) :not :to-be-truthy))))
    (describe "in the searching section,"
      (describe "--select-backend"
        (it "offers all backends"
          (spy-on #'biblio-completing-read-alist)
          (biblio--select-backend)
          (expect #'biblio-completing-read-alist
                  :to-have-been-called-with
                  "Backend: "
                  '(("arXiv" . biblio-arxiv-backend)
                    ("CrossRef" . biblio-crossref-backend)
                    ("DBLP" . biblio-dblp-backend))
                  nil t)))
      ;; (describe )
      ))

  (describe "In the arXiv module"
    (describe "biblio-arxiv--extract-year"
      (it "parses correct dates"
        (expect (biblio-arxiv--extract-year "2003-07-07T13:46:39")
                :to-equal "2003")
        (expect (biblio-arxiv--extract-year "2003-07-07T13:46:39-04:00")
                :to-equal "2003")
        (expect (biblio-arxiv--extract-year "1995-06-02T01:02:52+02:00")
                :to-equal "1995"))
      (it "rejects invalid dates"
        (expect (biblio-arxiv--extract-year "Mon Mar 21 19:24:32 EDT 2016")
                :to-equal nil)))))

(provide 'biblio-tests)
;;; biblio-tests.el ends here
