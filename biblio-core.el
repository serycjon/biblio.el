;;; biblio-core.el --- A framework for looking up and displaying bibliographic entries -*- lexical-binding: t -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit-Claudel
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (let-alist "1.0.4") (seq "1.11") (dash "2.12.1"))
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
;; A framework for browsing bibliographic search results.  This is the core
;; package; for user interfaces, see any of `biblio-crossref', `biblio-dblp',
;; `biblio-doi', and `biblio-dissemin', and the more general `biblio' package.

(require 'url-queue)
(require 'json)
(require 'dash)
(require 'let-alist)
(require 'seq)

;;; Code:

(defvar-local biblio--source-buffer nil
  "Buffer from which a bibliographic search was started.
This variable is local to eacg search results buffer.")

;;; Compatibility

(defun biblio-alist-get (key alist &optional default)
  "Copy of Emacs 25's `biblio-alist-get'.
Get the value associated to KEY in ALIST.  DEFAULT is the value
to return if KEY is not found in ALIST."
  (let ((x (assq key alist)))
    (if x (cdr x) default)))

(defun biblio--plist-to-alist (plist)
  "Copy of Emacs 25's `json--plist-to-alist'.
Return an alist of the property-value pairs in PLIST."
  (let (res)
    (while plist
      (let ((prop (pop plist))
            (val (pop plist)))
        (push (cons prop val) res)))
    (nreverse res)))

(defsubst biblio-string-join (strings &optional separator)
  "Copy of Emacs 24.5's `string-join'.
Join all STRINGS using SEPARATOR."
  (mapconcat 'identity strings separator))

;;; Utilities

(defun biblio--beginning-of-response-body ()
  "Move point to beginning of response body."
  (goto-char (point-min))
  (unless (re-search-forward "^\n" nil t)
    (error "Could not find body in response %S" (buffer-string))))

(defun biblio-response-as-utf-8 ()
  "Extract body of response."
  (set-buffer-multibyte t)
  (decode-coding-region (point) (point-max) 'utf-8 t))

(defun biblio--event-error-code (event)
  "Extract HTTP error code from EVENT, if any."
  (pcase event
    (`(:error . (error ,source ,details))
     (cons source details))))

(defun biblio-check-for-retrieval-error (events &rest allowed-errors)
  "Return list of errors in EVENTS.
If any of these errors is no in ALLOWED-ERRORS, signal an error."
  (message "Got events %S" (biblio--plist-to-alist events))
  (let ((errors (delq nil (mapcar #'biblio--event-error-code (biblio--plist-to-alist events)))))
    (dolist (err errors)
      (unless (or (eq (car err) 'url-queue-timeout) (member err allowed-errors))
        (error "Error %S while retrieving URL" err)))
    errors))

(defun biblio-generic-url-callback (callback &optional cleanup-function allowed-errors)
  "Make an `url'-ready callback from CALLBACK.
CALLBACK is called wit one argument, a buffer containing the
server's response (that buffer is current at the time of the
call, and killed after the call returns).  Call CLEANUP-FUNCTION
before checking for errors.  If the request returns one of the
errors in ALLOWED-ERRORS, CALLBACK is called with the list of
alowed errors that occured instead of a buffer.  If the request
returns another error, an exception is raised."
  (lambda (events)
    (let ((source-buffer (current-buffer)))
      (unwind-protect
          (progn
            (funcall (or cleanup-function #'ignore))
            (-if-let* ((errors (biblio-check-for-retrieval-error events allowed-errors)))
                (funcall callback errors)
              (biblio--beginning-of-response-body)
              (delete-region (point-min) (point))
              (funcall callback (current-buffer))))
        (kill-buffer source-buffer)))))

(defun biblio-url-retrieve (url callback)
  "Wrapper around `url-queue-retrieve'.
URL and CALLBACK; see `url-queue-retrieve'"
  (setq url-queue-timeout 1)
  (message "Fetching %s" url)
  (url-queue-retrieve url callback))

(defun biblio-strip (str)
  "Remove spaces surrounding STR."
  (->> str
       (replace-regexp-in-string "[ \t\n\r]+\\'" "")
       (replace-regexp-in-string "\\`[ \t\n\r]+" "")))

(defun biblio-cleanup-doi (doi)
  "Cleanup DOI string."
  (biblio-strip (replace-regexp-in-string "https?://dx.doi.org/" "" doi)))

;;; Interaction

(defconst biblio--search-result-marker-regexp "^> "
  "Indicator of a search result.")

(defun biblio--selection-move (move-fn search-fn)
  "Move using MOVE-FN, then call SEARCH-FN and go to first match."
  (let ((target))
    (save-excursion
      (funcall move-fn)
      (funcall search-fn biblio--search-result-marker-regexp nil t)
      (setq target (match-end 0)))
    (goto-char target)))

(defun biblio--selection-next ()
  "Move to next seach result."
  (interactive)
  (biblio--selection-move #'end-of-line #'re-search-forward))

(defun biblio--selection-previous ()
  "Move to previous seach result."
  (interactive)
  (biblio--selection-move #'beginning-of-line #'re-search-backward))

(defun biblio--selection-copy-callback (bibtex entry)
  "Add BIBTEX (from ENTRY) to kill ring."
  (kill-new bibtex)
  (message "Killed bibtex entry for %S." (biblio-alist-get 'title entry)))

(defun biblio--selection-copy ()
  "Copy BibTeX of entry at point."
  (interactive)
  (biblio--selection-forward-bibtex #'biblio--selection-copy-callback))

(defun biblio--selection-copy-quit ()
  "Copy BibTeX of entry at point and quit results."
  (interactive)
  (biblio--selection-forward-bibtex #'biblio--selection-copy-callback t))

(defun biblio--source-window ()
  "Get the window of the source buffer."
  (get-buffer-window biblio--source-buffer))

(defun biblio--selection-insert-callback (bibtex entry)
  "Add BIBTEX (from ENTRY) to kill ring."
  (let ((source-buffer biblio--source-buffer))
    (with-selected-window (or (biblio--source-window) (selected-window))
      (with-current-buffer source-buffer
        (insert bibtex "\n\n"))))
  (message "Inserted bibtex entry for %S." (biblio-alist-get 'title entry)))

(defun biblio--selection-insert ()
  "Insert BibTeX of entry in source buffer."
  (interactive)
  (biblio--selection-forward-bibtex #'biblio--selection-insert-callback))

(defun biblio--selection-insert-quit ()
  "Insert BibTeX of entry in source buffer and quit results."
  (interactive)
  (biblio--selection-forward-bibtex #'biblio--selection-insert-callback t))

(defun biblio--selection-metadata-at-point ()
  "Return the metadata of the entry at point."
  (get-text-property (point) 'biblio-metadata))

(defun biblio--selection-forward-bibtex (forward-to &optional quit)
  "Retrieve BibTeX for entry at point and pass it to FORWARD-TO.
If QUIT is set, also kill the results buffer."
  (-if-let* ((metadata (biblio--selection-metadata-at-point))
             (results-buffer (current-buffer)))
      (let-alist metadata
        (funcall .forward-bibtex-function .identifier
                 (lambda (bibtex)
                   (with-current-buffer results-buffer
                     (funcall forward-to bibtex metadata))))
        (when quit (quit-window)))
    (error "No entry at point")))

(defvar biblio-selection-mode-actions-alist nil
  "An alist of extensions for `biblio-selection-mode'.
Each element should be in the for (LABEL . FUNCTION); FUNCTION
will be called with the metadata of the current item.")

(defun biblio--read-selection-extensible-action ()
  "Read an action from `biblio-selection-mode-actions-alist'."
  (list (biblio-alist-get
         (completing-read "Action: " biblio-selection-mode-actions-alist nil t)
         biblio-selection-mode-actions-alist)))

(defun biblio--selection-extensible-action (action)
  "Run ACTION with metadata of current entry.
Interactively, query for ACTION from
`biblio-selection-mode-actions-alist'."
  (interactive (biblio--read-selection-extensible-action))
  (-if-let* ((metadata (biblio--selection-metadata-at-point)))
      (funcall action metadata)
    (user-error "No entry at point")))

(defvar biblio-selection-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<down>") #'biblio--selection-next)
    (define-key map (kbd "C-n") #'biblio--selection-next)
    (define-key map (kbd "<up>") #'biblio--selection-previous)
    (define-key map (kbd "C-p") #'biblio--selection-previous)
    (define-key map (kbd "c") #'biblio--selection-copy)
    (define-key map (kbd "M-w") #'biblio--selection-copy)
    (define-key map (kbd "C") #'biblio--selection-copy-quit)
    (define-key map (kbd "C-w") #'biblio--selection-copy-quit)
    (define-key map (kbd "i") #'biblio--selection-insert)
    (define-key map (kbd "I") #'biblio--selection-insert-quit)
    (define-key map (kbd "C-y") #'biblio--selection-insert-quit)
    (define-key map (kbd "o") #'biblio--selection-extensible-action)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keybindings for Bibliographic search results.")

(define-derived-mode biblio-selection-mode fundamental-mode "Bibliographic search results"
  "Browse bibliographic search results.
\\{biblio-selection-mode-map}"
  (hl-line-mode)
  (setq truncate-lines nil)
  (visual-line-mode)
  (setq-local cursor-type nil))

;;; Printing search results

(defun biblio-parenthesize (str)
  "Add parentheses to STR, if not empty."
  (if (seq-empty-p str) ""
    (concat "(" str ")")))

(defun biblio-remove-empty (strs)
  "Remove empty sequences from STRS."
  (seq-remove #'seq-empty-p strs))

(defmacro biblio--with-text-property (prop value &rest body)
  "Set PROP to VALUE on text inserted by BODY."
  (declare (indent 2)
           (debug t))
  (let ((beg-var (make-symbol "beg")))
    `(let ((,beg-var (point)))
       ,@body
       (put-text-property ,beg-var (point) ,prop ,value))))

(defmacro biblio-with-fontification (face &rest body)
  "Apply FACE to text inserted by BODY."
  (declare (indent 1)
           (debug t))
  `(biblio--with-text-property 'face ,face
     ,@body))

(defun biblio-join (sep fallback &rest strs)
  "Join STRS with SEP, unless empty; if empty, return FALLBACK."
  (let ((strs (biblio-remove-empty strs)))
    (if (seq-empty-p strs)
        fallback
      (biblio-string-join strs sep))))

(defun biblio-insert-with-prefix (prefix &rest strs)
  "Like INSERT with PREFIX and STRS, but set `wrap-prefix'.
That is, the inserted text gets a `wrap-prefix' made of enough
white space to align with the end of PREFIX."
  (biblio--with-text-property 'wrap-prefix (make-string (length prefix) ?\s)
    (apply #'insert prefix strs)))

(defun biblio--insert-detail (prefix items newline)
  "Insert PREFIX followed by ITEMS, if ITEMS is non-empty.
If ITEMS is a list or vector, join its entries with “, ”.
If NEWLINE is non-nil, add a newline before the main text."
  (unless (seq-empty-p items)
    (when newline
      (insert "\n"))
    (biblio-insert-with-prefix
     prefix (cond
             ((or (vectorp items) (listp items))
              (biblio-string-join items ", "))
             (t items)))))

(defun biblio-insert-result (item &optional no-sep)
  "Print a (prepared) bibliographic search result ITEM.
See also `crossref--extract-interesting-fields' and
`dblp--extract-interesting-fields'.  With NO-SEP, do not add
space after the record."
  (biblio--with-text-property 'biblio-metadata item
    (let-alist item
      (biblio-with-fontification 'font-lock-function-name-face
        (biblio-insert-with-prefix "> " .title))
      (insert "\n")
      (biblio-with-fontification 'font-lock-doc-face
        (biblio-insert-with-prefix "  " .authors))
      (biblio-with-fontification 'font-lock-comment-face
        (biblio--insert-detail "  In: " .container t)
        (biblio--insert-detail "  Publisher: " .publisher t)
        (biblio--insert-detail "  References: " .references t)
        (biblio--insert-detail "  URL: " .url t)
        (biblio--insert-detail "  Open Access: " .open-access-status t))
      (unless no-sep
        (insert "\n\n")))))

(defun biblio--make-buffer (source-buffer)
  "Create or retrieve the search results buffer for SOURCE-BUFFER."
  (get-buffer-create (format "*References search started from %s*" (buffer-name source-buffer))))

(defun biblio-insert-results (source-buffer items)
  "Create a results buffer for SOURCE-BUFFER and print ITEMS in it."
  (with-current-buffer (biblio--make-buffer source-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (seq-do #'biblio-insert-result items)
      (goto-char (point-min))
      (biblio-selection-mode))
    (setq-local biblio--source-buffer source-buffer)
    (setq buffer-read-only t)
    (current-buffer)))

(defun biblio--callback (source-buffer parse-buffer-function)
  "Generate a search results callback for SOURCE-BUFFER.
Results are parsed with PARSE-BUFFER-FUNCTION.  It should be a
function taking two arguments delimiting a region, and returning
a list of results."
  (biblio-generic-url-callback
   (lambda (_buffer-or-errors)
     "Parse results of bibliographic search."
     (->> (funcall parse-buffer-function)
          (biblio-insert-results source-buffer)
          (pop-to-buffer)))))

;;; Searching

(defvar biblio--search-history nil)

(defun biblio-read-query (source-name)
  "Read a search query, prompting with SOURCE-NAME."
  (read-string (format "%s query: " source-name) nil 'biblio--search-history))

(defun biblio-lookup (query url-function parse-buffer-function)
  "Lookup QUERY.
Format query with (URL-FUNCTION query), and parse results using
PARSE-BUFFER-FUNCTION (as described in `biblio--callback')"
  (biblio-url-retrieve (funcall url-function query)
                       (biblio--callback (current-buffer) parse-buffer-function)))

(provide 'biblio-core)
;;; biblio-core.el ends here
