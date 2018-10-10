;;; wiki-summary.el --- View Wikipedia summaries in Emacs easily.

;; Copright (C) 2015 Danny Gratzer <jozefg@cmu.edu>

;; Author: Danny Gratzer
;; URL: https://github.com/jozefg/wiki-summary.el
;; Keywords: wikipedia, utility
;; Package-Requires: ((emacs "24"))
;; Version: 0.1

;;; Commentary:

;; It's often the case when reading some document in Emacs (be it
;; code text or prose) that I come across a word or phrase that I
;; don't know. In order to simplify my feedback loop when wiki-summary
;; lets me look up something in a couple seconds.
;;
;; To use this package, simply call M-x wiki-summary (or bind it to a key).
;; This will prompt you for an article title to search. For convience,
;; this will default to the word under the point. When you hit enter
;; this will query Wikipedia and if an article is found, bring up the
;; title in a separate window. Spaces will be properly escaped so
;; something like "Haskell (programming language)" will bring up the
;; intended page.
;;
;; I'm not sure exactly what else people would want out of this package.
;; Feature request issues are welcome.

(require 'url)
(require 'json)
(require 'thingatpt)

(eval-when-compile
  ; This stops the compiler from complaining.
  (defvar url-http-end-of-headers))

;;;###autoload
(defun wiki-summary/make-api-query (s)
  "Given a wiki page title, generate the url for the API call
   to get the page info"
  (let ((pre "https://en.wikipedia.org/w/api.php?continue=&action=query&titles=")
        (post "&prop=extracts&exintro=&explaintext=&format=json&redirects")
        (term (url-hexify-string (replace-regexp-in-string " " "_" s))))
    (concat pre term post)))

;;;###autoload
(defun wiki-summary/extract-summary (resp)
  "Given the JSON reponse from the webpage, grab the summary as a string"
  (let* ((query (plist-get resp 'query))
         (pages (plist-get query 'pages))
         (info (cadr pages)))
    (plist-get info 'extract)))

;;;###autoload
(defun wiki-summary/format-summary-in-buffer (summary)
  "Given a summary, stick it in the *wiki-summary* buffer and display the buffer"
  (let ((buf (generate-new-buffer "*wiki-summary*")))
    (with-current-buffer buf
      (princ summary buf)
      (fill-paragraph)
      (goto-char (point-min))
      (text-mode)
      (read-only-mode))
    (display-buffer buf)))


;; Search functions begin here
;;;###autoload
(defun wiki-summary/make-api-search-query (s)
  "Given a search title, generate the url for the API call
   to return a list of wiki page titles"
  (let ((pre "https://en.wikipedia.org/w/api.php?continue=&action=query&list=search&srsearch=")
        (post "&prop=extracts&exintro=&explaintext=&format=json&redirects")
        (term (url-hexify-string s)))
    (concat pre term post)))

;;;###autoload
(defun wiki-summary/format-choice-text (tup)
  "Formatted text to offer to user, with snippets if variable set"
  (if wiki-summary-showsnippet
      (concat (tuple-title tup) " ::: " (tuple-summary tup))
    (tuple-title tup)))

;;;###autoload
(defun wiki-summary/clean-snippet (snippet)
  "Removes <span> tags and newlines from snippet text"
  (replace-regexp-in-string "\\(\n\\|\s\\)+" " "
    (replace-regexp-in-string "</span>" ""
      (replace-regexp-in-string "<span[^>]*>" "" snippet))))

;;;###autoload
(defun wiki-summary/extract-titleandsnippet (json)
  "Extracts title and snippet entries from search results"
  (make-tuple
   :title (plist-get json 'title)
   :snippet (clean-snippet (plist-get json 'snippet))))

;;;###autoload
(defun wiki-summary/offer-choices (resp)
  "Offers a choice of potential titles to the user"
  (let* ((query (plist-get resp 'query))
         (searches (plist-get query 'search))
         (choices (mapcar (lambda (x)
             (wiki-summary/format-choice-text (extract-titleandsnippet x))) searches)))
    (ido-completing-read "Matching Titles: " choices)))


;;;###autoload
(defun wiki-summary (s &optional level)
  "Return the wikipedia page's summary either by a directly
   provided title, or otherwise by searching for the title"
  (setq title s)
  (setq direct (if (> level 0) not d))
  (interactive
   (list
    (read-string
     (concat
      "Wikipedia Article"
      (if (thing-at-point 'word)
          (concat " (" (thing-at-point 'word) ")")
        "")
      ": ")
     nil
     nil
     (thing-at-point 'word))))
  (save-excursion
    (url-retrieve (if direct (wiki-summary/make-api-query title)
                    (wiki-summary/make-api-search-query title))
       (lambda (events)
         (message "") ; Clear the annoying minibuffer display
         (goto-char url-http-end-of-headers)
         (let ((json-object-type 'plist)
               (json-key-type 'symbol)
               (json-array-type 'vector))
           (let* ((result (json-read)))
             (if direct
                 (let ((summary (wiki-summary/extract-summary result)))
                   (if summary
                       (if (string-match-p "may refer to:" summary)
                           (progn
                             (message "Ambiguous Title, recurse with search")
                             (wiki-summary title (+ level 1)))
                         (wiki-summary/format-summary-in-buffer summary)) ;; Terminate with summary
                     (progn
                       (message "No title, recurse with search")
                       (wiki-summary title (+ level 1)))
                     ))
               (let* ((chosen (wiki-summary/offer-choices result)))
                 (if chosen
                     (progn
                       (message (concat "Chosen [" chosen "], recurse with title"))
                       (wiki-summary chosen 0))
                   (message "No article found"))
                   ))))))))

(provide 'wiki-summary)

;;; wiki-summary.el ends here
(wiki-summary "RNA Binding")

(let ((d 't))
  (> 1 (if (not d) 0 d)))
