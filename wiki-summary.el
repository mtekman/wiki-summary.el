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

(defcustom wiki-summary-language-string "en"
  "Language string for the API URL call, i.e.: 'en', 'fr', etc.")

(defvar wiki--pre-url-query-format-string
  "https://%s.wikipedia.org/w/api.php?continue=&action=query&list=search&srsearch=")

(defvar wiki--pre-url-format-string
  "https://%s.wikipedia.org/w/api.php?continue=&action=query&titles=")

(defvar wiki--post-url-format-string
  "&prop=extracts&exintro=&explaintext=&format=json&redirects")

(defvar wiki-summary-showsnippet 'nil)


;;;###autoload
(defun wiki-summary/make-api-query (s)
  "Given a wiki page title, generate the url for the API call
   to get the page info"
  (let ((pre (format wiki--pre-url-format-string wiki-summary-language-string))
        (post wiki--post-url-format-string)
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
      (view-mode))
    (pop-to-buffer buf)))

;;;###autoload
(defun wiki-summary/format-summary-into-buffer (summary buffer)
  "Given a summary, stick it in the *wiki-summary* buffer and display the buffer"
  (let ((this-buffer (get-buffer buffer)))
    (with-current-buffer (get-buffer this-buffer)
      (barf-if-buffer-read-only)
      (insert summary)
      (fill-paragraph))
    (display-buffer (get-buffer this-buffer))))


;; Search functions begin here
;;;###autoload
(defun wiki-summary/make-api-search-query (s)
  "Given a search title, generate the url for the API call
   to return a list of wiki page titles"
  (let ((pre (format wiki--pre-url-query-format-string wiki-summary-language-string))
        (post wiki--post-url-format-string)
        (term (url-hexify-string (replace-regexp-in-string " " "_" s))))
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


(cl-defstruct tuple title snippet)

;;;###autoload
(defun wiki-summary/extract-titleandsnippet (json)
  "Extracts title and snippet entries from search results"
  (make-tuple
   :title (plist-get json 'title)
   :snippet (wiki-summary/clean-snippet (plist-get json 'snippet))))

;;;###autoload
(defun wiki-summary/offer-choices (resp)
  "Offers a choice of potential titles to the user"
  (let* ((query (plist-get resp 'query))
         (searches (plist-get query 'search))
         (choices (mapcar
                   (lambda (x)
                     (wiki-summary/format-choice-text
                      (wiki-summary/extract-titleandsnippet x))) searches)))
    (ido-completing-read "Matching Titles: " choices)))

;;;###autoload
(defun wiki-summary-insert (s)
  "Return the wikipedia page's summary for a term"
  (interactive
   (list
    (read-string (concat
                  "Wikipedia Article"
                  (if (thing-at-point 'word)
                      (concat " (" (thing-at-point 'word) ")")
                    "")
                  ": ")
                 nil
                 nil
                 (thing-at-point 'word))))
  (save-excursion
    (url-retrieve
     (wiki-summary/make-api-query s)
     (lambda (events buf)
       (message "") ; Clear the annoying minibuffer display
       (goto-char url-http-end-of-headers)
       (let ((json-object-type 'plist)
             (json-key-type 'symbol)
             (json-array-type 'vector))
         (let* ((result (json-read))
                (summary (wiki-summary/extract-summary result)))
           (if (not summary)
               (message "No article found")
             (wiki-summary/format-summary-into-buffer summary buf)))))
     (list (buffer-name (current-buffer))))))

;;;###autoload
(defun wiki-summary/message-with-callback (mess timeout &optional callback)
  "Provide a quick message to the user for a specified interval,
   and then execute callback/recursion"
  (lambda () (message mess) (sit-for timeout) callback))


;;;###autoload
(defun wiki-summary (s &optional lev)
  "Return the wikipedia page's summary either by a directly
   provided title, or otherwise by searching for the title"
  (setq title s)
  (setq level (if lev lev 0))
  (setq direct (eq level 0))
  (if (>= level 5)
      (wiki-summary/message-with-callback "max recursion, quitting." 2)
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
      (url-retrieve
       (if direct (wiki-summary/make-api-query title)
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
                           (wiki-summary/message-with-callback
                            "ambiguous title, recurse with search" 1
                            (wiki-summary title (+ level 1)))
                         (wiki-summary/format-summary-in-buffer summary)) ;; Terminate with summary
                     (wiki-summary/message-with-callback
                      "no article found, recurse with search" 1
                      (wiki-summary title (+ level 1)))))
               (let* ((chosen (wiki-summary/offer-choices result)))
                 (if chosen
                     (wiki-summary/message-with-callback
                      (concat "chosen article [" chosen "], recurse with title") 2
                      (wiki-summary chosen))
                   (message "no article found in search, quitting.")))))))))))

(provide 'wiki-summary)

;;; wiki-summary.el ends here
;; --- tests ----
;; (wiki-summary "RNA")           ;; Direct title
;; (wiki-summary "RNA Binding")   ;; No direct title
;; (wiki-summary "Tony")          ;; Ambiguous title
