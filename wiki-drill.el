;;; wiki-drill.el --- Generate org-drill cards from wikipedia summaries

;; Copright (C) 2018 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/wiki-drill.el
;; Keywords: wikipedia, utility
;; Package-Requires: ((emacs "24"))
;; Version: 0.1

;;; Commentary:

;; Often when one skims over a wikipedia article, the information remembered in
;; the short time span is not preserved. This package uses Danny Gratzer's
;; excellent wiki-summary tool and converts the output into drill format.

;; To use this package, simply call M-x wiki-drill (or bind it to a key).
;; This will prompt you for an article title to search.

(require 'wiki-summary)


;; --- File operations
(defcustom wiki-drill--file "~/wiki-drill-inputs.org"
  "File to temprarily store drill entries. It is then up to the user to refile these entries.")
(defun wiki-drill/drill-file-touch ()
  "Touches drill file, creates it if not there"
  (if (not (file-exists-p wiki-drill--file))
      (write-region "" nil wiki-drill--file)))


;; -- FlashCard buffer
(defcustom wiki-drill--flashcardbuffer "*FlashCard*"
  "Buffer name for flashcard")
(defun wiki-drill/get-flashcard-buffer ()
  "Returns (and creates) the flashcard buffer"
  (if (get-buffer wiki-drill--flashcardbuffer)
      (get-buffer wiki-drill--flashcardbuffer)
    (generate-new-buffer wiki-drill--flashcardbuffer)))

;; --- Clozer Flashcard Functions
(defcustom wiki-drill--custom-clozer '("test")
  "A list of custom clozer types provided by the user")
(defun wiki-drill/offer-clozer-choices ()
  "Offers a choice of clozer categories to the user"
  (let ((choices
         (append
         '("hide1cloze"      ;; hides 1 at random, shows all others
           "show1cloze"      ;; shows 1 at random, hides all others
           "hide2cloze"      ;; hides 2 at random, shows all others
           "show2cloze"      ;; shows 2 at random, hides all others
           "hide1_firstmore" ;; hides 1st 75% of the time, shows all others
           "show1_firstless" ;; shows 1st 25% of the time, hides all others
           "show1_lastmore") ;; shows last 75% of the time, hides all others
         wiki-drill--custom-clozer)))
    (ido-completing-read "Clozer Types:" choices)))
(defun wiki-drill/make-flash-clozer-header (clozer-type)
  "Makes the PROPERTIES header part of a clozer flashcard
   using a selection of types offered to the user"
  (format ":PROPERTIES:\n:DRILL_CARD_TYPE:%s\n:END:" clozer-type))
(defun wiki-drill/make-flash-clozer ()
  "Pull text from wiki-summary into FlashCard, let the user mark words"
  (let* ((flashbuff (wiki-drill/get-flashcard-buffer)))
    (sit-for 0.1)
    (with-current-buffer "*wiki-summary*"
      (setq inhibit-read-only t)
      (let ((comment-str
             (concat ";; Mark keywords or regions with brackets ("
                     wiki-drill--binding-clozer-mark
                     "), and leave hints with\n;; bars, submit with ("
                     wiki-drill--binding-submit
                     ")   e.g. [hide these words||drop this hint]\n\n")))
        (put-text-property 0 (length comment-str) 'face 'font-lock-comment-face comment-str)
        (insert comment-str))
      (buffer-swap-text buf)      ;; switch text to that of flashcard
      (pop-to-buffer buf)
      (clozer-mode 1))))


;; --- Clozer Mode Functions --
(defcustom wiki-drill--binding-clozer-mark "C-b"
  "Binding to mark words or phrases in clozer minor mode")
(defcustom wiki-drill--binding-submit "C-c C-c"
  "Binding to submit flashcard")
(defun wiki-drill/clozer-brackets ()
  "Surrounds with [[words||hint]]"
  (let (pos1 pos2 bds)
    (if (use-region-p)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))
    (goto-char pos2) (insert "||<hint>]")
    (goto-char pos1) (insert "[")
    (goto-char (+ pos2 4))))
(define-minor-mode clozer-mode
  "Toggles clozer bracketing mode"
  :init-value nil
  :lighter " clozer"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd wiki-drill--binding-clozer-mark)
              (lambda () (interactive) (wiki-drill/clozer-brackets)))
            (define-key map (kbd wiki-drill--binding-submit)
              (lambda () (interactive) (wiki-drill/clozer-submit)))
            ;;                 (define-key map (kbd "RET") 'clozer-mode)
            map))
(defun wiki-drill/generate-clozer ()
  "Generate Clozer text"
  (message "clozer - not now")
  (let* ((clozer-type (wiki-drill/offer-clozer-choices))
         (clozer-head (wiki-drill/make-flash-clozer-header clozer-type))
         (flash-text (wiki-drill/get-flashcard-text))
         (total-text subject clozer-head flash-text))
    (place-into-drill-file total-text)))

;; --- General FlashCard functions
(defun wiki-drill/make-flash (type)
  "Given a flashcard type and text, generate a card"
  (when (string= type "simple")
    (wiki-drill/make-flash-simple))
  (when (string= type "clozer")
    (wiki-drill/make-flash-clozer)))  ;;works
(defun wiki-drill/offer-flashcard-choices ()
  "Offers simple or clozer type to user"
  (sit-for 1)
  (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
  (ido-completing-read "Flashcard Type:" '("simple" "clozer")))
(defun wiki-drill/get-flashcard-text ()
  "Get non-commented text from FlashCard buffer"
  (with-current-buffer (wiki-drill/get-flashcard-buffer)
    (save-restriction
      (widen)
      (goto-char (point-min))
      (buffer-substring-no-properties
       (search-forward-regexp "^[^;$]")
       (point-max)))))


(wiki-drill/make-flash "clozer")

 (lambda() message "simple")
 (lambda() message "clozer"))



(defun good-test (subject)
  (wiki-summary subject)
  (when (get-buffer "*wiki-summary*") (kill-buffer "*wiki-summary*"))
  (when (get-buffer "*FlashCard*" ) (kill-buffer "*FlashCard*"))
  (let ((type (wiki-drill/offer-flashcard-choices)))
    (wiki-drill/make-flash type)))

(good-test "Lawton railway station")


(defun test-wiki ()
  (progn
    (when (get-buffer "*wiki-summary*") (kill-buffer "*wiki-summary*"))
    (when (get-buffer "*FlashCard*" ) (kill-buffer "*FlashCard*"))
    (progn (wiki-summary "RNA")
           (sit-for 0.3)
           (wiki-drill/make-flash "clozer" ))))
(test-wiki)
