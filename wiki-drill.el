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

(setq wiki-drill--tmp-subject nil)
(setq wiki-drill--tmp-type nil)

;; --- File operations
(defcustom wiki-drill--file "~/wiki-drill-inputs.org"
  "File to temprarily store drill entries. It is then up to the user to refile these entries.")
(defun wiki-drill/drill-file-touch ()
  "Touches drill file, creates it if not there"
  (if (not (file-exists-p wiki-drill--file))
      (write-region "" nil wiki-drill--file)))
(defun wiki-drill/refile-into-file (text)
  "Refile into text"
  (progn (wiki-drill/drill-file-touch)
         (write-region text nil wiki-drill--file 'append)))


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
(defun wiki-drill/offer-flashcard-choices ()
  "Offers a choice of categories to the user"
  (let ((choices
         (append
          '("simple"
            "hide1cloze"      ;; hides 1 at random, shows all others
            "show1cloze"      ;; shows 1 at random, hides all others
            "hide2cloze"      ;; hides 2 at random, shows all others
            "show2cloze"      ;; shows 2 at random, hides all others
            "hide1_firstmore" ;; hides 1st 75% of the time, shows all others
            "show1_firstless" ;; shows 1st 25% of the time, hides all others
            "show1_lastmore") ;; shows last 75% of the time, hides all others
          wiki-drill--custom-clozer)))
    (progn (sit-for 1)
           (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
           (ido-completing-read "Clozer Types:" choices))))
(defun wiki-drill/make-flash (subject)
  "Offer the user flashcard"
  (let* ((flash-type (wiki-drill/offer-flashcard-choices)))
    (setq wiki-drill--tmp-subject subject)
    (setq wiki-drill--tmp-type flash-type)
    (wiki-drill/make-flash-clozer-usertext)))
(defun wiki-drill/make-flash-clozer-usertext ()
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
        (insert comment-str)
        (progn (buffer-swap-text flashbuff)      ;; switch text to that of flashcard
               (pop-to-buffer flashbuff)
               (clozer-mode 1))))))
(defun wiki-drill/total-text (subject type body)
  "Put together the header and body"
  (format "\
* %s     :drill:
   :PROPERTIES:
   :DRILL_CARD_TYPE: %s
   :END:

** Definition:
%s
" subject type body))


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

;; --- Submitting a flashcard
(defun wiki-drill/get-flashcard-text ()
  "Get non-commented text from FlashCard buffer"
  (with-current-buffer (wiki-drill/get-flashcard-buffer)
    (save-restriction
      (widen)
      (goto-char (point-min))
      (buffer-substring-no-properties
       (search-forward-regexp "^[^;$]")
       (point-max)))))
(defun wiki-drill/clozer-submit ()
  "Pulls text from Flashcard and calls refiler"
  (let* ((user-text (wiki-drill/get-flashcard-text))
         (total-text (wiki-drill/total-text
                      wiki-drill--tmp-subject
                      wiki-drill--tmp-type
                      user-text)))
    (progn (wiki-drill/refile-into-file total-text)
           (wiki-drill/kill-all-buffers)
           (switch-to-buffer (find-file wiki-drill--file)))))
(defun wiki-drill/kill-all-buffers ()
  "Kill buffers related to wiki-summary and FlashCard"
  (when (get-buffer "*wiki-summary*") (kill-buffer "*wiki-summary*"))
  (when (get-buffer "*FlashCard*" ) (kill-buffer "*FlashCard*")))
  
(defun wiki-drill (subject)
  (wiki-summary subject)
  (wiki-drill/kill-all-buffers)
  (wiki-drill/make-flash subject))


(wiki-drill "Lawton railway station")
;; (wiki-drill "RNA")
