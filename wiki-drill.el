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

(defcustom wiki-drill--file "~/wiki-drill.org"
  "File to store drill entries and read tags from")

(defcustom wiki-drill--custom-clozer '("test")
  "A list of custom clozer types provided by the user")

(defun wiki-drill/get-text-from-buffer (buffer)
  "Pull text from a given buffer")

(defun wiki-drill/make-flash (type text)
  "Given a flashcard type and text, generate a card"
  (if (eq type "simple") (wiki-drill/make-flash-simple text) nil)
  (if (eq type "clozer") (wiki-drill/make-flash-clozer text) nil))

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

(defun wiki-drill/make-flash-clozer-header ()
  "Makes the PROPERTIES header part of a clozer flashcard
   using a selection of types offered to the user"
  (let ((clozer-type (wiki-drill/offer-clozer-choices)))
    (format ":PROPERTIES:\n:DRILL_CARD_TYPE:%s\n:END:" clozer-type)))


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
                 (define-key map (kbd "C-b") (lambda () (interactive) (wiki-drill/clozer-brackets)))
                 (define-key map (kbd "RET") 'clozer-mode) map))

(defun wiki-drill/make-flash-clozer-usertext ()
  "Given a buffer of text for clozer type, let the user mark words"
  (let* ((flashbuff "*FlashCard*")
         (buf (generate-new-buffer flashbuff)))
    (with-current-buffer "*wiki-summary*"
      (setq inhibit-read-only t)
      (let ((comment-str ";; Mark keywords or regions with brackets (C-b), and leave hints with bars\n;; e.g. [hide these words||drop this hint]\n\n"))
        (put-text-property 0 (length comment-str) 'face 'font-lock-comment-face comment-str)
        (insert comment-str))
      ;; switch text to that of flashcard
      (buffer-swap-text buf) 
;;      (barf-if-buffer-read-only)
;;      (fill-paragraph)
;;      (goto-char (point-min))
;;      (text-mode)
;;      (view-mode))
      (pop-to-buffer buf)
      (clozer-mode 1))))

(defun test-wiki ()
  (progn
    (when (get-buffer "*wiki-summary*") (kill-buffer "*wiki-summary*"))
    (when (get-buffer "*FlashCard*" ) (kill-buffer "*FlashCard*"))
    (wiki-summary "RNA")
    (sit-for 1)
    (wiki-drill/make-flash-clozer-usertext)))

(test-wiki)


(defun wiki-drill/make-flash-clozer (text)
  "Makes a clozer flashcard")



(defun wiki-drill/insert-flash-to-file (flash)
  "Given a flash (org-mode heading), refile into a target subheading")
