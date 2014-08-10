;;; sqlup-mode.el --- Upcase SQL words for you

;; Copyright (C) 2014 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; URL: https://github.com/trevoke/sqlup-mode.el
;; Created: Jun 25 2014
;; Version: 0.4.2
;; Keywords: sql, tools

;;; License:

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Activate the minor mode (M-x sqlup-mode) and type away
;; Alternatively, use a hook: (add-hook 'sql-mode 'sqlup-mode)
;; The capitalization is triggered when you press the following keys:
;; * SPC
;; * ,
;; * ;
;; * (
;; * \r (Enter)
;;
;; This package also provides a function to capitalize SQL keywords inside a region - always available, no need to activate the minor mode to use it:
;;
;; M-x sqlup-capitalize-keywords-in-region
;;
;; It is not bound to a keybinding, but here is an example of how you could do it:
;;
;; (global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)
;;
;; Here follows an example setup to activate `sqlup-mode` automatically when entering sql-mode or sql-interactive-mode:
;;
;; (add-hook 'sql-mode-hook 'sqlup-mode)
;; (add-hook 'sql-interactive-mode-hook 'sqlup-mode)


;;; Code:

;;;###autoload
(define-minor-mode sqlup-mode
  "Capitalizes SQL keywords for you."
  :lighter " SUP"
  (if sqlup-mode
      (sqlup-enable-keyword-capitalization)
    (sqlup-disable-keyword-capitalization)))

(defun sqlup-enable-keyword-capitalization ()
  "Add buffer-local hook to handle this mode's logic"
  (add-hook 'post-command-hook 'sqlup-capitalize-as-you-type nil t))

(defun sqlup-disable-keyword-capitalization ()
  "Remove buffer-local hook to handle this mode's logic"
  (remove-hook 'post-command-hook 'sqlup-capitalize-as-you-type t))

(defun sqlup-capitalize-as-you-type ()
  "This function is the post-command hook. This code gets run after every command in a buffer with this minor mode enabled."
  (if (and (sqlup-should-trigger-upcasingp)
           (not (sqlup-is-commentp (thing-at-point 'line))))
      (sqlup-maybe-capitalize-last-word-typed)))

(defun sqlup-should-trigger-upcasingp ()
  (or (sqlup-user-pressed-returnp)
      (and (sqlup-user-is-typingp)
           (sqlup-trigger-self-insert-characterp))))

(defun sqlup-is-commentp (line)
  (and line
       (string-match "^\s*--.*$" line)
       t))

(defun sqlup-user-pressed-returnp ()
  (equal 13 (elt (this-command-keys-vector) 0)))

(defun sqlup-user-is-typingp ()
  (string= "self-insert-command" (symbol-name this-command)))

(defun sqlup-trigger-self-insert-characterp ()
  (let ((sqlup-trigger-characters '(?\; ?\  ?\( ?\,)) ;; _?\ _ is 'SPC'
        (sqlup-current-char (elt (this-command-keys-vector) 0)))
    (member sqlup-current-char sqlup-trigger-characters)))

(defun sqlup-maybe-capitalize-last-word-typed ()
  (save-excursion
    (backward-word)
    (sqlup-work-on-word-at-point)))

(defun sqlup-work-on-word-at-point ()
  (let ((sqlup-current-word (thing-at-point 'symbol t))
        (sqlup-current-word-boundaries (bounds-of-thing-at-point 'symbol)))
    (if (and sqlup-current-word
             (sqlup-is-keywordp (downcase sqlup-current-word)))
        (progn
          (delete-region (car sqlup-current-word-boundaries)
                         (cdr sqlup-current-word-boundaries))
          (insert (upcase sqlup-current-word))))))

;;;###autoload
(defun sqlup-capitalize-keywords-in-region ()
  "Call this function on a region to capitalize the SQL keywords therein."
  (interactive)
  (save-excursion
    (let* ((sqlup-start-of-region (region-beginning))
           (sqlup-end-of-region (region-end)))
        (goto-char sqlup-start-of-region)
        (while (search-forward-regexp "[[:alpha:]_]+" sqlup-end-of-region t)
          (sqlup-work-on-word-at-point)))))

(defun sqlup-keywords-regexps ()
  (if (not sqlup-local-keywords-regexps)
      (setq-local sqlup-local-keywords-regexps
                  (sqlup-find-correct-keywords)))
  sqlup-local-keywords-regexps)

(defun sqlup-find-correct-keywords ()
  "If emacs is handling the logic for syntax highlighting of SQL keywords, then we piggyback on top of that logic. If not, we use an sql-mode function to create a list of regular expressions and use that."
  (if (boundp 'sql-mode-font-lock-keywords)
      (mapcar 'car sql-mode-font-lock-keywords)
    (mapcar 'car (sql-add-product-keywords 'ansi '()))))

(defun sqlup-is-keywordp (word)
  (let* ((sqlup-keyword-found nil)
         (sqlup-terms (sqlup-keywords-regexps))
         (sqlup-term (car sqlup-terms)))
    (while (and (not sqlup-keyword-found)
                sqlup-terms)
      (setq sqlup-keyword-found (string-match sqlup-term word))
      (setq sqlup-term (car sqlup-terms))
      (setq sqlup-terms (cdr sqlup-terms)))
    (and sqlup-keyword-found t)))

(defvar sqlup-local-keywords-regexps nil
  "Buffer local variable holding regexps from sql-mode to
identify keywords.")

(provide 'sqlup-mode)
