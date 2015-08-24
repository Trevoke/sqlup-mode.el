;;; sqlup-mode.el --- Upcase SQL words for you

;; Copyright (C) 2014 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; URL: https://github.com/trevoke/sqlup-mode.el
;; Created: Jun 25 2014
;; Version: 0.5.1
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
  (if (sqlup-should-do-workp)
      (save-excursion (sqlup-maybe-capitalize-symbol -1))))

(defun sqlup-should-do-workp ()
  "sqlup is triggered after user keypresses. Here we check that this was one of the keypresses we care about."
  (or (sqlup-user-pressed-returnp)
      (and (sqlup-user-is-typingp)
	   (sqlup-trigger-self-insert-characterp))))

(defun sqlup-user-pressed-returnp ()
  (and (< 0 (length (this-command-keys-vector)))
       (or (equal 13 (elt (this-command-keys-vector) 0))
           (equal 10 (elt (this-command-keys-vector) 0)))))

(defun sqlup-user-is-typingp ()
  (string= "self-insert-command" (symbol-name this-command)))

(defun sqlup-trigger-self-insert-characterp ()
  (let ((sqlup-trigger-characters '(?\; ?\  ?\( ?\,)) ;; "?\ " is 'SPC'
	(sqlup-current-char (elt (this-command-keys-vector) 0)))
    (member sqlup-current-char sqlup-trigger-characters)))

(defun sqlup-maybe-capitalize-symbol (direction)
  "DIRECTION is either 1 for forward or -1 for backward"
  (forward-symbol direction)
  (sqlup-work-on-symbol (thing-at-point 'symbol) (bounds-of-thing-at-point 'symbol)))

(defun sqlup-work-on-symbol (symbol symbol-boundaries)
  (if (and symbol
	   (sqlup-keywordp (downcase symbol))
	   (sqlup-capitalizablep (point)))
      (progn
	(delete-region (car symbol-boundaries)
		       (cdr symbol-boundaries))
	(insert (upcase symbol)))))

(defun sqlup-capitalizablep (point-location)
  (let ((old-buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring old-buffer)
      (sql-mode)
      (goto-char point-location)
      (and (not (sqlup-commentp))
	   (not (sqlup-stringp))))))

(defun sqlup-commentp ()
  (and (nth 4 (syntax-ppss)) t))

(defun sqlup-stringp ()
  (and (nth 3 (syntax-ppss)) t))

;;;###autoload
(defun sqlup-capitalize-keywords-in-region (start-pos end-pos)
  "Call this function on a region to capitalize the SQL keywords therein."
  (interactive "r")
  (save-excursion
    (goto-char start-pos)
    (while (< (point) end-pos)
      (sqlup-maybe-capitalize-symbol 1))))

(defun sqlup-keywords-regexps ()
  (if (not sqlup-local-keywords-regexps)
      (setq-local sqlup-local-keywords-regexps (sqlup-find-correct-keywords)))
  sqlup-local-keywords-regexps)

(defun sqlup-find-correct-keywords ()
  "If emacs is handling the logic for syntax highlighting of SQL keywords, then we piggyback on top of that logic. If not, we use an sql-mode function to create a list of regular expressions and use that."
  (if (and (boundp 'sql-mode-font-lock-keywords) sql-mode-font-lock-keywords)
      (mapcar 'car sql-mode-font-lock-keywords)
    (mapcar 'car (sql-add-product-keywords
		  (or (and (boundp 'sql-product) sql-product) 'ansi) '()))))

(defun sqlup-keywordp (word)
  (let* ((sqlup-keyword-found nil)
	 (sqlup-terms (sqlup-keywords-regexps))
	 (sqlup-term (car sqlup-terms))
	 (temp-syntax (make-syntax-table)))
    (modify-syntax-entry ?_ "w" temp-syntax)
    (with-syntax-table temp-syntax
      (while (and (not sqlup-keyword-found)
		  sqlup-terms)
	(setq sqlup-keyword-found (string-match sqlup-term word))
	(setq sqlup-term (car sqlup-terms))
	(setq sqlup-terms (cdr sqlup-terms)))
      (and sqlup-keyword-found t))))

(defvar sqlup-local-keywords-regexps nil
  "Buffer local variable holding regexps from sql-mode to
identify keywords.")

(provide 'sqlup-mode)
