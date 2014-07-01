;;; sqlup-mode.el --- Upcase SQL words for you

;; Copyright (C) 2014 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; URL: https://github.com/trevoke/sqlup-mode.el
;; Created: Jun 25 2014
;; Version: 0.1.3
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
;; The capitalization is triggered when you press 'SPC', ';' or '('
;;
;; This mode also provides a function to capitalize SQL keywords inside a region.
;; M-x sqlup-capitalize-keywords-in-region
;; It is not bound to a keybinding, but here is an example of how you could do it:
;; (local-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)

;;; Code:

(defun sqlup-insert-space-and-maybe-capitalize ()
  (interactive)
  (sqlup-maybe-capitalize-word-at-point)
  (insert " "))

(defun sqlup-insert-open-parens-and-maybe-capitalize ()
  (interactive)
  (sqlup-maybe-capitalize-word-at-point)
  (insert "("))

(defun sqlup-insert-semicolon-and-maybe-capitalize ()
  (interactive)
  (sqlup-maybe-capitalize-word-at-point)
  (insert ";"))

(defun sqlup-insert-comma-and-maybe-capitalize ()
  (interactive)
  (sqlup-maybe-capitalize-word-at-point)
  (insert ","))

(defun sqlup-maybe-capitalize-word-at-point ()
  (if (not (sqlup-is-commentp (thing-at-point 'line)))
      (let ((sqlup-current-word (thing-at-point 'symbol))
	(sqlup-current-word-boundaries (bounds-of-thing-at-point 'symbol)))
    (if (and sqlup-current-word
	     (sqlup-is-keywordp (downcase sqlup-current-word)))
	(progn
	  (delete-region (car sqlup-current-word-boundaries)
			 (cdr sqlup-current-word-boundaries))
	  (insert (upcase sqlup-current-word)))))))

(defun sqlup-keywords-regexps ()
  (if (not sqlup-local-keywords-regexps)
      (setq-local sqlup-local-keywords-regexps
		  (mapcar 'car sql-mode-font-lock-keywords)))
  sqlup-local-keywords-regexps)

(defun sqlup-is-keywordp (word)
  (let* ((sqlup-keyword-found nil)
	(sqlup-terms (sqlup-keywords-regexps))
	(sqlup-term (car sqlup-terms)))
    (while (and (not sqlup-keyword-found) sqlup-terms)
      (setq sqlup-keyword-found (string-match sqlup-term word))
      (setq sqlup-term (car sqlup-terms))
      (setq sqlup-terms (cdr sqlup-terms)))
    (and sqlup-keyword-found t)))

(defun sqlup-is-commentp (line)
  (and (string-match "^\s+--.*$" line) t))

;;;###autoload
(defun sqlup-capitalize-keywords-in-region ()
  "Call this function on a region to capitalize the SQL keywords therein."
  (interactive)

  (save-excursion
    (let* ((sqlup-start-of-region (region-beginning))
           (sqlup-end-of-region (region-end)))
      (progn
        (goto-char sqlup-start-of-region)
        (while (search-forward-regexp "[[:alpha:]_]+" sqlup-end-of-region t)
          (if (member (downcase (match-string 0)) sqlup-keywords)
              (replace-match (upcase (match-string 0)) t t)))))))

;;;###autoload
(define-minor-mode sqlup-mode
  "Capitalizes SQL keywords for you."
  :lighter " SUP"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "SPC") 'sqlup-insert-space-and-maybe-capitalize)
            (define-key map (kbd "(") 'sqlup-insert-open-parens-and-maybe-capitalize)
            (define-key map (kbd ";") 'sqlup-insert-semicolon-and-maybe-capitalize)
	    (define-key map (kbd ",") 'sqlup-insert-comma-and-maybe-capitalize)
            map))

(defvar sqlup-local-keywords-regexps nil
  "Buffer local variable holding regexps from sql-mode to
identify keywords.")

(provide 'sqlup-mode)
