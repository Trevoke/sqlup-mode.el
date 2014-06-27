;; sqlup-mode.el --- Upcase SQL words for you

;; Copyright (C) 2014 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; Version: 0.0.1
;; Keywords: sql
;; URL: https://github.com/trevoke/sqlup.el

;;; Commentary:

;; Activate the minor mode (M-x sqlup-mode) and type away

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

;;; Code:

(defun sqlup-insert-space-and-maybe-capitalize ()
  (interactive)
  (sqlup-maybe-capitalize-word-at-point)
  (insert " "))

(defun sqlup-insert-open-parens-and-maybe-capitalize ()
  (interactive)
  (sqlup-maybe-capitalize-word-at-point)
  (insert "("))

(defun sqlup-maybe-capitalize-word-at-point ()
  (setq sqlup-current-word (thing-at-point 'symbol))
  (if (and (char-or-string-p sqlup-current-word)
           (member (intern (upcase sqlup-current-word)) sqlup-keywords))
      (progn
        (setq sqlup-current-word-upcase (upcase sqlup-current-word))
        (setq sqlup-current-word-boundaries (bounds-of-thing-at-point 'symbol))
        (delete-region (car sqlup-current-word-boundaries) (cdr sqlup-current-word-boundaries))
        (insert sqlup-current-word-upcase)
        )))

(define-minor-mode sqlup-mode
  "Capitalizes SQL keywords for you."
  :lighter " SUP"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "SPC") 'sqlup-insert-space-and-maybe-capitalize)
            (define-key map (kbd "(") 'sqlup-insert-open-parens-and-maybe-capitalize)
            map))

(make-variable-buffer-local
 (defvar sqlup-keywords
   '(
     ACTION
     ADA
     ADD
     ALL
     ALLOCATE
     ALTER
     AND
     ANY
     ARE
     AS
     ASC
     ASSERTION
     AT
     AUTHORIZATION
     AVG
     BEGIN
     BETWEEN
     BIT
     BIT_LENGTH
     BOTH
     BY
     CASCADE
     CASCADED
     CASE
     CAST
     CATALOG
     CHAR
     CHARACTER
     CHARACTER_LENGTH
     CHAR_LENGTH
     CHECK
     CLOSE
     COALESCE
     COLLATE
     COLLATION
     COLUMN
     COMMIT
     CONNECT
     CONNECTION
     CONSTRAINT
     CONSTRAINTS
     CONTINUE
     CONVERT
     CORRESPONDING
     COUNT
     CREATE
     CROSS
     CURRENT
     CURRENT_DATE
     CURRENT_TIME
     CURRENT_TIMESTAMP
     CURRENT_USER
     CURSOR
     DATE
     DAY
     DEALLOCATE
     DEC
     DECIMAL
     DECLARE
     DEFAULT
     DEFERRABLE
     DEFERRED
     DELETE
     DESC
     DESCRIBE
     DESCRIPTOR
     DIAGNOSTICS
     DISCONNECT
     DISTINCT
     DOMAIN
     DOUBLE
     DROP
     ELSE
     END
     END-EXEC
     ESCAPE
     EXCEPT
     EXCEPTION
     EXECUTE
     EXISTS
     EXTERNAL
     EXTRACT
     FALSE
     FETCH
     FIRST
     FLOAT
     FOR
     FOREIGN
     FORTRAN
     FOUND
     FROM
     FULL
     GET
     GLOBAL
     GO
     GOTO
     GRANT
     GROUP
     HAVING
     HOUR
     IDENTITY
     IMMEDIATE
     IN
     INCLUDE
     INDEX
     INDICATOR
     INITIALLY
     INNER
     INPUT
     INSENSITIVE
     INSERT
     INT
     INTEGER
     INTERSECT
     INTERVAL
     INTO
     IS
     ISOLATION
     JOIN
     KEY
     LANGUAGE
     LAST
     LEADING
     LEFT
     LEVEL
     LIKE
     LOCAL
     LOWER
     MATCH
     MAX
     MIN
     MINUTE
     MODULE
     MONTH
     NAMES
     NATIONAL
     NATURAL
     NCHAR
     NEXT
     NO
     NONE
     NOT
     NULL
     NULLIF
     NUMERIC
     OCTET_LENGTH
     OF
     ON
     ONLY
     OPEN
     OPTION
     OR
     ORDER
     OUTER
     OUTPUT
     PAD
     PARTIAL
     PASCAL
     POSITION
     PRECISION
     PREPARE
     PRESERVE
     PRIMARY
     PRIOR
     PRIVILEGES
     PROCEDURE
     PUBLIC
     READ
     REAL
     REFERENCES
     RELATIVE
     RESTRICT
     REVOKE
     RIGHT
     ROLLBACK
     ROWS
     SCHEMA
     SCROLL
     SECOND
     SECTION
     SELECT
     SESSION
     SESSION_USER
     SET
     SIZE
     SMALLINT
     SOME
     SPACE
     SQL
     SQLCA
     SQLCODE
     SQLERROR
     SQLSTATE
     SQLWARNING
     SUBSTRING
     SUM
     SYSTEM_USER
     TABLE
     TEMPORARY
     THEN
     TIME
     TIMESTAMP
     TIMEZONE_HOUR
     TIMEZONE_MINUTE
     TO
     TRAILING
     TRANSACTION
     TRANSLATE
     TRANSLATION
     TRIM
     TRUE
     UNION
     UNIQUE
     UNKNOWN
     UPDATE
     UPPER
     USAGE
     USER
     USING
     VALUE
     VALUES
     VARCHAR
     VARYING
     VIEW
     WHEN
     WHENEVER
     WHERE
     WITH
     WORK
     WRITE
     YEAR
     ZONE
     ABSOLUTE
     EXEC
     OVERLAPS
     )))

;;;###autoload
(add-hook 'sql-mode 'sqlup-mode)

(provide 'sqlup-mode)
