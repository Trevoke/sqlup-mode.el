(make-variable-buffer-local
 (defvar sqlup-keywords
   '(ABSOLUTE	EXEC	OVERLAPS
                ACTION	EXECUTE	PAD
                ADA	EXISTS	PARTIAL
                ADD	EXTERNAL	PASCAL
                ALL	EXTRACT	POSITION
                ALLOCATE	FALSE	PRECISION
                ALTER	FETCH	PREPARE
                AND	FIRST	PRESERVE
                ANY	FLOAT	PRIMARY
                ARE	FOR	PRIOR
                AS	FOREIGN	PRIVILEGES
                ASC	FORTRAN	PROCEDURE
                ASSERTION	FOUND	PUBLIC
                AT	FROM	READ
                AUTHORIZATION	FULL	REAL
                AVG	GET	REFERENCES
                BEGIN	GLOBAL	RELATIVE
                BETWEEN	GO	RESTRICT
                BIT	GOTO	REVOKE
                BIT_LENGTH	GRANT	RIGHT
                BOTH	GROUP	ROLLBACK
                BY	HAVING	ROWS
                CASCADE	HOUR	SCHEMA
                CASCADED	IDENTITY	SCROLL
                CASE	IMMEDIATE	SECOND
                CAST	IN	SECTION
                CATALOG	INCLUDE	SELECT
                CHAR	INDEX	SESSION
                CHAR_LENGTH	INDICATOR	SESSION_USER
                CHARACTER	INITIALLY	SET
                CHARACTER_LENGTH	INNER	SIZE
                CHECK	INPUT	SMALLINT
                CLOSE	INSENSITIVE	SOME
                COALESCE	INSERT	SPACE
                COLLATE	INT	SQL
                COLLATION	INTEGER	SQLCA
                COLUMN	INTERSECT	SQLCODE
                COMMIT	INTERVAL	SQLERROR
                CONNECT	INTO	SQLSTATE
                CONNECTION	IS	SQLWARNING
                CONSTRAINT	ISOLATION	SUBSTRING
                CONSTRAINTS	JOIN	SUM
                CONTINUE	KEY	SYSTEM_USER
                CONVERT	LANGUAGE	TABLE
                CORRESPONDING	LAST	TEMPORARY
                COUNT	LEADING	THEN
                CREATE	LEFT	TIME
                CROSS	LEVEL	TIMESTAMP
                CURRENT	LIKE	TIMEZONE_HOUR
                CURRENT_DATE	LOCAL	TIMEZONE_MINUTE
                CURRENT_TIME	LOWER	TO
                CURRENT_TIMESTAMP	MATCH	TRAILING
                CURRENT_USER	MAX	TRANSACTION
                CURSOR	MIN	TRANSLATE
                DATE	MINUTE	TRANSLATION
                DAY	MODULE	TRIM
                DEALLOCATE	MONTH	TRUE
                DEC	NAMES	UNION
                DECIMAL	NATIONAL	UNIQUE
                DECLARE	NATURAL	UNKNOWN
                DEFAULT	NCHAR	UPDATE
                DEFERRABLE	NEXT	UPPER
                DEFERRED	NO	USAGE
                DELETE	NONE	USER
                DESC	NOT	USING
                DESCRIBE	NULL	VALUE
                DESCRIPTOR	NULLIF	VALUES
                DIAGNOSTICS	NUMERIC	VARCHAR
                DISCONNECT	OCTET_LENGTH	VARYING
                DISTINCT	OF	VIEW
                DOMAIN	ON	WHEN
                DOUBLE	ONLY	WHENEVER
                DROP	OPEN	WHERE
                ELSE	OPTION	WITH
                END	OR	WORK
                END-EXEC	ORDER	WRITE
                ESCAPE	OUTER	YEAR
                EXCEPT	OUTPUT	ZONE
                EXCEPTION )))

(defun sqlup-insert-space-and-maybe-capitalize ()
  (interactive)
  (maybe-capitalize-word-at-point)
  (insert " "))

(defun sqlup-insert-open-parens-and-maybe-capitalize ()
  (interactive)
  (maybe-capitalize-word-at-point)
  (insert "("))

(defun maybe-capitalize-word-at-point ()
  (setq sqlup-current-word (thing-at-point 'symbol))
  (message sqlup-current-word)
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

;;;###autoload
(add-hook 'SQLi[Postgres]-mode-hook 'sqlup-mode)

(provide 'sqlup-mode)
