(provide 'sqlup-mode)

;; TODO figure out how to write this
;; (ert-deftest upcase-select ()
;;   (with-temp-buffer
;;     (sqlup-mode)
;;     (insert "select")
;;     ;;    (execute-kbd-macro (kbd "SPC"))
;;     (call-interactively (global-key-binding (kbd "SPC")))
;;     (message "|%s|" (buffer-string))
;;     (should (equal (buffer-string) "SELECT "))))

(ert-deftest upcase-a-region ()
  (with-temp-buffer
    (goto-char (point-min))
    (insert "select count(*) from 'select' -- select")
    (set-mark (point-min))
    (call-interactively 'sqlup-capitalize-keywords-in-region)
    (should (equal (buffer-string) "SELECT COUNT(*) FROM 'select' -- select"))))
