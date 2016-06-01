
(ert-deftest upcase-select ()
  "Will upcase the word 'select' if it sees it"
  (with-temp-buffer
    (insert "select")
    (message "%s" (thing-at-point 'symbol))
    (sqlup-maybe-capitalize-symbol -1)
    (message "%s" (point))
    (message "%s" (thing-at-point 'symbol))
    (should (equal (buffer-string) "SELECT"))))

(ert-deftest upcase-a-region ()
  (with-temp-buffer
    (insert "select count(*) from 'select' -- select")
    (set-mark (point-min))
    (call-interactively 'sqlup-capitalize-keywords-in-region)
    (should (equal (buffer-string) "SELECT COUNT(*) FROM 'select' -- select"))))