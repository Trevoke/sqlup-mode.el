;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(When "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(Then "^I should have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(And "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(But "^I should not have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))


(When "^I turn off \\(.+\\)$"
  "Turns off some mode."
  (lambda (mode)
    (let ((v (vconcat [?\C-u ?- 1 ?\M-x] (string-to-vector mode))))
(execute-kbd-macro v))))

(When "^I turn on \\(.+\\)$"
  "Turns on some mode."
  (lambda (mode)
    (let ((v (vconcat [?\C-u ?\M-x] (string-to-vector mode) [?\C-m])))
(execute-kbd-macro v))))

(When "^I mock turn on sql-interactive-mode$"
      "Turn on sql-interactive mode with process interaction functions mocked out"
      (lambda ()
        (cl-letf (((symbol-function 'set-process-sentinel) #'ignore)
                  ((symbol-function 'get-buffer-process) #'ignore))
          (sql-interactive-mode))))
