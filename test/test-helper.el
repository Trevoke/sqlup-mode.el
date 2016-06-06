(require 'f)

(defvar sqlup-mode-test-path
  (f-dirname (f-this-file)))

(defvar sqlup-mode-code-path
  (f-parent sqlup-mode-test-path))

(require 'sqlup-mode (f-expand "sqlup-mode.el" sqlup-mode-code-path))
