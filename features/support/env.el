(require 'f)

(defvar sqlup-mode-support-path
  (f-dirname (f-this-file)))

(defvar sqlup-mode-features-path
  (f-parent sqlup-mode-support-path))

(defvar sqlup-mode-root-path
  (f-parent sqlup-mode-features-path))

(add-to-list 'load-path sqlup-mode-root-path)

(require 'ert)
(require 'espuds)
(require 'sqlup-mode (f-expand "sqlup-mode.el" sqlup-mode-root-path))
(require 'redis)
(require 'cl-lib)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
  ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
