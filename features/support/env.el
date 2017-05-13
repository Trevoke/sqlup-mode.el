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
 (setq inhibit-message t) ;; Do not log out that minor mode is enabled/disabled (or other calls to (message) )

 ;; This fixes an issue in emacs 25.1 where the debugger would be invoked
 ;; incorrectly, breaking ert.
 (when (and (= emacs-major-version 25) (< emacs-minor-version 2))
   (require 'cl-preloaded)
   (setf (symbol-function 'cl--assertion-failed)
         (lambda (form &optional string sargs args)
           "This function has been modified by electric-operator to remove an incorrect manual call
to the debugger in emacs 25.1. The modified version should only be used for
running the espuds tests."
           (if string
               (apply #'error string (append sargs args))
             (signal 'cl-assertion-failed `(,form ,@sargs))))))
 )

(Before
 ;; Before each scenario is run
 )

(After
  ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 (setq inhibit-message nil)
 )
