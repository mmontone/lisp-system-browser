(require :def-properties (merge-pathnames #p"cl-def-properties/module.lisp" (uiop/pathname:pathname-directory-pathname *load-pathname*)))

(defpackage :emacs-system-browser
  (:nicknames :esb)
  (:use :cl)
  (:export
   :package-definitions
   :list-definitions
   :emacs-package-definitions
   :serialize-for-emacs
   :asdf-system-packages))

(in-package :emacs-system-browser)

(defun package-definitions (package-name category &key include-internal-p)
  (let ((properties-function
          (ecase category
            (:function 'def-properties:function-properties)
            (:macro 'def-properties:macro-properties)
            (:class 'def-properties:class-properties)
            (:variable 'def-properties:variable-properties)
	    (:generic-function 'def-properties:generic-function-properties)))
        (predicate-function
          (ecase category
            (:macro 'def-properties:symbol-macro-p)
            (:function 'def-properties:symbol-function-p)
            (:class 'def-properties:symbol-class-p)
            (:variable 'def-properties:symbol-variable-p)
	    (:generic-function 'def-properties:symbol-generic-function-p))))
    (let (defs
             (package (find-package package-name)))
      (if include-internal-p
	  (do-symbols (symbol package)
	    (when (and (eql (symbol-package symbol) package)
		       (funcall predicate-function symbol))
	      (push (funcall properties-function symbol) defs)))
	  (do-external-symbols (symbol package)
	    (when (and (eql (symbol-package symbol) package)
		       (funcall predicate-function symbol))
	      (push (funcall properties-function symbol) defs))))
      (sort defs 'string< :key (lambda (x) (alexandria:assoc-value x :name))))))

(defun serialize-for-emacs (info)
  (when (alexandria:assoc-value info :package)
    (setf (cdr (assoc :package info))
          (package-name (alexandria:assoc-value info :package))))
  (when (alexandria:assoc-value info :arglist)
    ;; arglist is conflictive for slime protocol. do not use.
    (setf (cdr (assoc :arglist info)) nil))
  (push (cons :symbol (cdr (assoc :name info))) info)
  (setf (cdr (assoc :name info)) (princ-to-string (cdr (assoc :name info))))
  info)

(defun emacs-package-definitions (package-name category)
  (mapcar 'serialize-for-emacs (package-definitions package-name category)))

(defun list-definitions (package-name category &key include-internal-p)
  (let ((predicate-function
          (ecase category
            (:function 'def-properties:symbol-function-p)
            (:macro 'def-properties:symbol-macro-p)
            (:class 'def-properties:symbol-class-p)
            (:variable 'def-properties:symbol-variable-p)
	    (:generic-function 'def-properties:symbol-generic-function-p))))
    (let (defs
             (package (find-package package-name)))
      (if include-internal-p
	  (do-symbols (symbol package)
	    (when (and (eql (symbol-package symbol) package)
		       (funcall predicate-function symbol))
	      (push symbol defs)))
	  (do-external-symbols (symbol package)
	    (when (and (eql (symbol-package symbol) package)
		       (funcall predicate-function symbol))
	      (push symbol defs))))
      (sort (mapcar 'symbol-name defs) 'string<))))

(defun asdf-system-packages (system-name &optional include-direct-dependencies)
  "Get the list of packages for ASDF system with name SYSTEM-NAME.
If INCLUDE-DIRECT-DEPENDENCIES is true, then packages of the system's direct dependencies are included too."
  (let* ((system (or (asdf:find-system system-name nil)
		     (return-from asdf-system-packages)))
	 (packages (def-properties:asdf-system-packages system)))
    (when include-direct-dependencies
      (dolist (dependency (asdf:system-depends-on system))
	;; TODO: manage :feature dependencies?
	(when (stringp dependency)
	  (alexandria:appendf packages (asdf-system-packages dependency)))))
    (sort (mapcar 'package-name packages) 'string<)))

(provide 'emacs-system-browser)
