(defpackage :emacs-system-browser
  (:nicknames :esb)
  (:use :cl))

(in-package :emacs-system-browser)

(defun package-definitions (package-name category)
  (let ((properties-function
          (ecase category
            (:function 'def-properties:function-properties)
	    (:macro 'def-properties:macro-properties)
            (:class 'def-properties:class-properties)
            (:variable 'def-properties:variable-properties)))
        (predicate-function
          (ecase category
	    (:macro 'def-properties:symbol-macro-p)
            (:function 'def-properties:symbol-function-p)
            (:class 'def-properties:symbol-class-p)
            (:variable 'def-properties:symbol-variable-p))))
    (let (defs
             (package (find-package package-name)))
      (do-symbols (symbol package)
        (when (and (eql (symbol-package symbol) package)
                   (funcall predicate-function symbol))
          (push (funcall properties-function symbol) defs)))
      (sort defs 'string< :key (lambda (x) (cdr (assoc x :name)))))))

(defun serialize-for-emacs (info)
  (when (alexandria:assoc-value info :package)
    (setf (cdr (assoc :package info))
          (package-name (alexandria:assoc-value info :package))))
  (when (alexandria:assoc-value info :arglist)
    ;; arglist is conflictive for slime protocol. do not use.
    (setf (cdr (assoc :arglist info)) nil))
  (push (cons :symbol (cdr (assoc :name info))) info)
  (setf (cdr (assoc :name info)) (symbol-name (cdr (assoc :name info))))
  info)

(defun emacs-package-definitions (package-name category)
  (mapcar 'serialize-for-emacs (package-definitions package-name category)))

(defun list-definitions (package-name category)
  (let ((predicate-function
          (ecase category
            (:function 'def-properties:symbol-function-p)
	    (:macro 'def-properties:symbol-macro-p)
            (:class 'def-properties:symbol-class-p)
            (:variable 'def-properties:symbol-variable-p))))
    (let (defs
	     (package (find-package package-name)))
      (do-symbols (symbol package)
        (when (and (eql (symbol-package symbol) package)
		   (funcall predicate-function symbol))
          (push symbol defs)))
      (sort (mapcar 'symbol-name defs) 'string<))))

(package-definitions "SWANK" :function)
(package-definitions "SWANK" :variable)
(emacs-package-definitions "SWANK" :function)

(list-definitions "SWANK" :function)
