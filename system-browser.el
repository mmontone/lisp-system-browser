;;; system-browser.el --- System browser      -*- lexical-binding: t -*-

(require 'cl)
(require 'window-layout)

(defclass sb:system-browser-system ()
  ())

(defclass sb:common-lisp-system (sb:system-browser-system)
  ())

(defgeneric sb:list-packages (system-browser-system))
(defgeneric sb:list-categories (system-browser-system package))
(defgeneric sb:list-definitions (system-browser-system package category))

(defmethod sb:list-packages ((system sb:common-lisp-system))
  (slime-eval '(cl:sort (cl:mapcar 'cl:package-name (cl:list-all-packages)) 'cl:string<)))

(sb:list-packages (make-instance 'sb:common-lisp-system))

(defvar sb:packages-buffer)
(defvar sb:catgories-buffer)
(defvar sb:definitions-buffer)
(defvar sb:definitions-buffer)

(defvar sb:current-browser-system (make-instance 'sb:common-lisp-system))

(defun sb:open-packages-buffer ()
  (with-current-buffer "*sb-packages*"
    (erase-buffer)
    (dolist (package-name (sb:list-packages sb:current-browser-system))
      (insert-button package-name
                     'action (lambda (btn)
                               (message package-name)
                               (sb:open-categories-buffer package-name))
                     'follow-link t
                     'help-echo "Browse package")
      (newline)))
  (wlf:select sb:wm 'packages))

(defun sb:open-categories-buffer (package)
  (with-current-buffer "*sb-categories*"
    (erase-buffer)
    (insert package)
    (newline)
    (dolist (category (sb:list-categories sb:current-browser-system package))
      (insert-button category
                     'action (lambda (btn)
                               (sb:open-definitions-buffer package category))
                     'follow-link t
                     'help-echo "Browse category")
      (newline)))
  (wlf:select sb:wm 'categories))

(defun sb:open-definitions-buffer (package category)
  (with-current-buffer "*sb-definitions*"
    (erase-buffer)
    (insert category)
    (newline)
    (dolist (definition (sb:list-definitions sb:current-browser-system package category))
      (insert-button definition
                     'action (lambda (btn)
                               (sb:open-definition-buffer package category definition))
                     'follow-link t
                     'help-echo "Browse definition")
      (newline)))
  (wlf:select sb:wm 'definitions))

(defun sb:open-definition-buffer (package category definition)
  (let ((definition-type
          (cond
           ((string= category "Functions") :function)
           ((string= category "Variables") :variable)
	   ((string= category "Macros") :macro)
           ((string= category "Classes") :class)))
        (definition-function
          (cond
           ((string= category "Functions") 'def-properties:function-properties)
           ((string= category "Variables") 'def-properties:variable-properties)
	   ((string= category "Macros") 'def-properties:macro-properties)
           ((string= category "Classes") 'def-properties:class-properties))))
    (let ((definition-properties (slime-eval `(esb::serialize-for-emacs (,definition-function ',(make-symbol (concat package "::" definition)))))))
      (with-current-buffer "*sb-definition*"
	(erase-buffer)
	(let ((source (find :source definition-properties :key 'car)))
	  (let ((file (cadr (find :file (remove-if-not 'listp source) :key 'car)))
		(position (cadr (find :position (remove-if-not 'listp source) :key 'car))))
	    (setq buffer-file-name file)
	    (insert-file-contents file)
	    (lisp-mode)
	    (wlf:select sb:wm 'definition)
	    (goto-char position)))))))

(defmethod sb:list-categories ((system sb:common-lisp-system) package)
  '("Variables" "Macros" "Functions" "Classes" "Generic Functions"))

(defmethod sb:list-definitions ((system sb:common-lisp-system) package category)
  (let ((definition-type
          (cond
           ((string= category "Functions") :function)
           ((string= category "Variables") :variable)
	   ((string= category "Macros") :macro)
           ((string= category "Classes") :class))))

    (slime-eval `(esb::list-definitions ,package ,definition-type))))

(defvar sb:wm)

(defun open-system-browser ()
  (interactive)

  (setq sb:packages-buffer (get-buffer-create "*sb-packages*"))
  (setq sb:categories-buffer (get-buffer-create "*sb-categories*"))
  (setq sb:definitions-buffer (get-buffer-create "*sb-definitions*"))
  (setq sb:definition-buffer (get-buffer-create "*sb-definition*"))

  (setq sb:wm
        (wlf:layout
         '(| (:left-size-ratio 0.3)
             (- (:left-size-ratio 0.33)
                packages
                (- categories
                   definitions))
             definition)
         '((:name packages
                  :buffer "*sb-packages*")
           (:name categories
                  :buffer "*sb-categories*")
           (:name definitions
                  :buffer "*sb-definitions*")
           (:name definition
                  :buffer "*sb-definition*")
           )))
  (sb:open-packages-buffer)
  (wlf:select sb:wm 'packages))
