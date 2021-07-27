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
(defgeneric sb:packages-buffer-mode-line-format (system-browser-system))
(defgeneric sb:categories-buffer-mode-line-format (system-browser-system))
(defgeneric sb:definitions-buffer-mode-line-format (system-browser-system))
(defgeneric sb:definition-buffer-mode-line-format (system-browser-system))

;; Default mode-line
(defmethod sb:packages-buffer-mode-line-format (system-browser-system)
  "Packages")

(defmethod sb:categories-buffer-mode-line-format (system-browser-system)
  "Categories")

(defmethod sb:definitions-buffer-mode-line-format (system-browser-system)
  "Definitions")

(defmethod sb:definition-buffer-mode-line-format (system-browser-system)
  nil)

(defmethod sb:list-packages ((system sb:common-lisp-system))
  (slime-eval '(cl:sort (cl:mapcar 'cl:package-name (cl:list-all-packages)) 'cl:string<)))

(defvar sb:packages-buffer)
(defvar sb:catgories-buffer)
(defvar sb:definitions-buffer)
(defvar sb:definitions-buffer)
(defvar sb:documentation-buffer)

(defvar sb:current-browser-system (make-instance 'sb:common-lisp-system))

(defgroup system-browser nil
  "System browser configuration")

(defun sb:setup-list-buffer ()
  (set-face-attribute 'header-line nil
			:foreground "gray0"
			:background "cyan"
			:overline "cyan"
			:underline "cyan")
  (setq header-line-format mode-line-format)
  (setq mode-line-format nil))

(defun sb:initialize-packages-buffer ()
  (setq sb:packages-buffer (get-buffer-create "*sb-packages*"))
  (with-current-buffer sb:packages-buffer
    (sb:setup-list-buffer)
    (when (sb:packages-buffer-mode-line-format sb:current-browser-system)
      (setq header-line-format (sb:packages-buffer-mode-line-format sb:current-browser-system)))))

(defun sb:initialize-categories-buffer ()
  (setq sb:categories-buffer (get-buffer-create "*sb-categories*"))
  (with-current-buffer sb:categories-buffer
    (sb:setup-list-buffer)
    (when (sb:categories-buffer-mode-line-format sb:current-browser-system)
      (setq header-line-format (sb:categories-buffer-mode-line-format sb:current-browser-system)))))

(defun sb:initialize-definitions-buffer ()
  (setq sb:definitions-buffer (get-buffer-create "*sb-definitions*"))
  (with-current-buffer sb:definitions-buffer
    (sb:setup-list-buffer)
    (when (sb:definitions-buffer-mode-line-format sb:current-browser-system)
      (setq header-line-format (sb:definitions-buffer-mode-line-format sb:current-browser-system)))))

(defun sb:initialize-definition-buffer ()
  (setq sb:definition-buffer (get-buffer-create "*sb-definition*")))

(defun sb:initialize-documentation-buffer ()
  (setq sb:documentation-buffer (get-buffer-create "*sb-documentation*")))

(defun sb:create-packages-buffer ()
  (with-current-buffer "*sb-packages*"
    (setq buffer-read-only nil)
    (erase-buffer)
    (dolist (package-name (sb:list-packages sb:current-browser-system))
      (insert-button package-name
                     'action (lambda (btn)
                               (message package-name)
                               (sb:create-categories-buffer package-name))
                     'follow-link t
                     'help-echo "Browse package")
      (newline))
    (setq buffer-read-only t))
  (wlf:select sb:wm 'packages))

(defun sb:create-categories-buffer (package)
  (with-current-buffer "*sb-categories*"
    (setq buffer-read-only nil)    
    (erase-buffer)
    (insert package)
    (newline)
    (dolist (category (sb:list-categories sb:current-browser-system package))
      (insert-button category
                     'action (lambda (btn)
                               (sb:create-definitions-buffer package category))
                     'follow-link t
                     'help-echo "Browse category")
      (newline))
    (setq buffer-read-only t))
  (wlf:select sb:wm 'categories))

(defun sb:create-definitions-buffer (package category)
  (with-current-buffer "*sb-definitions*"
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert category)
    (newline)
    (dolist (definition (sb:list-definitions sb:current-browser-system package category))
      (insert-button definition
                     'action (lambda (btn)
			       (sb:create-definition-buffer package category definition)
			       (sb:create-documentation-buffer package category definition))
                     'follow-link t
                     'help-echo "Browse definition")
      (newline))
    (setq buffer-read-only t))
  (wlf:select sb:wm 'definitions))

(defun sb:create-definition-buffer (package category definition)
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
            (insert-file-contents file)
            (lisp-mode)
            (wlf:select sb:wm 'definition)
            ;; Assign file to buffer so changes in definition buffer can be saved
            (setq buffer-file-name file)
            ;; For some reason, sometimes definition buffer sets to read-only.
            ;; The following prevents that:
            (setq buffer-read-only nil)
            (goto-char position)
	    (recenter-top-bottom 0)
	    ))))))

(defun sb:create-documentation-buffer (package category definition)
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
      (with-current-buffer "*sb-documentation*"
	(setq buffer-read-only nil)
        (erase-buffer)
	;;(insert (prin1-to-string definition-properties))
	(let ((documentation (cdr (assoc :documentation definition-properties))))
	  (when documentation
	    (insert documentation)))	
	(goto-char 0)
	(setq buffer-read-only t)))))

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

(defun system-browser ()
  (interactive)

  (sb:initialize-packages-buffer)
  (sb:initialize-categories-buffer)
  (sb:initialize-definitions-buffer)
  (sb:initialize-definition-buffer)
  (sb:initialize-documentation-buffer)
  
  (setq sb:wm
        (wlf:layout
         '(| (:left-size-ratio 0.3)
             (- (:left-size-ratio 0.33)
                packages
                (- categories
                   definitions))
             (- (:left-size-ratio 0.66)
		definition
		documentation))
         '((:name packages
                  :buffer "*sb-packages*")
           (:name categories
                  :buffer "*sb-categories*")
           (:name definitions
                  :buffer "*sb-definitions*")
           (:name definition
                  :buffer "*sb-definition*")
	   (:name documentation
		  :buffer "*sb-documentation*")
           )))
  (sb:create-packages-buffer)
  (wlf:select sb:wm 'packages))

(defun quit-system-browser ()
  (interactive)
  (kill-buffer sb:packages-buffer)
  (kill-buffer sb:categories-buffer)
  (kill-buffer sb:definitions-buffer)
  (kill-buffer sb:definition-buffer)
  (wlf:clear-windows sb:wm t))
