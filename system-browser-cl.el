;;; system-browser.el --- System Browser backend for Common Lisp   -*- lexical-binding: t -*-

;;; Commentary:
;;; System Browser backend for Common Lisp.

;;; Code:

(require 'system-browser)
(require 'slime)

(defgroup system-browser-cl nil
  "System Browser Common Lisp backend."
  :group 'system-browser)

(defcustom esb/cl:start-slime-automatically nil
  "When enabled, SLIME is started automatically when system browser is started."
  :type 'boolean
  :group 'system-browser-cl
  :tag "Start SLIME automatically")

(defcustom esb/cl:load-asdf-systems-on-browse t
  "When enabled, load ASDF systems before browsing them."
  :type 'boolean
  :group 'system-browser-cl
  :tag "Load ASDF systems on browse")

(defcustom esb/cl:asdf-system (cons "" nil)
  "When set, system-browser will browse the ASDF system on start.
The first argument specifies the ASDF system name.
The second argument indicates if include system's direct dependencies or not."
  :type '(cons (string :tag "ASDF system name")
               (boolean :tag "Include direct dependencies"))
  :group 'system-browser-cl
  :tag "ASDF system")

(add-hook 'system-browser-start-hook 'esb/cl:maybe-browse-customized-asdf-system)

(defclass esb/cl:common-lisp-system (esb:system-browser-system)
  ((modules-list-function :accessor esb:modules-list-function
                          :initform nil
                          :documentation "Function used to get the list of modules, when present")))

(defun esb/cl:list-all-cl-packages ()
  "Return list of Common Lisp packages."
  (slime-eval '(cl:sort (cl:mapcar 'cl:package-name (cl:list-all-packages)) 'cl:string<)))

(defun esb/cl:asdf-system-packages (system-name &optional include-direct-dependencies)
  (slime-eval `(esb:asdf-system-packages ,system-name ,include-direct-dependencies)))

(cl-defmethod esb:list-modules ((system esb/cl:common-lisp-system))
  (if (esb:modules-list-function system)
      (funcall (esb:modules-list-function system))
    (esb/cl:list-all-cl-packages)))

(defun esb/cl:maybe-browse-customized-asdf-system ()
  (when (not (zerop (length (car esb/cl:asdf-system))))
    (when (cdr esb/cl:asdf-system)
      (setq current-prefix-arg (cdr esb/cl:asdf-system)))
    (system-browser-browse-system (car esb/cl:asdf-system))))

(cl-defmethod esb:system-initialize-definition-buffer ((system esb/cl:common-lisp-system))
  (lisp-mode))

(cl-defmethod esb:get-module-properties ((system esb/cl:common-lisp-system) module)
  (let* ((module-properties (slime-eval `(esb::serialize-for-emacs (def-properties:package-properties ,module t))))
         (source (cl-find :source module-properties :key 'car))
         (file (and source
                    (or (cadr (cl-find :file (cl-remove-if-not 'listp source) :key 'car))
                        (caddr (cl-find :buffer-and-file (cl-remove-if-not 'listp source) :key 'car)))))
         (position (and source (or
                                (cadr (cl-find :position (cl-remove-if-not 'listp source) :key 'car))
                                (cadr (cl-find :offset (cl-remove-if-not 'listp source) :key 'car)))))
         (documentation (cdr (assoc :documentation module-properties))))
    (list (cons 'source source)
	  (cons 'file file)
	  (cons 'position position)
	  (cons 'documentation documentation))))

(cl-defmethod esb:get-definition-properties ((system esb/cl:common-lisp-system) definition category module)
  (let ((definition-function
         (cond
          ((string= category "functions") 'def-properties:function-properties)
          ((string= category "variables") 'def-properties:variable-properties)
          ((string= category "macros") 'def-properties:macro-properties)
          ((string= category "classes") 'def-properties:class-properties)
          ((string= category "generic functions") 'def-properties:generic-function-properties)
          (t (error "Invalid category: %s" category)))))
    (let* ((definition-properties (slime-eval `(esb:serialize-for-emacs (,definition-function (cl:intern ,definition ,module) t))))
           (source (cl-find :source definition-properties :key 'car))
           (file (and source (or
                              (cadr (cl-find :file (cl-remove-if-not 'listp source) :key 'car))
                              (caddr (cl-find :buffer-and-file (cl-remove-if-not 'listp source) :key 'car)))))
           (position (and source (or
                                  (cadr (cl-find :position (cl-remove-if-not 'listp source) :key 'car))
                                  (cadr (cl-find :offset (cl-remove-if-not 'listp source) :key 'car)))))
	   (documentation (cdr (assoc :documentation definition-properties))))
      (list (cons 'source source)
	    (cons 'file file)
	    (cons 'position position)
	    (cons 'documentation documentation)))))

(cl-defmethod esb:read-module-name ((system esb/cl:common-lisp-system) prompt)
  (slime-read-package-name prompt))

(cl-defmethod esb:list-categories ((system esb/cl:common-lisp-system) module)
  (ignore system module)
  '("functions" "variables" "macros" "classes" "generic functions"))

(cl-defmethod esb:list-definitions ((system esb/cl:common-lisp-system) module category)
  (ignore system)
  (let ((definition-type
         (cond
          ((string= category "functions") :function)
          ((string= category "variables") :variable)
          ((string= category "macros") :macro)
          ((string= category "classes") :class)
          ((string= category "generic functions") :generic-function))))

    (slime-eval `(esb:list-definitions ,module ,definition-type :include-internal-p ,esb:list-internal-definitions))))

(defun system-browser-browse-system (system-name)
  "Browse ASDF system packages."
  (interactive (list (slime-read-system-name)))
  (if (zerop (length system-name))
      (oset esb:current-browser-system modules-list-function nil)
    (let ((include-direct-dependencies (not (null current-prefix-arg))))
      (when esb:load-asdf-systems-on-browse
        (slime-eval `(cl:progn (asdf:operate 'asdf:load-op ,system-name) nil)))
      (oset esb:current-browser-system modules-list-function
            (lambda ()
              (esb:asdf-system-packages system-name include-direct-dependencies)))))
  (system-browser-refresh))

(defun lisp-system-browser ()
  "Open the Common Lisp system browser."
  (interactive)
  (cl-block system-browser

    ;; Start SLIME if needed
    (when (not (slime-connected-p))
      (when (or esb:start-slime-automatically
                (yes-or-no-p "SLIME is not connected. Start? "))
        (add-hook 'slime-connected-hook 'system-browser t)
        (slime))
      (cl-return-from system-browser))
    
    (setq esb:current-browser-system (make-instance 'esb/cl:common-lisp-system))
    (system-browser)))

;;------ SLIME --------------------------------------------

(define-slime-contrib system-browser-cl
  "Smalltalk-like system browser for Common Lisp"
  (:authors "Mariano Montone")
  (:license "GPL")
  (:slime-dependencies slime-asdf)
  (:swank-dependencies emacs-system-browser))

(provide 'system-browser-cl)

;;; system-browser-cl.el ends here
