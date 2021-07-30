;;; system-browser.el --- System browser      -*- lexical-binding: t -*-
;;; Commentary: An Smalltalk-like browser for Common Lisp.

(require 'cl)
(require 'window-layout)


;------ Model ------------------------------------------

(defclass esb:system-browser-system ()
  ((selected-package :accessor esb:selected-package
		     :initform nil)
   (selected-category :accessor esb:selected-category
		      :initform nil)
   (selected-definition :accessor esb:selected-definition
			:initform nil)))

(defclass esb:common-lisp-system (esb:system-browser-system)
  ())

(defgeneric esb:list-packages (system-browser-system))
(defgeneric esb:list-categories (system-browser-system package))
(defgeneric esb:list-definitions (system-browser-system package category))
(defgeneric esb:packages-buffer-mode-line-format (system-browser-system))
(defgeneric esb:categories-buffer-mode-line-format (system-browser-system))
(defgeneric esb:definitions-buffer-mode-line-format (system-browser-system))
(defgeneric esb:definition-buffer-mode-line-format (system-browser-system))

;; Default mode-line
(defmethod esb:packages-buffer-mode-line-format (system-browser-system)
  "Packages")

(defmethod esb:categories-buffer-mode-line-format (system-browser-system)
  "Categories")

(defmethod esb:definitions-buffer-mode-line-format (system-browser-system)
  "Definitions")

(defmethod esb:definition-buffer-mode-line-format (system-browser-system)
  nil)

(defmethod esb:list-packages ((system esb:common-lisp-system))
  (slime-eval '(cl:sort (cl:mapcar 'cl:package-name (cl:list-all-packages)) 'cl:string<)))

(defvar esb:packages-buffer)
(defvar esb:catgories-buffer)
(defvar esb:definitions-buffer)
(defvar esb:definitions-buffer)
(defvar esb:documentation-buffer)

(defvar esb:current-browser-system (make-instance 'esb:common-lisp-system))

(defun alist-to-plist (alist)
  (let ((plist '()))
    (dolist (cons alist)
      (push (car cons) plist)
      (push (cdr cons) plist))
    (reverse plist)))

;;(alist-to-plist '((a . 22) (b . "asf")))

;; --------- Settings ---------------------------------

(defgroup system-browser nil
  "System browser configuration")

(defcustom esb:show-documentation-buffer nil
  "Show documentation buffer in system browser."
  :type 'boolean
  :group 'system-browser
  :tag "Show documentation buffer")

(defcustom esb:downcase-definition-names t
  "Show system-browser definition names in lowercase."
  :type 'boolean
  :group 'system-browser
  :tag "Downcase definition names")

(defcustom esb:list-internal-definitions t
  "List packages internal defintions, apart from the exported."
  :type 'boolean
  :group 'system-browser
  :tag "List internal definitions")

(defcustom esb:preserve-definition-buffer-on-exit t
  "Keep the current system browser definition buffer and file alive when the system browser is closed."
  :type 'boolean
  :group 'system-browser
  :tag "Preserve definition buffer on exit")

(defcustom esb:start-slime-automatically nil
  "When enabled, SLIME is started automatically when system browser is started."
  :type 'boolean
  :group 'system-browser
  :tag "Start SLIME automatically")

;------- Faces --------------------------

(defface esb:definition-list-item-face
  '((((background light))
     :foreground "black"
     :height 0.9)
    (((background dark))
     :foreground "white"
     :height 0.9)
    )
  "Face for system-browser definitions list items"
  :group 'system-browser-faces)

(defface esb:mode-line-buttons-face
  '((((background light))
     :box (:line-width 2 :color "dark grey")
     :background "light grey" :foreground "black")
    (((background dark))
     :box (:line-width 2 :color "dark grey")
     :background "black" :foreground "white"))
  "Face for system-browser buttons in mode-line"
  :group 'system-browser-faces)

(defface esb:definitions-list-header-face
  '((t :inherit bold))
  "Face for system-browser definitions list headers"
  :group 'system-browser-faces
  )

;-------- Buffers ---------------------------------

(defvar-local esb:system-browser-buffer-type nil)

(defvar esb:mode-line-toggle-docs-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      (lambda (_e)
        (interactive "e")
        (wlf:toggle esb:wm 'documentation)))
    map))

(defun esb:setup-list-buffer ()
  ;; TODO: the following COPY-FACE is global. We need to do something to apply locally.
  (copy-face 'mode-line 'header-line)
  (setq header-line-format mode-line-format)
  (setq mode-line-format nil)
  (hl-line-mode)
  (system-browser-mode))

(defun esb:initialize-packages-buffer ()
  (setq esb:packages-buffer (get-buffer-create "*esb-packages*"))
  (with-current-buffer esb:packages-buffer
    (esb:setup-list-buffer)
    (when (esb:packages-buffer-mode-line-format esb:current-browser-system)
      (setq header-line-format (esb:packages-buffer-mode-line-format esb:current-browser-system)))
    (setq esb:system-browser-buffer-type 'packages)
    ))

(defun esb:initialize-categories-buffer ()
  (setq esb:categories-buffer (get-buffer-create "*esb-categories*"))
  (with-current-buffer esb:categories-buffer
    (esb:setup-list-buffer)
    (when (esb:categories-buffer-mode-line-format esb:current-browser-system)
      (setq header-line-format (esb:categories-buffer-mode-line-format esb:current-browser-system)))
    (setq esb:system-browser-buffer-type 'categories)))

(defun esb:initialize-definitions-buffer ()
  (setq esb:definitions-buffer (get-buffer-create "*esb-definitions*"))
  (with-current-buffer esb:definitions-buffer
    (esb:setup-list-buffer)
    (when (esb:definitions-buffer-mode-line-format esb:current-browser-system)
      (setq header-line-format (esb:definitions-buffer-mode-line-format esb:current-browser-system)))
    (setq esb:system-browser-buffer-type 'definitions)))

(defun esb:initialize-definition-buffer ()
  (setq esb:definition-buffer (get-buffer-create "*esb-definition*"))
  (with-current-buffer esb:definition-buffer
    (lisp-mode)

    ;; Show visited file in mode-line
    (setq mode-line-format (cons '(:eval (file-name-nondirectory buffer-file-name))
                                 (cdr mode-line-format)))

    ;; Buttons in mode-line
    (push '(:eval (propertize "toggle docs "
                              'local-map esb:mode-line-toggle-docs-map
                              'face 'esb:mode-line-buttons-face
                              'mouse-face 'mode-line-highlight))
          mode-line-format)
    (push '(:eval (propertize "quit "
                              'local-map quit-system-browser
                              'face 'esb:mode-line-buttons-face
                              'mouse-face 'mode-line-highlight))
          mode-line-format)

    (system-browser-mode)
    ))

(defun esb:initialize-documentation-buffer ()
  (setq esb:documentation-buffer (get-buffer-create "*esb-documentation*")))

(defun esb:update-packages-buffer ()
  (let ((packages (esb:list-packages esb:current-browser-system)))
    (with-current-buffer esb:packages-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (dolist (package-name packages)
        (insert-button (if esb:downcase-definition-names
                           (downcase package-name)
                         package-name)
                       'action (lambda (btn)
				 (esb:select-package package-name))
                       'face 'esb:definition-list-item-face
                       'follow-link t
                       'help-echo "Browse package")
        (newline))
      (setq buffer-read-only t))
    (wlf:select esb:wm 'packages)

    (esb:select-package (first packages))))

(defun esb:select-package (package)
  ;;(oset esb:current-browser-system selected-package package)
  (esb:update-categories-buffer package))

(defun esb:update-categories-buffer (package)
  (let ((categories (esb:list-categories esb:current-browser-system package)))
    (with-current-buffer esb:categories-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (propertize package 'face
                          'esb:definitions-list-header-face
                          ))
      (newline)
      (dolist (category categories)
        (insert-button category
                       'action (lambda (btn)
                                 (esb:select-category package category))
                       'follow-link t
                       'face 'esb:definition-list-item-face
                       'help-echo "Browse category")
        (newline))
      (setq buffer-read-only t))
    (wlf:select esb:wm 'categories)

    (let* ((package-properties (slime-eval `(esb::serialize-for-emacs (def-properties:package-properties ,package t))))
           (source (find :source package-properties :key 'car))
           (file (and source
                      (or (cadr (find :file (remove-if-not 'listp source) :key 'car))
                          (caddr (find :buffer-and-file (remove-if-not 'listp source) :key 'car)))))
           (position (and source (or
                                  (cadr (find :position (remove-if-not 'listp source) :key 'car))
                                  (cadr (find :offset (remove-if-not 'listp source) :key 'car))
                                  )))
           (documentation (cdr (assoc :documentation package-properties))))
      (if (and file position)
          (progn
            (esb:set-definition-buffer-file file position)
            (esb:set-documentation-buffer-contents (or documentation "This package is not documented."))
	    (esb:select-category package (first categories)))
        (message "Definition source not found.")
        ))))

(defun esb:select-category (package category)
  ;;(oset esb:current-browser-system selected-category  category)
  (esb:update-definitions-buffer package category))

(defun esb:update-definitions-buffer (package category)
  (with-current-buffer esb:definitions-buffer
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (propertize category 'face
                        'esb:definitions-list-header-face
                        ))
    (newline)
    (dolist (definition (esb:list-definitions esb:current-browser-system package category))
      (insert-button (if esb:downcase-definition-names
                         (downcase definition)
                       definition)
                     'action (lambda (btn)
			       (esb:select-definition package category definition))
                     'face 'esb:definition-list-item-face
                     'follow-link t
                     'help-echo "Browse definition")
      (newline))
    (setq buffer-read-only t))
  (wlf:select esb:wm 'definitions))

(defun esb:select-definition (package category definition)
  ;;(oset esb:current-browser-system selected-definition definition)
  (esb:update-definition-buffer package category definition)
  (esb:update-documentation-buffer package category definition))

(defun esb:set-definition-buffer-file (file &optional position)
  (block func
    (with-current-buffer esb:definition-buffer

      ;; Check for unsaved changes in definition buffer
      (when (buffer-modified-p)
        (when (not (yes-or-no-p "System Browser definition buffer modified. Discard changes? "))
          (return-from func)))

      (wlf:select esb:wm 'definition)

      (erase-buffer)
      (insert-file-contents file)
      ;; Assign file to buffer so changes in definition buffer can be saved
      (setq buffer-file-name file)
      (setq default-directory file)
      ;; For some reason, sometimes definition buffer sets to read-only.
      ;; The following prevents that:
      (setq buffer-read-only nil)
      (set-buffer-modified-p nil)
      (when position
        (goto-char position)
        (recenter-top-bottom 0)))))

(defun esb:update-definition-buffer (package category definition)
  (let ((definition-type
          (cond
           ((string= category "functions") :function)
           ((string= category "variables") :variable)
           ((string= category "macros") :macro)
           ((string= category "classes") :class)
           ((string= category "generic functions") :generic-function)
           (t (error "Invalid category: %s" category))))
        (definition-function
          (cond
           ((string= category "functions") 'def-properties:function-properties)
           ((string= category "variables") 'def-properties:variable-properties)
           ((string= category "macros") 'def-properties:macro-properties)
           ((string= category "classes") 'def-properties:class-properties)
           ((string= category "generic functions") 'def-properties:generic-function-properties)
           (t (error "Invalid category: %s" category))
           )))
    (let* ((definition-properties (slime-eval `(esb:serialize-for-emacs (,definition-function ',(intern (concat package "::" definition)) t))))
           (source (find :source definition-properties :key 'car))
           (file (and source (or
                              (cadr (find :file (remove-if-not 'listp source) :key 'car))
                              (caddr (find :buffer-and-file (remove-if-not 'listp source) :key 'car)))))
           (position (and source (or
                                  (cadr (find :position (remove-if-not 'listp source) :key 'car))
                                  (cadr (find :offset (remove-if-not 'listp source) :key 'car))))))
      (if (and file position)
          (with-current-buffer esb:definition-buffer
            (wlf:select esb:wm 'definition)
            (switch-to-buffer esb:definition-buffer nil t)
            (esb:set-definition-buffer-file file position))
        (message "Definition source not found.")))))

(defun esb:set-documentation-buffer-contents (contents)
  (with-current-buffer esb:documentation-buffer
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert contents)
    (goto-char 0)
    (setq buffer-read-only t)))

(defun esb:update-documentation-buffer (package category definition)
  (let ((definition-type
          (cond
           ((string= category "functions") :function)
           ((string= category "variables") :variable)
           ((string= category "macros") :macro)
           ((string= category "classes") :class)
           ((string= category "generic functions") :generic-function)
           ))
        (definition-function
          (cond
           ((string= category "functions") 'def-properties:function-properties)
           ((string= category "variables") 'def-properties:variable-properties)
           ((string= category "macros") 'def-properties:macro-properties)
           ((string= category "classes") 'def-properties:class-properties)
           ((string= category "generic functions") 'def-properties:generic-function-properties))))
    (let* ((definition-properties (slime-eval `(esb::serialize-for-emacs (,definition-function ',(intern (concat package "::" definition)) t))))
           (documentation (cdr (assoc :documentation definition-properties)))
           (contents (or documentation "This definition is not documented.")))
      (when (eql definition-type :variable)
        (setq contents (concat contents "\n\n"))
        (if (not (cdr (assoc :boundp definition-properties)))
            (setq contents (concat contents "The variable is UNBOUND."))
          (progn
            (setq contents (concat contents (propertize "Variable value: " 'face 'bold)))
            (setq contents (concat contents (cdr (assoc :value definition-properties)))))))
      (esb:set-documentation-buffer-contents contents))))

(defmethod esb:list-categories ((system esb:common-lisp-system) package)
  '("functions" "variables" "macros" "classes" "generic functions"))

(defmethod esb:list-definitions ((system esb:common-lisp-system) package category)
  (let ((definition-type
          (cond
           ((string= category "functions") :function)
           ((string= category "variables") :variable)
           ((string= category "macros") :macro)
           ((string= category "classes") :class)
           ((string= category "generic functions") :generic-function))))

    (slime-eval `(esb:list-definitions ,package ,definition-type :include-internal-p ,esb:list-internal-definitions))))

;---- Window management ---------------------------

(defvar esb:wm)

(defun esb:initialize-windows ()
  "Mark selection windows as dedicated."
  (let ((winfo-list (wlf:wset-winfo-list esb:wm)))
    (set-window-dedicated-p (wlf:window-window (wlf:get-winfo 'packages winfo-list)) t)
    (set-window-dedicated-p (wlf:window-window (wlf:get-winfo 'categories winfo-list)) t)
    (set-window-dedicated-p (wlf:window-window (wlf:get-winfo 'definitions winfo-list)) t)))

(defun system-browser ()
  "Open the currently instantiated system browser."
  (interactive)

  (block system-browser

    ;; Start SLIME if needed
    (when (not (slime-connected-p))
      (when (or esb:start-slime-automatically
		(yes-or-no-p "SLIME is not connected. Start? "))
	(add-hook 'slime-connected-hook 'system-browser)
	(slime))
      (return-from system-browser))

    ;; Initialize system browser buffers
    (esb:initialize-packages-buffer)
    (esb:initialize-categories-buffer)
    (esb:initialize-definitions-buffer)
    (esb:initialize-definition-buffer)
    (esb:initialize-documentation-buffer)

    (setq esb:wm
          (wlf:layout
           '(| (:left-size-ratio 0.20)
               (- (:left-size-ratio 0.33)
                  packages
                  (- categories
                     definitions))
               (- (:left-size-ratio 0.66)
                  definition
                  documentation))
           '((:name packages
                    :buffer "*esb-packages*")
             (:name categories
                    :buffer "*esb-categories*")
             (:name definitions
                    :buffer "*esb-definitions*")
             (:name definition
                    :buffer "*esb-definition*")
             (:name documentation
                    :buffer "*esb-documentation*")
             )))

    (when (not esb:show-documentation-buffer)
      (wlf:hide esb:wm 'documentation))

    (esb:initialize-windows)
    (esb:update-packages-buffer)
    (wlf:select esb:wm 'packages)))

;------- Commands ------------------------------------------------

(defun lisp-system-browser ()
  "Open the Common Lisp system browser."
  (interactive)
  (setq esb:current-browser-system (make-instance 'esb:common-lisp-system))
  (system-browser))

(defun system-browser-reset-layout ()
  "Reset system browser layout. Use this when Emacs windows break the browser's layout."
  (interactive)
  (wlf:reset-window-sizes esb:wm)
  (esb:initialize-windows))

(defun quit-system-browser ()
  "Quit the system browser."
  (interactive)
  (kill-buffer esb:packages-buffer)
  (kill-buffer esb:categories-buffer)
  (kill-buffer esb:definitions-buffer)
  (kill-buffer esb:definition-buffer)
  (kill-buffer esb:documentation-buffer)
  (wlf:clear-windows esb:wm t))

(defun esb:read-name ()
  (case esb:system-browser-buffer-type
    (package (slime-read-package-name "Package: "))
    (definitions (slime-read-symbol-name "Symbol: "))))

;; (defun esb:goto (name)
;;   (case esb:system-browser-buffer-type
;;     (package (esb:update-categories-buffer name))
;;     (definitions (esb:update-definition-buffer))))

;; (defun system-browser-goto (name)
;;   (interactive (list (esb:read-name)))

;;   (esb:goto name))

(defun system-browser-browse-package (package-name)
  "Browse a particular package completed from command bar."
  (interactive (list (slime-read-package-name "Browse package: ")))
  (esb:select-package package-name))

(defun system-browser-browse-definition (name)
  (interactive))

(defun system-browser-refresh ()
  "Refresh the system browser contents and reset its layout."
  (interactive)
  (system-browser))

(defun system-browser-toggle-docs ()
  "Toggle documentation panel in system browser."
  (interactive)
  (wlf:toggle esb:wm 'documentation))

(defun system-browser-toggle-internal-definitions ()
  "Toggle internal definitions listing in system browser."
  (interactive)
  (setq esb:list-internal-definitions (not esb:list-internal-definitions))
  (system-browser-refresh))

(defun system-browser-customize ()
  "Customize system browser."
  (interactive)
  (customize-group 'system-browser))

(defun system-browser-help ()
  "Show help about system browser."
  (interactive)
  (apropos-command "system-browser"))

;------ Menu ----------------------------

(defvar system-browser-mode-map
  (let ((map (make-keymap)))
    (define-key map "C-q" 'quit-system-browser)
    map))

(define-minor-mode system-browser-mode
  "System browser minor mode."
  :init-value nil
  :lighter " system-browser"
  :keymap system-browser-mode-map
  :group 'system-browser)

(easy-menu-define
  system-browser-mode-menu system-browser-mode-map
  "Menu for system-browser"
  '("System Browser"
    ["Browse package..." system-browser-browse-package
     :help "Browse a package"]
    ["Browse definition..." system-browser-browse-definition
     :help "Browse a definition"]
    "--"
    ["Refresh browser" system-browser-refresh
     :help "Refresh the system browser"]
    ["Reset layout" system-browser-reset-layout
     :help "Reset system browser windows layout"]
    ["Toggle internal defintions" system-browser-toggle-internal-definitions
     :help "Toggle the listing of internal defintions"]
    ["Toggle documentation panel" system-browser-toggle-docs
     :help "Toggle documentation panel"]
    "--"
    ["Settings" system-browser-customize
     :help "Customize system browser"]
    ["Help" system-browser-help
     :help "Help on system browser"]
    ["Quit" quit-system-browser
     :help "Quit System Browser"]))

;------ SLIME --------------------------------------------

(define-slime-contrib system-browser
  "Smalltalk-like system browser for Common Lisp"
  (:authors "Mariano Montone")
  (:license "GPL")
  (:swank-dependencies emacs-system-browser))

(provide 'system-browser)

;;; system-browser.el ends here
