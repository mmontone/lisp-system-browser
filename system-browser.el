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

(defun alist-to-plist (alist)
  (let ((plist '()))
    (dolist (cons alist)
      (push (car cons) plist)
      (push (cdr cons) plist))
    (reverse plist)))

;;(alist-to-plist '((a . 22) (b . "asf")))

(defgroup system-browser nil
  "System browser configuration")

(defcustom sb:show-documentation-buffer nil
  "Show documentation buffer in system browser."
  :type 'boolean
  :group 'system-browser
  :tag "Show documentation buffer")

(defcustom sb:downcase-definition-names t
  "Show definition names in lowercase."
  :type 'boolean
  :group 'system-browser
  :tag "Downcase definition names")

(defface sb:definition-list-item-face
  '((t :foreground "black"
       :height 0.9))
  "Face for definitions list items"
  :group 'system-browser-faces)

(defun sb:setup-list-buffer ()
  ;; TODO: use a minor mode for list buffer to set this
  (apply 'set-face-attribute
         'header-line nil
         (alist-to-plist (face-all-attributes 'mode-line)))
  (setq header-line-format mode-line-format)
  (setq mode-line-format nil)
  (hl-line-mode)
  (system-browser-mode))

(defvar-local sb:system-browser-buffer-type nil)

(defun sb:initialize-packages-buffer ()
  (setq sb:packages-buffer (get-buffer-create "*sb-packages*"))
  (with-current-buffer sb:packages-buffer
    (sb:setup-list-buffer)
    (when (sb:packages-buffer-mode-line-format sb:current-browser-system)
      (setq header-line-format (sb:packages-buffer-mode-line-format sb:current-browser-system)))
    (setq sb:system-browser-buffer-type 'packages)
    ))

(defun sb:initialize-categories-buffer ()
  (setq sb:categories-buffer (get-buffer-create "*sb-categories*"))
  (with-current-buffer sb:categories-buffer
    (sb:setup-list-buffer)
    (when (sb:categories-buffer-mode-line-format sb:current-browser-system)
      (setq header-line-format (sb:categories-buffer-mode-line-format sb:current-browser-system)))
    (setq sb:system-browser-buffer-type 'categories)))

(defun sb:initialize-definitions-buffer ()
  (setq sb:definitions-buffer (get-buffer-create "*sb-definitions*"))
  (with-current-buffer sb:definitions-buffer
    (sb:setup-list-buffer)
    (when (sb:definitions-buffer-mode-line-format sb:current-browser-system)
      (setq header-line-format (sb:definitions-buffer-mode-line-format sb:current-browser-system)))
    (setq sb:system-browser-buffer-type 'definitions)))

(defvar sb:mode-line-toggle-docs-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      (lambda (_e)
        (interactive "e")
        (wlf:toggle sb:wm 'documentation)))
    map))

(defun sb:initialize-definition-buffer ()
  (setq sb:definition-buffer (get-buffer-create "*sb-definition*"))
  (with-current-buffer "*sb-definition*"
    (lisp-mode)

    ;; Show visited file in mode-line
    (setq mode-line-format (cons '(:eval (file-name-nondirectory buffer-file-name))
                                 (cdr mode-line-format)))

    ;; Buttons in mode-line
    (push '(:eval (propertize "[toggle docs] "
                              'local-map sb:mode-line-toggle-docs-map))
          mode-line-format)
    (push '(:eval (propertize "[quit] "
                              'local-map quit-system-browser))
          mode-line-format)

    (system-browser-mode)
    ))

(defun sb:initialize-documentation-buffer ()
  (setq sb:documentation-buffer (get-buffer-create "*sb-documentation*")))

(defun sb:update-packages-buffer ()
  (let ((packages (sb:list-packages sb:current-browser-system)))
    (with-current-buffer sb:packages-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (dolist (package-name packages)
        (insert-button (if sb:downcase-definition-names
                           (downcase package-name)
                         package-name)
                       'action (lambda (btn)
                                 (message package-name)
                                 (sb:update-categories-buffer package-name))
                       'face 'sb:definition-list-item-face
                       'follow-link t
                       'help-echo "Browse package")
        (newline))
      (setq buffer-read-only t))
    (wlf:select sb:wm 'packages)
    (sb:update-categories-buffer (first packages))))

(defun sb:update-categories-buffer (package)
  (let ((categories (sb:list-categories sb:current-browser-system package)))
    (with-current-buffer sb:categories-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert package)
      (newline)
      (dolist (category categories)
        (insert-button category
                       'action (lambda (btn)
                                 (sb:update-definitions-buffer package category))
                       'follow-link t
		       'face 'sb:definition-list-item-face
                       'help-echo "Browse category")
        (newline))
      (setq buffer-read-only t))
    (wlf:select sb:wm 'categories)

    (let* ((package-properties (slime-eval `(esb::serialize-for-emacs (def-properties:package-properties ,package t))))
           (source (find :source package-properties :key 'car))
           (file (cadr (find :file (remove-if-not 'listp source) :key 'car)))
           (position (cadr (find :position (remove-if-not 'listp source) :key 'car)))
           (documentation (cdr (assoc :documentation package-properties))))
      (sb:set-definition-buffer-file file position)
      (sb:set-documentation-buffer-contents (or documentation "")))

    (sb:update-definitions-buffer package (first categories))
    ))

(defun sb:update-definitions-buffer (package category)
  (with-current-buffer sb:definitions-buffer
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert category)
    (newline)
    (dolist (definition (sb:list-definitions sb:current-browser-system package category))
      (insert-button (if sb:downcase-definition-names
                         (downcase definition)
                       definition)
                     'action (lambda (btn)
                               (sb:update-definition-buffer package category definition)
                               (sb:update-documentation-buffer package category definition))
		     'face 'sb:definition-list-item-face
                     'follow-link t
                     'help-echo "Browse definition")
      (newline))
    (setq buffer-read-only t))
  (wlf:select sb:wm 'definitions))

(defun sb:set-definition-buffer-file (file &optional position)
  (block func
    (with-current-buffer sb:definition-buffer

      ;; Check for unsaved changes in definition buffer
      (when (buffer-modified-p)
        (when (not (yes-or-no-p "System Browser definition buffer modified. Discard changes? "))
          (return-from func)))

      (wlf:select sb:wm 'definition)

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

(defun sb:update-definition-buffer (package category definition)
  (let ((definition-type
          (cond
           ((string= category "functions") :function)
           ((string= category "variables") :variable)
           ((string= category "macros") :macro)
           ((string= category "classes") :class)))
        (definition-function
          (cond
           ((string= category "functions") 'def-properties:function-properties)
           ((string= category "variables") 'def-properties:variable-properties)
           ((string= category "macros") 'def-properties:macro-properties)
           ((string= category "classes") 'def-properties:class-properties))))
    (let* ((definition-properties (slime-eval `(esb::serialize-for-emacs (,definition-function ',(make-symbol (concat package "::" definition))))))
           (source (find :source definition-properties :key 'car))
           (file (cadr (find :file (remove-if-not 'listp source) :key 'car)))
           (position (cadr (find :position (remove-if-not 'listp source) :key 'car))))
      (with-current-buffer "*sb-definition*"
        (wlf:select sb:wm 'definition)
        (switch-to-buffer "*sb-definition*" nil t)
        (sb:set-definition-buffer-file file position)))))

(defun sb:set-documentation-buffer-contents (contents)
  (with-current-buffer "*sb-documentation*"
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert contents)
    (goto-char 0)
    (setq buffer-read-only t)))

(defun sb:update-documentation-buffer (package category definition)
  (let ((definition-type
          (cond
           ((string= category "functions") :function)
           ((string= category "variables") :variable)
           ((string= category "macros") :macro)
           ((string= category "classes") :class)))
        (definition-function
          (cond
           ((string= category "functions") 'def-properties:function-properties)
           ((string= category "variables") 'def-properties:variable-properties)
           ((string= category "macros") 'def-properties:macro-properties)
           ((string= category "classes") 'def-properties:class-properties))))
    (let* ((definition-properties (slime-eval `(esb::serialize-for-emacs (,definition-function ',(make-symbol (concat package "::" definition))))))
           (documentation (cdr (assoc :documentation definition-properties))))
      (sb:set-documentation-buffer-contents (or documentation "")))))

(defmethod sb:list-categories ((system sb:common-lisp-system) package)
  '("variables" "macros" "functions" "classes" "generic functions"))

(defmethod sb:list-definitions ((system sb:common-lisp-system) package category)
  (let ((definition-type
          (cond
           ((string= category "functions") :function)
           ((string= category "variables") :variable)
           ((string= category "macros") :macro)
           ((string= category "classes") :class))))

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
         '(| (:left-size-ratio 0.20)
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

  (when (not sb:show-documentation-buffer)
    (wlf:hide sb:wm 'documentation))

  ;; Mark selection windows as dedicated
  (let ((winfo-list (wlf:wset-winfo-list sb:wm)))
    (set-window-dedicated-p (wlf:window-window (wlf:get-winfo 'packages winfo-list)) t)
    (set-window-dedicated-p (wlf:window-window (wlf:get-winfo 'categories winfo-list)) t)
    (set-window-dedicated-p (wlf:window-window (wlf:get-winfo 'definitions winfo-list)) t))

  (sb:update-packages-buffer)
  (wlf:select sb:wm 'packages))

(defun quit-system-browser ()
  (interactive)
  (kill-buffer sb:packages-buffer)
  (kill-buffer sb:categories-buffer)
  (kill-buffer sb:definitions-buffer)
  (kill-buffer sb:definition-buffer)
  (wlf:clear-windows sb:wm t))

(defun sb:read-name ()
  (case sb:system-browser-buffer-type
    (package (slime-read-package-name "Package: "))
    (definitions (slime-read-symbol-name "Symbol: "))))

(defun sb:goto (name)
  (case sb:system-browser-buffer-type
    (package (sb:update-categories-buffer name))
    (definitions (sb:update-definition-buffer))))

(defun system-browser-goto (name)
  (interactive (list (sb:read-name)))

  (sb:goto name))

(defun system-browser-browse-package (name)
  (interactive))

(defun system-browser-browse-definition (name)
  (interactive))

(defun system-browser-toggle-docs ()
  (interactive)
  (wlf:toggle sb:wm 'documentation))

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
    ["Refresh browser" system-browser
     :help "Refresh the system browser"]
    ["Toggle documentation panel" system-browser-toggle-docs
     :help "Toggle documentation panel"]
    "--"
    ["Quit" quit-system-browser
     :help "Quit System Browser"]))
