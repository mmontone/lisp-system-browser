;; Copyright (C) 2021 Mariano Montone

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require :alexandria)
(require :swank)
(require :closer-mop)

(defpackage :def-properties
  (:use :cl)
  (:export
   :symbol-properties
   :variable-properties
   :function-properties
   :macro-properties
   :class-properties
   :type-properties
   :package-properties
   :generic-function-properties
   :parse-docstring
   :list-lambda-list-args
   :asdf-system-packages

   :symbol-kinds
   :symbol-kind-p
   :symbol-variable-p
   :symbol-function-p
   :symbol-macro-p
   :symbol-generic-function-p
   :symbol-type-p
   :symbol-class-p
   :symbol-structure-p)
  (:documentation "Collects properties about Lisp definitions, in a portable way"))

(in-package :def-properties)

;; TODO: support all aspects from swank::describe-symbol-for-emacs:
;; :VARIABLE :FUNCTION :SETF :SPECIAL-OPERATOR :MACRO :COMPILER-MACRO
;; :TYPE :CLASS :ALIEN-TYPE :ALIEN-STRUCT :ALIEN-UNION :ALIEN-ENUM

(defun symbol-kinds (symbol)
  "Return the kinds of the SYMBOL."
  (remove-if-not 'symbolp (swank::describe-symbol-for-emacs symbol)))

(defun symbol-kind-p (symbol kind)
  (find kind (symbol-kinds symbol)))

(defun symbol-variable-p (symbol)
  (symbol-kind-p symbol :variable))

(defun symbol-function-p (symbol)
  (symbol-kind-p symbol :function))

(defun symbol-macro-p (symbol)
  (symbol-kind-p symbol :macro))

(defun symbol-generic-function-p (symbol)
  (symbol-kind-p symbol :generic-function))

(defun symbol-type-p (symbol)
  (symbol-kind-p symbol :type))

(defun symbol-class-p (symbol)
  (find-class symbol nil))

(defun symbol-structure-p (symbol)
  (and (find-class symbol nil)
       (typep (find-class symbol nil) 'structure-class)))

(defun symbol-properties (symbol &optional shallow)
  "Collects properties about a symbol.
If TYPE is specified, then SYMBOL is treated as the given TYPE (variable, function, package, etc).
If SHALLOW is T, then only fundamental properties are collected.
Returns a list of alists of properties, one alist for each type of definition that SYMBOL is bound to."
  (let (properties)
    (when (symbol-function-p symbol)
      (push (function-properties symbol shallow) properties))
    (when (symbol-generic-function-p symbol)
      (push (function-properties symbol shallow) properties))
    (when (symbol-macro-p symbol)
      (push (function-properties symbol shallow) properties))
    (when (symbol-variable-p symbol)
      (push (variable-properties symbol shallow) properties))
    (when (symbol-class-p symbol)
      (push (class-properties symbol shallow) properties))
    ;; TODO
    #+nil(when (symbol-type-p symbol)
           (push (type-properties symbol shallow) properties))
    properties))

(defun aget (alist key)
  (cdr (assoc key alist :test 'equalp)))

(defun package-properties (package &optional shallow)
  (let ((pck (find-package package)))
    (list
     (cons :name (package-name pck))
     (cons :type :package)
     (cons :documentation (documentation pck t))
     (unless shallow
       (cons :external-symbols
             (let (docs)
               (do-external-symbols (symbol package)
                 (alexandria:when-let ((symbol-properties (symbol-properties symbol)))
                   (push symbol-properties docs)))
               docs)))
     (cons :source (swank/backend:find-source-location pck)))))

;; From docbrowser

(defun nice-princ-to-string (obj)
  (typecase obj
    (string obj)
    (keyword (prin1-to-string obj))
    (t (princ-to-string obj))))

#+sbcl(defmethod documentation ((slotd sb-pcl::condition-effective-slot-definition) (doc-type (eql 't)))
        "This method definition is missing in SBCL as of 1.0.55 at least. Adding it here
will make documentation for slots in conditions work properly."
        (slot-value slotd 'sb-pcl::%documentation))

;; SWANK CCL patch

#+ccl
(swank/ccl::defimplementation swank::describe-symbol-for-emacs (symbol)
  (let ((result '()))
    (flet ((doc (kind &optional (sym symbol))
             (or (documentation sym kind) :not-documented))
           (maybe-push (property value)
             (when value
               (setf result (list* property value result)))))
      (maybe-push
       :variable (when (boundp symbol)
                   (doc 'variable)))
      (maybe-push
       :macro (when (macro-function symbol)
                (doc 'function)))
      (maybe-push
       :function (if (and (not (macro-function symbol)) (fboundp symbol))
                     (doc 'function)))
      (maybe-push
       :setf (let ((setf-function-name (ccl:setf-function-spec-name
                                        `(setf ,symbol))))
               (when (fboundp setf-function-name)
                 (doc 'function setf-function-name))))
      (maybe-push
       :type (when (ccl:type-specifier-p symbol)
               (doc 'type)))
      result)))

;; Some Swank backends support getting the source location of a SYMBOL, and others not. 
#-sbcl
(defun variable-source-location (name)
  (swank/backend:find-source-location name))

#+sbcl
(defun variable-source-location (name)
  (alexandria:when-let ((definition-source (first (sb-introspect:find-definition-sources-by-name name :variable))))
    (swank/sbcl::definition-source-for-emacs definition-source :variable name)))

#-sbcl
(defun macro-source-location (name)
  (swank/backend:find-source-location name))

#+sbcl
(defun macro-source-location (name)
  (alexandria:when-let ((definition-source (first (sb-introspect:find-definition-sources-by-name name :macro))))
    (swank/sbcl::definition-source-for-emacs definition-source :macro name)))

(defun assoc-cdr (key data &key error-p)
  "Return (CDR (ASSOC KEY DATA)). If ERROR-P is non-NIL, signal an error if KEY is
not available is DATA."
  (let ((v (assoc key data)))
    (when (and error-p
               (not v))
      (error "~s not found in data" key))
    (cdr v)))

(defun prin1-to-string-with-package (obj package)
  (let ((*package* package))
    (prin1-to-string obj)))

(defun format-argument-to-string (arg)
  (etypecase arg
    (symbol (nice-princ-to-string arg))
    (list   (mapcar #'(lambda (entry conversion) (funcall conversion entry))
                    arg (list #'(lambda (v)
                                  (if (listp v)
                                      (nice-princ-to-string (car v))
                                      (nice-princ-to-string v)))
                              #'prin1-to-string
                              #'nice-princ-to-string)))))

(defun type-properties (symbol)
  (list (cons :name symbol)
        (cons :package (symbol-package symbol))
        (cons :type :type)
        (cons :documentation (documentation symbol 'type))))

(defun macro-properties (symbol &optional shallow)
  (list (cons :name symbol)
        (cons :documentation (documentation symbol 'function))
        (cons :args (let ((*print-case* :downcase)
                          (*print-pretty* nil)
                          (*package* (symbol-package symbol)))
                      #+nil(format nil "~{~a~^ ~}"
                                   (mapcar #'format-argument-to-string (swank-backend:arglist symbol))
                                   )
                      (prin1-to-string (swank-backend:arglist symbol))))
        (cons :arglist (swank::arglist symbol))
        (cons :package (symbol-package symbol))
        (cons :type :macro)
        (cons :source (macro-source-location symbol))))


(defun function-properties (symbol &optional shallow)
  (list (cons :name symbol)
        (cons :documentation (documentation symbol 'function))
        (cons :args (let ((*print-case* :downcase)
                          (*print-pretty* nil)
                          (*package* (symbol-package symbol)))
                      #+nil(format nil "~{~a~^ ~}"
                                   (mapcar #'format-argument-to-string (swank-backend:arglist symbol))
                                   )
                      (prin1-to-string (swank-backend:arglist symbol))))
        (cons :arglist (swank::arglist symbol))
        (cons :package (symbol-package symbol))
        (cons :type (cond ((macro-function symbol) :macro)
                          ((typep (symbol-function symbol) 'generic-function) :generic-function)
                          (t :function)))
        (cons :source (swank/backend:find-source-location (symbol-function symbol)))))

(defun generic-function-properties (symbol &optional shallow)
  (assert (typep (symbol-function symbol) 'generic-function))
  (list (cons :name symbol)
        (cons :documentation (documentation symbol 'function))
        (cons :args (let ((*print-case* :downcase)
                          (*print-pretty* nil)
                          (*package* (symbol-package symbol)))
                      #+nil(format nil "~{~a~^ ~}"
                                   (mapcar #'format-argument-to-string (swank-backend:arglist symbol))
                                   )
                      (prin1-to-string (swank-backend:arglist symbol))))
        (cons :arglist (swank::arglist symbol))
        (cons :package (symbol-package symbol))
        (cons :type :generic-function)
	(cons :source (swank/backend:find-source-location (symbol-function symbol)))
        (unless shallow
          (cons :methods (closer-mop:generic-function-methods (symbol-function symbol))))))

(defun variable-properties (symbol &optional shallow)
  (list (cons :name symbol)
        (cons :documentation (documentation symbol 'variable))
        (cons :boundp (boundp symbol))
        (cons :value (when (boundp symbol) (prin1-to-string (symbol-value symbol))))
        (cons :constant-p (constantp symbol))
        (cons :package (symbol-package symbol))
        (cons :type :variable)
        ;; TODO: fix me
        ;;(cons :source (swank/backend:find-source-location symbol))
        (cons :source (variable-source-location symbol))
        ))

(defun find-superclasses (class)
  (labels ((f (classes found)
             (if (and classes
                      (not (eq (car classes) (find-class 'standard-object)))
                      (not (member (car classes) found)))
                 (f (cdr classes)
                    (f (closer-mop:class-direct-superclasses (car classes))
                       (cons (car classes) found)))
                 found)))
    (f (list class) nil)))

(defun assoc-name (v)
  (assoc-cdr :name v :error-p t))

(defun specialise->symbol (spec)
  (case (caar spec)
    ((defmethod) (cadar spec))
    #+ccl((ccl::reader-method) (cadr (assoc :method (cdar spec))))
    (t nil)))

(defun symbol-external-p (symbol &optional (package (symbol-package symbol)))
  "Return non-NIL if SYMBOL is external in PACKAGE. SYMBOL may be either
a symbol, or a SETF form, in which case the check will be performed on
the CADR of the list."
  (eq (nth-value
       1
       (find-symbol
        (symbol-name
         (cond ((symbolp symbol)
                symbol)
               ((eq (car symbol) 'setf)
                (cadr symbol))
               (t (error "Unknown symbol type: ~s" symbol))))
        package))
      :external))

(defun specialisation-properties (class-name &key include-internal)
  (let* ((ignored '(initialize-instance))
         (class (if (symbolp class-name) (find-class class-name) class-name))
         (spec (swank-backend:who-specializes class)))
    (unless (eq spec :not-implemented)
      (sort (loop
              for v in spec
              for symbol = (specialise->symbol v)
              when (and (not (member symbol ignored))
                        (or include-internal
                            (symbol-external-p symbol (symbol-package (class-name class)))))
                collect (list (cons :name symbol) (cons :documentation (documentation symbol 'function))))
            #'string< :key (alexandria:compose #'princ-to-string #'assoc-name)))))

(defun %ensure-external (symbol)
  (let ((name (cond ((symbolp symbol)
                     symbol)
                    ((and (listp symbol) (eq (car symbol) 'setf))
                     (cadr symbol))
                    (t
                     (warn "Unknown type: ~s. Expected symbol or SETF form." symbol)
                     nil))))
    (when (swank::symbol-external-p name)
      symbol)))

(defun accessor-properties (class slot)
  (flet ((getmethod (readerp method-list)
           (dolist (method method-list)
             (let ((name (closer-mop:generic-function-name (closer-mop:method-generic-function method))))
               (when (and (eq (type-of method) (if readerp
                                                   'closer-mop:standard-reader-method
                                                   'closer-mop:standard-writer-method))
                          (eq (closer-mop:slot-definition-name (closer-mop:accessor-method-slot-definition method))
                              (closer-mop:slot-definition-name slot)))
                 (return-from getmethod name))))))

    ;; There are several different situations we want to detect:
    ;;   1) Only a reader method: "reader FOO"
    ;;   2) Only a writer method: "writer FOO"
    ;;   3) Only a writer SETF method: "writer (SETF FOO)"
    ;;   4) A reader and a SETF method: "accessor FOO"
    ;;   5) A reader and non-SETF writer: "reader FOO, writer FOO"
    ;;
    ;; The return value from this function is an alist of the following form:
    ;;
    ;;  ((:READER . FOO-READER) (:WRITER . FOO-WRITER) (:ACCESSOR . FOO-ACCESSOR))
    ;;
    ;; Note that if :ACCESSOR is given, then it's guaranteed that neither
    ;; :READER nor :WRITER will be included.
    ;;
    ;; We start by assigning the reader and writer methods to variables
    (let* ((method-list (closer-mop:specializer-direct-methods class))
           (reader (%ensure-external (getmethod t method-list)))
           (writer (%ensure-external (getmethod nil method-list))))
      ;; Now, detect the 5 different cases, but we coalease case 2 and 3.
      (cond ((and reader (null writer))
             `((:reader . ,reader)))
            ((and (null reader) writer)
             `((:writer . ,writer)))
            ((and reader (listp writer) (eq (car writer) 'setf) (eq (cadr writer) reader))
             `((:accessor . ,reader)))
            ((and reader writer)
             `((:reader . ,reader) (:writer . ,writer)))))))

(defun load-slots (class)
  (closer-mop:ensure-finalized class)
  (flet ((load-slot (slot)
           (list (cons :name (string (closer-mop:slot-definition-name slot)))
                 (cons :documentation (swank-mop:slot-definition-documentation slot))
                 (when (swank-mop:slot-definition-documentation slot)
                   (cons :parsed-documentation (parse-docstring (swank-mop:slot-definition-documentation slot) nil :package (symbol-package (class-name class)))))
                 ;; The LIST call below is because the accessor lookup is wrapped
                 ;; in a FOR statement in the template.
                 (cons :accessors (let ((accessor-list (accessor-properties class slot)))
                                    (when accessor-list
                                      (list accessor-list)))))))
    (mapcar #'load-slot (closer-mop:class-slots class))))

(defun class-properties (class-name &optional shallow)
  (let ((cl (find-class class-name)))
    (list (cons :name          (class-name cl))
          (cons :documentation (documentation cl 'type))
          (unless shallow (cons :slots (load-slots cl)))
          (unless shallow (cons :methods (specialisation-properties cl)))
          (unless shallow (cons :class-precedence-list (mapcar 'class-name (find-superclasses cl))))
          (unless shallow (cons :direct-superclasses (mapcar 'class-name (closer-mop:class-direct-superclasses cl))))
          (unless shallow (cons :direct-subclasses (mapcar 'class-name (closer-mop:class-direct-subclasses cl))))
          (cons :source (swank/backend:find-source-location cl))
          (cons :package (symbol-package class-name))
          (cons :type :class))))

(defun %annotate-function-properties (fn-properties classes)
  "Append :ACCESSORP tag if the function is present as an accessor function."
  (loop
    with name = (cdr (assoc :name fn-properties))
    for class-properties in classes
    do (loop
         for slot-properties in (cdr (assoc :slots class-properties))
         do (loop
              for accessor in (cdr (assoc :accessors slot-properties))
              for accessor-sym = (cdar accessor)
              when (or (and (symbolp accessor-sym) (eq accessor-sym name))
                       (and (listp accessor-sym) (eq (car accessor-sym) 'setf) (eq (cadr accessor-sym) name)))
                do (return-from %annotate-function-properties (append fn-properties '((:accessorp t))))))
    finally (return fn-properties)))

;; docbrowser stuff ends here

(defun concat-rich-text (text)
  (when (stringp text)
    (return-from concat-rich-text text))
  (let ((segments nil)
        (segment nil))
    (loop for word in text
          do (if (stringp word)
                 (push word segment)
                 ;; else, it is an "element"
                 (destructuring-bind (el-type content &rest args) word
                   (push (apply #'concatenate 'string (nreverse segment))
                         segments)
                   (setf segment nil)
                   (push (list* el-type (concat-rich-text content) args)
                         segments)))
          finally (when segment
                    (push (apply #'concatenate 'string (nreverse segment))
                          segments)))
    (nreverse segments)))

(defun split-string-with-delimiter (string delimiter
                                    &key (keep-delimiters t)
                                    &aux (l (length string)))
  (let ((predicate (cond
                     ((characterp delimiter) (lambda (char) (eql char delimiter)))
                     ((listp delimiter) (lambda (char) (member char delimiter)))
                     ((functionp delimiter) delimiter)
                     (t (error "Invalid delimiter")))))
    (loop for start = 0 then (1+ pos)
          for pos   = (position-if predicate string :start start)

          ;; no more delimiter found
          when (and (null pos) (not (= start l)))
            collect (subseq string start)

          ;; while delimiter found
          while pos

          ;;  some content found
          when (> pos start) collect (subseq string start pos)
            ;;  optionally keep delimiter
            when keep-delimiters collect (string (aref string pos)))))

(defun list-lambda-list-args (lambda-list)
  "Takes a LAMBDA-LIST and returns the list of all the argument names."
  (loop for symbol in (alexandria:flatten lambda-list)
        when (and (symbolp symbol)
                  (not (char-equal (aref (symbol-name symbol) 0) #\&))) ;; special argument
          collect symbol))

;; (list-lambda-list-args '(foo))
;; (list-lambda-list-args '(foo &optional bar))
;; (list-lambda-list-args '(foo &optional (bar 22)))
;; (list-lambda-list-args '(foo &optional (bar 22) &key key (key2 33) &rest args &body body))
;; (list-lambda-list-args '((stream-name file-name &rest args &key (direction) &allow-other-keys) &body body))

(defmacro aand (arg1 &rest args)
  `(let ((it ,arg1))
     (and it ,@args)))

(defun quoted-symbol-p (string)
  (and (eql (aref string 0) #\`)
       (eql (aref string (1- (length string))) #\')))

(defun quoted-symbol-name (string)
  (string-upcase (subseq string 1 (1- (length string)))))

;; TODO: consider elisp style docstring parsing.
;; See: https://www.gnu.org/software/emacs/manual/html_node/elisp/Documentation-Tips.html
;; For example, option to only parse quoted words as function/variable references, like `my-function`. Also, consider using `(type)symbol` syntax to disambiguate, like `(function)my-function`.
(defun parse-docstring (docstring bound-args &key case-sensitive ignore (package *package*))
  "Parse a docstring.
BOUND-ARGS: when parsing a function/macro/generic function docstring, BOUND-ARGS contains the names of the arguments. That means the function arguments are detected by the parser.
CASE-SENSITIVE: when case-sensitive is T, bound arguments are only parsed when in uppercase.
IGNORE: an optional predicate. When ignore is given and invoking it returns T, the current word is not parsed as special symbol.
PACKAGE: the package to use to read the docstring symbols.
"
  (let ((words (split-string-with-delimiter
                docstring
                (lambda (char)
                  (not
                   (or (alphanumericp char)
                       (find char "+-*/@$%^&_=<>~:'`"))))))
        (string-test (if case-sensitive
                         'string=
                         'equalp)))
    (concat-rich-text
     (loop for word in words
           collect (cond
                     ((and ignore
                           (funcall ignore word))
                      ;; don't parse as special
                      word)
                     ((eql (aref word 0) #\:)
                      (list :key word))
                     ((member (string-upcase word) (mapcar 'symbol-name bound-args) :test string-test)
                      (list :arg word (aand (find-symbol (string-upcase word) package)
                                            (aand (find-class it nil)
                                                  :class))))
                     ((and (position #\: word) ;; could be a qualified symbol
                           (ignore-errors (read-from-string word)))
                      (let ((symbol (let ((*package* package)) (read-from-string word))))
                        (cond
                          ((ignore-errors (fboundp symbol))
                           (list :fn word symbol))
                          ((ignore-errors (boundp symbol))
                           (list :var word symbol))
                          (t ;; I don't know what this is
                           word))))
                     ((aand
                       (find-symbol word package)
                       (fboundp it))
                      (list :fn word (find-symbol word package)))
                     ((aand
                       (find-symbol word package)
                       (find-class it nil))
                      (list :class word (find-symbol word package)))
                     ((aand (find-symbol word package)
                            (boundp it))
                      (list :var word (find-symbol word package)))
                     ((quoted-symbol-p word)
                      (let ((symbol (find-symbol (quoted-symbol-name word) package)))
                        (cond
                          ((member (symbol-name symbol) (mapcar 'symbol-name bound-args) :test string-test)
                           (list :arg word symbol))
                          ((fboundp symbol) (list :fn word symbol))
                          ((boundp symbol) (list :var word symbol))
                          ((find-class symbol nil) (list :class word symbol))
                          (t ;; I don't know what this is
                           word))))
                     (t word))))))

;; (parse-docstring "asdf" nil)
;; (parse-docstring "asdf" '(asdf))
;; (parse-docstring "funcall parse-docstring" nil)
;; (parse-docstring "adsfa adf
;; asdfasd" nil)
;;       (parse-docstring "lala :lolo" nil)
;;       (parse-docstring "*communication-style*" nil)

(defun location-pathname (location)
  (pathname
   (cadr
    (or (find :file (cdr location)
              :key 'car)
        (find :buffer (cdr location)
              :key 'car)
        (find :buffer-and-file (cdr location)
              :key 'car)
        ))))

(defvar *package-source-locations* (make-hash-table)
  "A cache of packages source locations")

(defun package-source-location (package)
  (or (gethash package *package-source-locations*)
      (setf (gethash package *package-source-locations*)
            (swank/backend:find-source-location package))))

;; This function finds the packages defined from an ASDF, approximatly. And it is very slow.
(defun asdf-system-packages (system)
  (when (not (asdf:component-loaded-p system))
    (return-from asdf-system-packages nil))
  (let* ((asdf-system (if (or (symbolp system)
                              (stringp system))
                          (asdf:find-system system)
                          system))
         (system-source-directory (asdf:system-source-directory asdf-system)))
    (loop for package in (list-all-packages)
          for location := (package-source-location package)
          when (and (eql (car location) :location)
                    (uiop/pathname:subpathp
                     (location-pathname location)
                     system-source-directory))
            collect package)))

(provide :def-properties)
