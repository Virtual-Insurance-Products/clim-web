
(in-package :clim-web)

;; A view for presenting menus of links
;; This will probably need some useful slots
(defclass menu-view (url-view)
  (
   (only-parameterless-p :initarg :only-parameterless-p :initform nil :reader only-parameterless-p)
   ;; allow commands to be filtered?
   ;; (command-filter )
   (provided-arguments :initarg :provided-arguments :initform nil :reader provided-arguments)
   (only-explicit :initarg :only-explicit :initform nil :reader only-explicit)
   (without-ul :initarg :without-ul :initform nil :reader without-ul)))


(define-presentation-method present ((object command-table) (type command-table) (stream (eql :html)) (view menu-view)
                                     &key &allow-other-keys)
  ;; !!! Should I make tooltips or something to show the parameters?
  ;; What about elipsis to indicate when more parameters are required?
  (let ((climwi::*active-command-table* object))
    (climwi::apply-with-command-table-inheritance
     (lambda (object)
       (let ((commands (loop for entry in (sort (copy-list (climwi::commands object))
                                                #'string-lessp
                                                :key #'climwi:command-name)
                             ;; now we need to make a command from the entry...
                             for simple-command = (list (climwi::command-symbol-name entry))
                             for command = (add-incomplete-markers (append simple-command
                                                                           (provided-arguments view)))
                             ;; I can use this same view since it subclasses url-view. That should make things convenient I THINK
                             when (or (command-option entry :include-in-menu)
                                      (not (only-parameterless-p view))
                                      (and (equal command simple-command)
                                           (not (command-option entry :exclude-from-menu))
                                           (not (only-explicit view))))
                               collect command
                             )))
         (when commands
           (html (anaphors:awhen (command-table-name object)
                   (unless (without-ul view)
                     (html (:h1 anaphors:it))))
                 (flet ((items ()
                          (dolist (command commands)
                            (html-present command 'command :view view :element-type :li))))
                   (if (without-ul view)
                       (items)
                       (html (:ul (items)))))))))
     
     object)))



(defclass editable-value ()
  ((set-command :initarg :set-command :reader set-command)
   (additional-parameters :initform nil :accessor additional-parameters)))

(defmethod update-js ((view editable-value))
  (let* ((hole (gensym))
         (index 0)
         (basic-command (apply (set-command view)
                               (cons hole
                                     (additional-parameters view))))
         (cte (climwi:find-command basic-command climwi::*active-command-table*))
         (cmd `(js:array
                ,(command-name cte)
                ,@(with-presentation-type-decoded (name params)
                      (climwi::command-parameters-type cte)
                    (declare (ignore name))
                    (loop for x in (cdr basic-command)
                       for i from 0
                       for ptype in params
                       when (eq x hole)
                       do (setf index i)
                       collect (if (eq x hole)
                                   0 (present-to-string x ptype)))))))
    
    (js:ps* `(let ((x ,cmd))
               (setf (js:elt x ,(+ 1 index)) this.value)
               (ex (-j-s-o-n.stringify x))))))

(define-presentation-method present ((object editable-value) (type editable-value) (stream stream)
                                     (view textual-view) &key &allow-other-keys)
  (format stream "EDITABLE-VIEW"))

;; this handles primitive types
(define-presentation-method present ((object t) (type t) (stream (eql :html))
                                     (view editable-value)
                                     &key &allow-other-keys)
  ;; to make the command we evaluate the set-command passing it some value and the additional parameters slot contents
  (html (:input :value (:print (regex-replace-all #\Pound_Sign (present-to-string object type) ""))
                :onfocus "this.style.backgroundColor='#fdd'"
                :onblur (:print (update-js view)))))



;; I could represent html as a sexp but I don't think I'll bother with that. 
(define-presentation-type html ()
  :inherit-from 'string)

(define-presentation-method presentation-typep (object (type html))
  (stringp object))

(define-presentation-method present ((object string) (type html) (stream (eql :html)) (view textual-view)
                                     &key &allow-other-keys)
  (html (:noescape object)))

(define-presentation-method present ((object string) (type html) (stream (eql :html))
                                     (view editable-value)
                                     &key &allow-other-keys)
  ;; to make the command we evaluate the set-command passing it some value and the additional parameters slot contents
  (let ((id (js-next-window-id)))
    (html (:div :onclick (:print (ps:js* `(let ((x ($e ,id)))
                                            (x.focus)
                                            (setf x.style.display "block"
                                                  x.style.width (+ this.client-width "px")
                                                  x.style.height (+ this.client-height "px")
                                                  this.style.display "none"
                                                  ))))
                :style (css :margin 0)
                (html-present object type :view (make-instance 'textual-view)
                              :sensitive nil))
          (:textarea :onfocus "this.style.backgroundColor='#fdd'"
                     :id id
                     :style (css :width "50%" :height "25%" :display "none")
                     :onblur (:print (update-js view))
                     (:noescape object)))))


;; This is, at least, better
(define-presentation-method default-view-for-object ((x command-table) (type command-table) (stream keyword))
  (make-instance 'menu-view))

