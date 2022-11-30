
(in-package :clim-web)

;; TBH this is a bit confused and I'm not quite sure what I should do
;; the general definitions of the approach was codified before I had general presentation methods
;; so it might be worth reviewing the definition of stream accept below

;; I want to be able to specialise on the view and (maybe) the type
;; since the view comes from the type I kind of have that
;; I want to be able to override the view though, as well as having a default
;; Hmmm (not thought through properly)


;; First we just define what stream accept does for web monad, which is very very simple



(defun maccept (name type &key
                            (view (default-view-for-accepting type :web-monad :name name))
                            (default (typespec-option type :default))
                            (allow-command-invocation t)
                                        ; do I need parameters to pass the default view? probably
                            )
  (accept type :stream :web-monad
          :view view :default default
          :allow-command-invocation allow-command-invocation))


;; This should have various other parameters
;; do we need a default and/or a history here? caption? hint? 
(defclass minput-acceptor ()
  ((name :initarg :name :accessor field-name)
   (mfunction :initarg :mfunction :initform #'minput :reader mfunction)
   (options :initarg :options :initform nil :accessor field-options)))


(defmethod field-option ((x minput-acceptor) name &optional default)
  (aif (member name (field-options x))
       (second it)
       default))


;; unless otherwise specialised we use the above view to accept stuff
;; BUT we can specialise for particular types
(define-presentation-method default-view-for-accepting ((type thing) (stream (eql :web-monad))
                                                        &key name)
  (make-instance 'minput-acceptor :name name
                 :options climwi::other-options))


(define-presentation-method default-view-for-accepting ((type and) (stream (eql :web-monad)) &key name)
  (let ((view (default-view-for-accepting (with-presentation-type-decoded (n p o)
                                              (first climwi::??c)
                                            `((,n ,@p) ,@o ,@climwi::other-options))
                  stream :name name)))
    (when (typep view 'minput-acceptor)
      ;; look for any other type information which can be used to inform the minput acceptor...
      (dolist (type (cdr climwi::??c))
        (with-presentation-type-decoded (name parameters)
            type
          (cond ((eq name 'scan)
                 (setf (field-options view)
                       (append (field-options view)
                               (list :pattern (first parameters)))))))))
    
    view))


(define-presentation-method default-view-for-accepting ((type date) (stream (eql :web-monad))
                                                        &key name)
  (make-instance 'minput-acceptor :name name
                 :options (append climwi::other-options
                                  '(:type "date"))))

(define-presentation-method default-view-for-accepting ((type one-of) (stream (eql :web-monad))
                                                        &key name)
  (make-instance 'minput-acceptor :name name
                 :options climwi::other-options
                 :mfunction (lambda (name &rest x)
                              (declare (ignore x))
                              (mselect name
                                       (cons '("-" :error)
                                             (loop for item in climwi::items
                                                collect (list item item)))))))

;; This is very similar to the above
(define-presentation-method default-view-for-accepting ((type member) (stream (eql :web-monad))
                                                        &key name)
  (make-instance 'minput-acceptor :name name
                 :options climwi::other-options
                 :mfunction (lambda (name &rest x)
                              (declare (ignore x))
                              (mselect name
                                       (cons '("-" :error)
                                             (loop for item in climwi::objects
                                                collect (list (present-to-string item (list 'eql item))
                                                              item)))))))


(defmethod wrap-input-field ((view minput-acceptor) (m web-monad))
  (melement m
    (labels ((opt (name)
               (second (member name (field-options view))))
             (wrap (option content)
               (aif (opt option)
                    (html (:div :class it (funcall content)))
                    (funcall content))))
        
      (wrap :field-wrapper
            (dhtml
              (awhen (or (opt :label)
                         (awhen (opt :name)
                           (string-capitalize
                            (regex-replace-all "-"
                                               (symbol-name it) " "))))
                (wrap :label-wrapper
                      (dhtml it)))
              (wrap :input-wrapper
                    (dhtml (draw it))))))))

(define-presentation-method accept ((type t) (stream (eql :web-monad))
                                    (view minput-acceptor) &key default &allow-other-keys)
  (apply (mfunction view)
         `(,(field-name view)
            :default ,(when default
                        (present-to-string default type :view (make-instance 'html-value-view)))
            :formatter
            ,(lambda (x)
               (let ((x (regex-replace "^\\s*" x "")))
                 ;; !!! Need to catch this error and convert it to an erroneous reply
                 (let ((r (ignore-errors (accept-from-string type x :error-if-not-eof t))))
                   (if (presentation-typep r type)
                       ;; If whatever was typed leads to nil then we won't present nil but just leave it
                       (list (if r
                                 (present-to-string r type :view (make-instance 'html-value-view))
                                 x)
                             r)
                       (list x :error)))))
            ;; !!! I'm not sure whether we want to do something like this
            ;; I don't think (probably) that having the type options be transferred like this
            ;; is what we want
            ;; Probably the view needs another parameter???
            ;; I don't know what's the best way to handle these options. We're conflating things
            ,@ (loop for (name value) on (field-options view) by #'cddr
                  for minput-option = (member name '(:value :class :style :size :type
                                                     :min :max :readonly :placeholder :spellcheck
                                                     :autocomplete
                                                     :no-dynamic-update :validate
                                                     :dynamic-update-pattern
                                                     :pattern :autofocus
                                                     :error-style))
                  when minput-option collect name
                  when minput-option collect value))))

;; in the above we only have access to the options of the thing type except via other-options

;; this only needs to understand how to deal with typespecs
;; object (eql nil) means that we are ACCEPTING the thing (it's the only way this can happen)
(define-presentation-method accept :around ((type t) (stream (eql :web-monad))
                                            (view minput-acceptor) &key &allow-other-keys)
  (wrap-input-field view
                    (call-next-method)))

(define-presentation-method accept ((type currency) (stream (eql :web-monad)) (view minput-acceptor)
                                    &key default &allow-other-keys)
  default
  ;; (break)
  (melement (call-next-method)
    (html (:noescape (:print (or (html-currency-symbol climwi::code) "")))
          " ")
    (draw it)))



;; does it make sense to subclass here? Maybe
(defclass form-view (minput-acceptor)
  ;; !!! Maybe just use the options for this instead of another slot???
  ((given-values :initarg :given-values :initform nil :reader given-values)
   ;; (show-command-name :initform t)
   (show-given-parameters :initform t :initarg :show-given-parameters :reader show-given-parameters)))




(define-presentation-method default-view-for-accepting ((type parameter-list) (stream (eql :web-monad))
                                                        &key name)
  (make-instance 'form-view :name name
                 :options climwi::other-options))



(define-presentation-method accept ((type parameter-list) (stream (eql :web-monad)) (view form-view)
                                    &key &allow-other-keys)
  (with-web-monad
    ;; it would be useful to display a caption here...
    value <- (mmapcar-i :web-monad
                        (lambda (i x given)
                          (if (and given
                                   (not (and (listp given)
                                             (eq (first given)
                                                 climwi::*incomplete-command-marker*)))
                                       
                                   (presentation-typep given x))
                              (mprogn
                                (if (show-given-parameters view)
                                    (with-presentation-type-decoded (name params options)
                                        x
                                      (declare (ignore name params))
                                      (wrap-input-field (make-instance 'minput-acceptor
                                                                       :options (append (second (member :child-field-options
                                                                                                        (field-options view)))
                                                                                        options))
                                                        (mhtml (:span :class "fixed-value"
                                                                      ;; This isn't really perfect
                                                                      :style "-webkit-appearance: textfield;
    background-color: white;
    -webkit-rtl-ordering: logical;
    padding: 1px;
    width:200px;
    border-width: 2px;
    border-style: inset;
    border-color: initial;
    border-image: initial;
    color: #737373;" 
                                                                      (:print (present-to-string given x))))))
                                    (unit nil))
                                (unit given))
                              (maccept (list (field-name view) i)
                                       ;; let's add the options from the form view to each item...
                                       (with-presentation-type-decoded (name parameters options)
                                           x
                                         `((,name ,@parameters)
                                           ,@options
                                           ;; so we can have options applied to the parameter list
                                           ;; AND options applied to each nested field...
                                           ,@ (second (member :child-field-options
                                                              (field-options view)))))
                                       ;; The incomplete marker may have been annotated with a default
                                       ;; !!! This needs to be merged with the default passed in to this
                                       :default (typespec-option (second given) :default))))
                        climwi::??parameters
                        (or (given-values view)
                            (mapcar (constantly nil) climwi::??parameters)))
    (if (presentation-typep value type)
        (unit value)
        ;; if it's just a list of nils then nothing was entered. Otherwise error
        (if (find t value :key (@ and ?1 t))
            (unit :error)
            (unit nil)))))

(defclass searchable-drop-down-view (minput-acceptor)
  ((possible-values :initarg :possible-values :initform nil :reader possible-values)))


(define-presentation-method accept ((type t) (stream (eql :web-monad))
                                    (view searchable-drop-down-view) &key &allow-other-keys)

  (with-web-monad
    value <- (melement (apply #'msearchable-drop-down
                              `(,(field-name view)
                                ,(if (functionp (possible-values view))
                                     (funcall (possible-values view))
                                     (possible-values view))
                                ,@ (loop for (name value) on (field-options view) by #'cddr
                                         for minput-option = (member name '(:height :width :item-height
                                                                            :font-size :font :validate
                                                                            :class :default :field-value))
                                         when minput-option collect name
                                           when minput-option collect value)))
               (:div :style (css :display "inline-block")
                     (draw it)))
    (unit (when (and value (stringp value)
                     (not (equal value "")))
            (accept-from-string type value)))))



;; more user oriented forms. Not necessarily as general. More for making custom things rather than auto generated UI forms...

(defclass user-form ()
  ((label-wrapper :initarg :label-wrapper :reader label-wrapper)
   (field-wrapper :initarg :field-wrapper :reader field-wrapper)
   (qname :initarg :qname :reader form-qname))
  (:default-initargs :field-wrapper "cfield" :label-wrapper "field-label"
   :qname "form"))


(defmethod draw-label ((x user-form) label)
  (html (:span :class (:print (label-wrapper x)) label " ")))

(defmethod draw-field ((x user-form) thunk)
  (html (funcall thunk)))

(defmethod wrap ((form user-form) label control &key before-label after-label after-control)
  ;; I should probably implement stuff like the following
  (declare (ignore before-label after-label after-control))
  (melement control
    (:div :class (:print (field-wrapper form))
          (draw-label form label)
          (draw-field form (lambda () (draw it))))))

;; This would have to be changed
(defmethod qname ((x user-form) name)
  (query-parameter-name (list (form-qname x) name)))

