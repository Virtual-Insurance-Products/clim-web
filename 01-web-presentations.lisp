

;; This relies on other bits of ABEL...

(in-package :clim-web)

;; !!! Where did the name come in to it?

(defun mpresent (object &optional (type (presentation-type-of object))
                 &key
                   ;; !!! Should this follow the proper protocol or just do the following
                   (view (default-view-for-object object type :web-monad))
                   acceptably
                   (sensitive t))
  (present object type :view view :acceptably acceptably :sensitive sensitive
           :stream :web-monad))

(defun html-present (object &optional (type (presentation-type-of object))
                              &key (view (default-view-for-object object type :html))
                                if-of-type
                                acceptably
                                (sensitive t)
                                (element-type nil))
  (unless (and if-of-type (not (presentation-typep object type)))
    (if element-type
        (with-output-as-presentation (:html object type :element-type element-type :event-handlers
                                            (if sensitive *monadic-event-handlers* nil))
          (present object type :view view :acceptably acceptably :sensitive nil :stream :html))
        (present object type :view view :acceptably acceptably :sensitive sensitive :stream :html))))


(defun present-command-if-able (command &key view (element-type :span))
  (when (presentation-typep command 'command)
    (html-present
     (add-incomplete-markers command)
     `((command) :view ,view)
     :element-type element-type)))


;; A general selection tracking mechansim...
(defmethod selected-p ((x t)) nil)

(defmethod climwi::%do-output-as-presentation ((stream (eql :web-monad)) object type content
                                       &key (class (if (selected-p object)
                                                       "selected" ; "selected presentation"
                                                       nil ; "presentation"
                                                       )) (element-type nil) (event-handlers))
  
  (declare (ignore event-handlers))
  (melement (funcall content)
    (climwi::%do-output-as-presentation
     :html object type
     (lambda ()
       (draw it))
     :class class
     :element-type element-type
     :event-handlers web-monad::+event-handlers)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod climwi::type-arg-position ((name (eql 'object-html-element))) 1))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod climwi::type-arg-position ((name (eql 'object-html-class))) 1))

(defun object-html-element (object type view)
  (climwi::funcall-presentation-generic-function object-html-element object type view))
(defun object-html-class (object type view)
  (climwi::funcall-presentation-generic-function object-html-class object type view))

(define-presentation-method object-html-element ((object t) (type t) (view t))
  type ; ignored
  :div)
(define-presentation-method object-html-class ((object t) (type t) (view t))
  type ; ignored
  (if (selected-p object) "selected" nil))

;; (object-html-element 12 'integer :asd)


(defmethod climwi::%do-output-as-presentation ((stream (eql :html)) object type content
                                                       &key
                                                         (element-type nil)
                                                         (class (if (selected-p object)
                                                                    "selected" nil
                                                                    ;; "selected presentation" "presentation"
                                                                    ))
                                                         (event-handlers *monadic-event-handlers*))
  (flet ((event-matches (object type)
           (lambda (event)
             (let ((event-object (event-handler-object event)))
               (and (consp event-object)
                    (eq (car event-object) :presentation)
                    ;; !!! FIXME - we need AND here
                    ;; well, this is done, but needs testing
                    (and (presentation-subtypep type
                                                ;; !!! Put the already
                                                ;; calculated type-for-subtype-tests
                                                ;; as the third item to optimize this
                                                ;; !!! Could be done lazily
                                                (or (third event-object)
                                                    (type-for-subtype-tests
                                                     (second event-object))))
                         ;; !!! Cache the function for doing checking???
                         (presentation-typep object (second event-object))))))))
    ;; the id is supposed to be something which accept will understand when specialised on that type
    ;; it might be a textual description of that object or a URI or a readable string or number or something
    (let ((events (events-for-object (event-matches object type)
                                     event-handlers)))
      ;; (break)
      ;; if there was an href event then override the element-type
      (<> (or element-type
              (and (find :href events :key 'event-handler-event)
                   ;; (error "All: ~A" (remove-if-not (@ eq ?1 :href) events :key 'event-handler-event))
                   :a)
              :div)
          :class class
          (output-event-list
           (reduce #'append
                   (cons events
                         (mapcar (lambda (object-and-type)
                                   (destructuring-bind (object type)
                                       object-and-type
                                     (mapcar (lambda (e)
                                               (setf (event-handler-context e)
                                                     (list object type))
                                               e)
                                             (events-for-object (event-matches object type)
                                                                event-handlers))))
                                 (climwi::all-possible-translations object type
                                                                            (active-command-table)))))

           (list object type))
          content))))


(defun web-present (stream object type view acceptably sensitive)
  (flet ((draw ()
           (climwi::funcall-presentation-generic-function
            present object type stream view
            :acceptably acceptably
            ;; :for-context-type for-context-type
            )))
    (if sensitive
        (climwi::%do-output-as-presentation stream object type #'draw
                                                    :element-type (object-html-element object type view)
                                                    :class (object-html-class object type view)
                                                    :event-handlers *monadic-event-handlers*)
        (draw))))

;; !!! Did I need to put a name in here
;; ALSO: the reason I started redoing this was because of how to do the whole accept mechanism
;; SO I need to get back to that
;; general accept gubbins will probably be 02
;; web monad acceptor stuff will probably be 03


;; the implementation of stream present then does this
;; (this probably doesn't need to be specialised for web-monad here)
(defmethod stream-present ((stream (eql :web-monad)) object type
                           &key view
                             acceptably
                             sensitive)
  (web-present stream object type view acceptably sensitive))

(defmethod stream-present ((stream (eql :html)) object type
                           &key view
                             acceptably
                             sensitive)
  (web-present stream object type view acceptably sensitive))

;; (present 12 'integer :stream :html)

;; #'climwi::%present

;; default web-monad implementation using just the html one...
(define-presentation-method present ((object t) (type t) (stream (eql :web-monad))
                                     view &key acceptably)
  (mhtml (climwi::funcall-presentation-generic-function present
                                                                object type :html view :acceptably acceptably)))

;; for a default present method for html stream we can just present the object to a string
(define-presentation-method present ((object t) (type t) (stream (eql :html))
                                     (view textual-view) &key acceptably)
  (html (:print (present-to-string object type :view view :acceptably acceptably))))


(defun html-currency-symbol (code)
  (cond ((eq code :gbp)
         "&#163;")
        ((eq code :eur)
         "&#8364;")
        ((eq code :usd)
         "$")))

;; This should output the appropriate currency symbol nicely
;; in fact, this could be done away with now, because I have a general implementation of this on streams
;; the only caveat is that we have issues with unicode characters
;; I could deal with that somewhere...
(define-presentation-method present ((object number) (type currency) (stream (eql :html)) (view textual-view)
                                     &key acceptably)
  acceptably type ; muffle warning

  (html
    (:noescape (:print (or (html-currency-symbol climwi::code) "")))
    
    (:format "~/dollar/" object)
    (unless (member climwi::code '(:gbp :usd :eur))
      (html (:print (symbol-name climwi::code))))))

;; These all work nicely now
;; (present 23 '(currency :usd) :stream :html :view (make-instance 'textual-view))
;; (present 23 '(currency :gbp) :stream :html)
;; (present 23 'currency :stream :html :view (make-instance 'textual-view))
;; (present 23 'currency :stream :html)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Simple means to output lists of things wrapped in some element...


(define-presentation-method object-html-element ((object t) (type t) (view keyword))
  type ; ignore
  view)

(define-presentation-method present ((x list) (type t) (stream (eql :html)) (view keyword)
                                     &key acceptably)
  acceptably
  (dolist (a x)
    (let ((type (or (typespec-option type :item-type)
                    (presentation-type-of a))))
      (if (member view '(:ul :ol))
          (html (:li (html-present a type)))
          (html-present a type)))))


(define-presentation-method present ((x list) (type parameter-list) (stream (eql :html)) (view keyword)
                                     &key acceptably)
  acceptably ; ignore
  (loop for a in x
     for item-type in climwi::??parameters
     do (if (member view '(:ul :ol))
            (html (:li (html-present a item-type)))
            (html-present a item-type))))



;; This probably shouldn't be here since it's actually quite general
;; A function can be interpreted as a view as follows
;; this gives convenient shorthand notations for presenting an object and specifying
;; how to get something drawable from that object
(define-presentation-method present ((x t) (type t) (stream t) (view function)
                                     &key acceptably)
  type
  (multiple-value-bind (object type)
      (funcall view x)
    (present object (or type
                        (presentation-type-of object))
             :stream stream
             :sensitive nil
             :acceptably acceptably)))
