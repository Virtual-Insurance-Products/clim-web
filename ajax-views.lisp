
(in-package :clim-web)

;; This class is intended to be used as a mixin
(defclass ajax-fade-in ()
  ((container :initarg :container :reader ajax-fade-in-container)
   (spinner :initarg :spinner :reader spinner :initform nil)
   (latch :reader latch :initform nil :initarg :latch)))


(define-presentation-method present :around ((x t) (type t) (stream (eql :web-monad))
                                             (view ajax-fade-in)
                                             &key &allow-other-keys)
  (with-web-monad
    - (mhttp-response)
    value <-
    (melement (with-web-monad
                - (if (latch view)
                      (mhtml
                        (:script "$e('" (:print (latch view)) "').value='" (:print (present-to-string x type)) "'"))
                      (unit nil))
                (call-next-method))
      (:ap :id (:print (ajax-fade-in-container view))
           (draw it))
      (when (spinner view)
        (html (:rm :id (:print (spinner view)))))
      ;; !!! I'm going to need a spinner whilst this is loading. How long does it take to show the gap wizard?
      ;; This allows us to set it transparent initially and then do a fade in
      (:script (:format "window.setTimeout(function() {$('~A').style.opacity=1;}, 50);"
                        (ajax-fade-in-container view))))
    
    - (end-request)
    (unit value)))
