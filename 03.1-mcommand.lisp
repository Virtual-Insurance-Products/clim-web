(in-package :clim-web)


;; Things here are for doing things with command in the web monad


;; so, I'm not sure if I want to specify a different default view for command table entries
;; do I want to present a command table entry to get the command form? Probably I guess
;; The view can be made to contain any already filled in values and to control whether to show those or not
;;



;; then there are a few things we need to be able to present using this view

(define-presentation-method default-view-for-object ((x list) (type command) (stream (eql :web-monad)))
  (make-instance 'form-view :name (list "_cmd" (first x))
                 :options (append climwi::other-options
                                  '(:child-field-options
                                    (:label-wrapper "label"
                                     :field-wrapper "field")))))



;; !!! This is the right approach:-
(define-presentation-method present ((object list)
                                     (type command)
                                     (stream (eql :web-monad))
                                     (view form-view)
                                     &key acceptably)
  acceptably
  ;; I can craft a typespec from the parameters list once I have got the command table entry
  ;; by putting the cdr of list in as the :given-values option, which is a special thing
  (let* ((command (find-command (first object)
                                (active-command-table))))
    ;; I might do things like showing a nice header from metadata in the CTE
    (awhen (with-web-monad

             - (mhtml
                 (awhen (field-option view :title)
                   (html (:h2
                          (:print (cond ((stringp it) it)
                                        (it (climwi::command-name command))))))))
             
             parameters <- (maccept nil ; not used
                                    (climwi::make-parameter-list
                                     (climwi::command-parameters command))
                                    :view (update-slots view
                                                        'given-values (cdr object)))

             submit <- (aif (field-option view :submit-label t)
                            (msubmit (list (field-name view) :submit)
                                     (if (stringp it)
                                         it
                                         (climwi::command-name command)))
                            (unit t))

             (cons (first object) parameters)
             :== result-command

             (unit (when submit
                     (cond ((presentation-typep result-command 'command)
                            result-command)
                           (t :error)))))

      (if (field-option view :execute t)
          (mexecute-command it)
          it))))





;; (default-view-for-object (find-command 'set-foo *test-commands-2*))

;; (much stuff form the previous mcommand bits is now remove from here - see elsewhere)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Another way of having commands invoked is to generate links
;; (essentially left click actions) for certain kinds of objects which
;; invoke the commands. This effectively works by giving a translator
;; for some type of object to a command. This doesn't have the full
;; generality of command translators. Yet.

;; specify a list of command names which should be made into links where appropriate...
#+nil(defun links-for-commands (commands &key (parameters '(:?))
                                      (command-table (active-command-table)))
  ;; then output href handlers for them...
  (mmapcar
   :web-monad
   (lambda (command)
     ;; (break)
     (event-handler (list :presentation
                          (first (context-parameter-type parameters command)))
                    :href
                    (lambda (object)
                      (url-for-top-level-command-processor
                       (cons (command-symbol-name command)
                             (mapcar (lambda (x)
                                       (if (eq x :?)
                                           object x))
                                     parameters))))))

   (find-commands-in-table (lambda (entry)
                             (and (member (command-symbol-name entry)
                                          commands)
                                  (member (command-return-type entry)
                                          '(web-monad command))
                                  (values-satisfy-parameters parameters entry)))
                           command-table)))



;; !!! This should be displayed much more nicely - not just in textual view
;; !!! Maybe I should make a textual base command table and a non textual one.
(define-command (help :name "Help" :command-table *base-command-table*
                      :return-type 'command-table)
    ()
  (active-command-table))

