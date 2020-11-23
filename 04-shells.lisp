
(in-package :clim-web)


;; Definition of 'shells' - ie pages which just invoke commands and display stuff
;; Very useful for quickly making bits of UI and *might* be good enough to build everything inside of them




;; It would be nice to try and get the basic CLIM stuff deployed and live fairly soon because it will give me an
;; easy way to provide additional functionality (like clearing wizard page cache) to people.

;; In order to make that viable I guess I will need to provide some starting point to find the objects one would
;; like to operate on...

;; Is there a general way I could do that? 

;; Also, is the CLIM code in a state that it will load cleanly? 

;; I will also make an rcall handler for when we want to call commands that way
;; it will be used for when you don't want commands to


;; Some examples of web pages which can be used as 'shells' - that is,
;; simple web pages for doing arbitrary things with presentable object
;; and commands.

;; Probably a very small number of these would be sufficient to do
;; almost everything.

;; I would like to have one which allows everything from the initial
;; login stage, so it might be necessary to have some kind of initial
;; object which presents as such a login page.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Initial command tables...


(defun set-active-command-table (table)
  (setf (session-datum :command-table) table))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The page below is a WIP

;; the link-view links to a page defined as follows:-
;; (more or less)

;; this shouldn't have many commands, but should have some

;; to get the command table dynamically bound for everything I will wrap the standard
;; dispatcher...

;; then I will make a dispatcher which executes a command...



;; the above basically works
;; it would be nice to have prefixes that just map straightforwardly to command executors
;; it would also be good for the shell page to have a way of jumping to a different page with a redirect
;; if something returns (say) a web monad.


(define-condition object-needs-redisplay (condition)
  ((object :initarg :object :reader redisplay-object)
   (view :initarg :view :reader redisplay-view)
   (sensitive :initarg :sensitive :initform nil :reader redisplay-sensitive)
   (type :initarg :type :initform nil :reader redisplay-type)))

(defun show-redrawable-object (object &optional (type (presentation-type-of object))
                               &key (view (default-view-for-object object type :html))
                                                       (sensitive nil))
  (html (:div :id (:print (redisplay-id
                           (make-instance 'object-needs-redisplay
                                          :object object :view view
                                          :type type)))
              (html-present object type
                            :view view :sensitive sensitive))))

;; (find-class 'object-needs-redisplay)
(defmethod redisplay-id ((x object-needs-redisplay))
  ;; The js-identifier will hash it to avoid bad characters in names
  (js-identifier
   (query-parameter-name
    (list (present-to-string (redisplay-object x)
                             (or (redisplay-type x)
                                 (presentation-type-of (redisplay-object x))))
          (present-to-string (redisplay-view x))))))

(defmethod redisplay ((object object-needs-redisplay) &optional (id (redisplay-id object)))
  (html (:rp :id id
             (html-present (redisplay-object object)
                           (or (redisplay-type object)
                               (presentation-type-of (redisplay-object object)))
                           :view (redisplay-view object)
                           :sensitive (redisplay-sensitive object)))
        ;; It's identity may have changed
        (unless (equal id (redisplay-id object))
          (html "$e('" id "').id='" (:print (redisplay-id object)) "';"))))


;; (redisplay-id (make-instance 'object-needs-redisplay :object (db 'db-broker 4186) :view :asdfasdf))


;; I think this only handles the AJAX case
;; If I want something which can execute command forms I will have to add a bit more to this.
(defun append-result-to-transcript-or-redirect (&key (id "output") (echo-command t)
                                                  ;; this must be explicitly requested
                                                  (honor-page-reloads nil)
                                                  mhttp-response)
  (lambda (command-or-string)
    (let ((id-override (when http-response::*request*
                         (setf echo-command nil)
                         (request-query-value "__ID" http-response::*request*)))
          (input-command (if (stringp command-or-string)
                             (accept-from-string 'command command-or-string)
                             command-or-string)))
      (when input-command
        (let* ((command (find-command (first input-command) (active-command-table)))
               (return-type (when command (clim-internals::command-return-type command))))

          (if (eq return-type 'web-application)
              (ajax-js `(setf window.location
                              ;; !!! IMPLEMENT application-url-from-command
                              ;; (or do a view and presentation)
                              ,(present-to-string input-command 'command :view (make-instance 'url-view))))

              (let ((objects-to-redisplay nil))
                (handler-bind
                    ((object-needs-redisplay (lambda (condition)
                                               ;; !!! Just grab it for now
                                               (push (cons condition (redisplay-id condition))
                                                     objects-to-redisplay))))
                  (let ((value (when command (execute-command input-command))))
                    (if (eq return-type 'command)
                        ;; if the command RETURNS a command it is assumed (here) that the returned command
                        ;; will be a web application. If this isn't the case then we have a problem.
                        (ajax-js `(setf window.location
                                        ,(present-to-string value
                                                            'command :view (make-instance 'url-view))))
                        (if id
                            (if (or (and honor-page-reloads
                                         (command-option command :maybe-requires-page-reload))
                                    (command-option command :requires-page-reload))
                                (ajax-js `(setf window.location
                                                window.location))
                                (with-web-monad
                                  big-console <- (mquery "__big_console")
                                  (ajax-render
                                   
                                   (melement (mprogn
                                               (if (and echo-command (stringp command-or-string))
                                                   (melement (aif (ignore-errors
                                                                   ;; This might well error due to not being able to accept
                                                                   ;; the command after it has executed
                                                                   (accept-from-string 'command-with-unevaluated-parameters
                                                                                       command-or-string))
                                                                  (if (presentation-typep it 'command-with-unevaluated-parameters)
                                                                      (mpresent it
                                                                                'command-with-unevaluated-parameters
                                                                                :acceptably t)
                                                                      (unit ""))
                                                                  (unit ""))
                                                     (:div :class "command"
                                                           "Command:  " (draw it)))
                                                   (unit nil))

                                               (if mhttp-response
                                                   (mhttp-response)
                                                   (unit nil))

                                               (cond
                                                 ((not command) (unit nil))
                                                 ((presentation-subtypep return-type 'web-monad)
                                                  value)
                                       
                                                 ((not value) (unit nil))
                                                 ;; This means that if a command says it returns currency or date it will look
                                                 ;; like that
                                                 (t (if (command-option command :redrawable-result)
                                                        (mhtml (show-redrawable-object value
                                                                                       return-type
                                                                                       :view (or (command-option command :redraw-view)
                                                                                                 (default-view-for-object value return-type :html))))
                                                        (mpresent value return-type)))))

                                     ;; Redraw anything according to signals raised which indicate this is required...
                                     (mapc (lambda (x)
                                             ;; I'll pass in the id explicitly
                                             (redisplay (car x) (cdr x)))
                                           objects-to-redisplay)

                                     (if big-console
                                         (html (:ap :id id
                                                    (draw it))
                                               "window.scrollTo(0,document.body.scrollHeight);"
                                               "$e('_console').scrollTop=1e12")
                                         (when value
                                           (if id-override
                                               (html (:ap :id id-override (draw it)))
                                               (html (:ap :id "welcome" ; !!! Not going to always work!
                                                          ;; !!! This should close on escape or clicking on a
                                                          ;; close button
                                                          (:div :class "dialog-container animated"
                                                                :id "_popup_output"
                                                                :style (css :animation-duration "0.3s")
                                                                :onclick "var x = this; x.classList.add('fadeOut'); window.setTimeout(function(){x.parentElement.removeChild(x);},500);"
                                                                (:div :class "dialog animated fadeIn"
                                                                      :onclick "event.stopPropagation();"
                                                                      :data-x 0 :data-y 0
                                                                      :onmousedown
                                                                      (:print
                                                                       (js:ps*
                                                                        `(when (= 1 event.buttons)
                                                                           (let* ((element this)
                                                                                  (xoff (- event.page-x (parse-int element.dataset.x)))
                                                                                  (yoff (- event.page-y (parse-int element.dataset.y)))
                                                                                  (mouse-move document.onmousemove)
                                                                                  (mouse-up document.onmouseup))
                                                                             (setf document.onmousemove
                                                                                   (lambda (event)
                                                                                     (let ((x (- event.page-x xoff))
                                                                                           (y (- event.page-y yoff)))
                                                                                       (setf element.style.transform
                                                                                             (+ "translate(" x "px, " y "px)")
                                                                                             element.dataset.x x element.dataset.y y)
                                                                                       (event.prevent-default)
                                                                                       (event.stop-propagation)))
                                                                                   document.onmouseup
                                                                                   (lambda (event)
                                                                                     (setf document.onmouseup mouse-up
                                                                                           document.onmousemove mouse-move)))))))
                                                                  
                                                                      :style (css (if (command-option command :max-width)
                                                                                      :max-width :width)
                                                                                  (or (command-option command :max-width)
                                                                                      (command-option command :width)
                                                                                      "66%")
                                                                                  :animation-duration "0.3s"
                                                                                  :max-height "75%"
                                                                                  :overflow "auto")
                                                                      (:div :onmousedown "event.stopPropagation()" 
                                                                            (draw it)))))
                                                     #+nil(:rm :id "_popup_output")))))))))
                        
                            (with-web-monad (unit nil)))))))))))))


(defun handle-command-without-transcript ()
  ;; by passing nil as the transcript id we suppress the output part
  (append-result-to-transcript-or-redirect :id nil))


;; this makes a command processor - a little state machine which awaits commands and then either
;; executes them or asks for more information which it needs in order to do so.
(defun command-processor (&key (name "__CMP")
                            ;; this function determines what to do with the result
                            ;; (for example, append it to the output)
                            command-handler)
  (labels ((handle-and-get-command (command)
             (with-web-monad
                    value <- (mquery (list name "c"))
                    ok <- (mquery (list "__event" (list name "OK") :onclick))
                    - (if (or value ok)
                          ;; !!! If _CMP_c isn't posted then we aren't really running this stage of the VM
                          (if *first-half*
                              (handle-command command)
                              (unit nil))
                          (unit nil))
                    (unit value)))
           
           ;; This dialog should look nice, because the command table entry contains enough
           ;; information to make it look so
           (complete-command (command)
             (with-web-monad
               (accept-from-string 'command command) :== command

               ;; so I can look for options...
               (when command
                 (find-command (first command) clim-internals::*active-command-table*))
               :== cte

               command <- 
               (draw-context (list name "dialog")
                             (melement (seq
                                         ;; - (mhtml (:script "$e('cmd').blur()"))
                                         id <- (mquery "__ID")
                                         - (if (and id (not (equal id "")))
                                               (mhidden-field "__ID")
                                               (unit nil))
                                         
                                         command
                                         <- (mpresent command
                                                      'command
                                                      :view (make-instance 'form-view
                                                                           :name (list name "complete")
                                                                           :show-given-parameters (not (command-option cte
                                                                                                                       :suppress-given-parameters))
                                                                           :options
                                                                           '(:child-field-options
                                                                             (:label-wrapper "label"
                                                                              :field-wrapper "field"
                                                                              ;; !!! The following doesn't (always?) seem to work
                                                                              :autofocus t)
                                                                             :submit-label nil
                                                                             :execute nil
                                                                             :title t)))
                                         
                                         - (melement (mprogn
                                                       (mbutton (list name "Cancel")
                                                                (or (command-option cte :cancel-button-label)
                                                                    "Cancel"))
                                                       (mbutton (list name "OK")
                                                                (or (command-option cte :ok-button-label)
                                                                    "OK")
                                                                :class "cbutton cbutton--effect-jagoda" 
                                                                :style "position: relative"))
                                             (:div :class "buttons"
                                                   (draw it)))

                                         - (mhtml (:input :class "confirm"
                                                          :type "hidden"))
                                         
                                         - (event-handler (list name "OK")
                                                          :onclick
                                                          `(progn
                                                             (let ((get-confirm (aref (event.target.parent-element.parent-element.get-elements-by-class-name "confirm") 0)))
                                                               (when (and get-confirm.value
                                                                          (not (confirm get-confirm.value)))
                                                                 (return)))

                                                             (setf event.target.disabled t)
                                                             (event.target.class-list.add "cbutton--click")))


                                         ;; !!! Focus the first displayed input field
                                         ;; - (mhtml (:script ))
                               
                                         (unit command))
                               (:div :class "dialog-container"
                                     (:div :class "dialog"
                                           (draw it))))
                             :redraw t)


               cancel <- (ajax-event (list name "Cancel") :onclick)
               ok <- (ajax-event (list name "OK") :onclick)

               (unit (cond (cancel nil)
                           (ok (present-to-string command 'command
                                                  :acceptably t))
                           (t :error)))))
           
           
           (handle-command (command)
             (with-web-monad
               (if (and command command-handler)
                   (funcall command-handler command)
                   (unit nil)))))
    
    (with-web-monad
      - (draw-context (list name "dialog")
                      (unit nil) :redraw t)
      
      (transpile (:name name)
        (let ((command nil))
          (loop while t
             do
               (setf command (web (handle-and-get-command command)))
               (when (and command
                          (clim-internals::incomplete-command-p (accept-from-string 'command command)))
                 (loop for c = (web (complete-command command))
                    while (eq c :error)
                    finally (setf command c)))))))))



;; I will need to pass additional options and whatnot (probably)
(defun command-input-field (name &key (command-processor "__CMP"))
  (with-web-monad
    command <- (maccept name '(or command-with-unevaluated-parameters (eql nil))
                        :view (make-instance 'minput-acceptor
                                             :name name
                                             :options `(:validate t ; ,(ajax-event name :onblur)
                                                                  :spellcheck "false"
                                                                  :autocomplete "off"
                                                                  :autocorrect "off"
                                                                  :dynamic-update-pattern "\\s$"
                                                                  )))

    ;; (break)

    ;; now I can display argument hints if I want...
    ;; (although this makes me wonder why not just show a form and make space do helpful things)
    ;; This really doesn't look very good yet, but we can improve it and also generally improve how this
    ;; works. It useful though.
    ;; also, this does not recurse yet. How can I fix that?
    - (draw-context (list name "hints")
                    (mhtml
                      (when (and command (clim-internals::incomplete-command-p command))
                        (let ((parameters (cdr command)))
                          (loop for p in parameters
                               when (and (listp p)
                                         (eq (first p)
                                             clim-internals::*incomplete-command-marker*))
                             do (let ((actual-type (third (second p))))
                                  (with-presentation-type-decoded
                                      (type-name)
                                      actual-type
                                    ;; !!! There should be a way of presenting the typespec nicely
                                    ;; this needs some work
                                    (let ((name (typespec-option actual-type :name)))
                                      (html (:div name " (" type-name ")")))))))))
                    
                    
                    
                    :redraw t)

    ;; If this was more clever it would be able to complet partial commands too
    ;; I should get to that...
    - (event-handler name
                     :onkeyup
                     `(if (= event.key-code 32)
                          (ev "__event__cmd__ONBLUR" 1)))

    ;; !!! In the event of typing a partial command pressing return we need to blur the input field
    (event-handler name
                   :onkeydown
                   `(progn
                      (if (= event.key-code 13)
                          (progn
                            (event.prevent-default)
                            (let ((v this.value))
                              (setf this.value "")
                              ;; (this.blur)
                              (ev ,(format nil "~A__c" command-processor) v))))))))



;; I can wrap the above cmd stuff up in a subclass of minput-acceptor which will do that stuff
;; I can then declare that as the default view for accepting commands

;; it also certainly might be possible to use the web transpiler to get some very nice
;; intellisense style UI stuff going on here

