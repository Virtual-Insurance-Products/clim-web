
(in-package :clim-web)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Application definition

;; These kinds of commands will correspond to more normal web pages...

(define-presentation-type web-application ()
  ;; the inherit-from causes an error
  ;; :inherit-from 'web-monad
  )

(define-presentation-method presentation-typep (object (type web-application))
  ;; this is the only check we can do
  (web-monad-p object))

(define-presentation-type parenscript ())

(define-presentation-method presentation-typep (object (type parenscript))
  (and (ignore-errors
         (ps:ps* object))
       t))

;; now to output parenscript code...
(define-presentation-method present ((object t) (type parenscript) (stream stream) (view textual-view)
                                     &key acceptably)
  acceptably
  (write-sequence (ps:ps* object) stream))

;; what about displaying it to html or web monad? I can probably just put in script tags I guess
;; should it be possible to output it as a link target too? Maybe.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These things support command<->url transformations

;; NOTE: there will be other ways of making links including turning presented objects into links
;; automatically. What sort of presented objects?

;; NOTE: we could pull out the triggers which start/run the vm


(defclass url-view ()
  ((url-prefix :initform "/app/" :initarg :url-prefix :reader url-prefix)
   (link-target :initform nil :initarg :target :reader link-target)
   (link-text :initform nil :initarg :link-text :reader link-text)
   (link-class :initform nil :initarg :link-class :reader link-class)
   (link-style :initform nil :initarg :link-style :reader link-style)
   (link-icon :initform nil :initarg :link-icon :reader link-icon)
   ;; This can be used to turn arbitrary objects into links by making them into commands
   (link-command :initform nil :initarg :link-command :reader link-command)
   (link-title :initform nil :initarg :link-title :reader link-title)
   (absolute-url-prefix :initarg :absolute-url-prefix :reader absolute-url-prefix :initform nil)
   (output-target :initform nil :initarg :output-target :reader link-output-target)
   (replace-output :initform t :initarg :replace-output :reader link-replace-output)
   (require-confirmation :initform nil :initarg :require-confirmation :reader link-require-confirmation)))

;; !!! I need to somehow hide this from general view (perhaps)
;; Should this be able to accept commands which return web-application?
;; Should I constrain the type to make it only appears for commands which are
;; valid command returning commands? Would that be slow?
(define-command (/app/exec :command-table *base-command-table*
                           :name "Execute Command"
                           :return-type 'web-application
                           :options '(:no-context-menu t))
    
    ((command string))
  (show-application (with-web-monad
                      (accept-from-string '(command :return-type command)
                                          command)
                      :== command-sexp

                      (if command-sexp
                          (let ((app (execute-command command-sexp)))
                            (mprogn
                              (mhttp-redirect
                               (present-to-string app 'command :view (make-instance 'url-view)))
                              ;; why would this be required?
                              (end-request)))
                            
                          (mhtml (:h1 "Command not found: " command))))
                    
                    :draw-header nil))





(define-presentation-method present ((command list) (type command) (stream stream) (view url-view)
                                     &key acceptably)
  acceptably
  ;; the url-prefix has to be in the command name. It isn't really used for much here
  ;; (format stream url-prefix)
  (let* ((cte (or (find-command (first command) (active-command-table))
                  (error "Command not found: ~A" (first command))))
         (return-type (climwi::command-return-type cte)))
    (awhen (absolute-url-prefix view)
      (write-sequence it stream))
    (cond ((eq return-type 'web-application)
           (write-sequence
            (regex-replace "^//"
                           (apply #'href-fn
                                  (cons (first command)
                                        (with-presentation-type-decoded
                                            (name parameters)
                                            (climwi::command-parameters-type cte)
                                          (declare (ignore name))
                                          (loop for p in parameters
                                             for v in (cdr command)
                                             collect (typespec-option p :name)
                                             ;; !!! Should we 
                                             collect (present-to-string v p)))))
                           "/")
            stream))
          ((eq return-type 'command)
           (write-sequence (href-fn 'app/exec
                                    :command (present-to-string command 'command))
                           stream))

          (t (error
              "Cannot present '~A' as a link because it does not return a command or web-application"
              (present-to-string command 'command))))))


;; this is what I need to do the url encoding
;; (href-fn '/app/something :a 1 :b 2)
;; (href-fn '/app/listener)
;; (href 'something :a 1 :b 2)


;; since just presenting a URL on an HTML stream doesn't really make sense EXCEPT as a link, let's do that...
(define-presentation-method present ((command list) (type command) (stream (eql :html)) (view url-view)
                                                    &key acceptably)
  acceptably
  ;; if the command is incomplete then we should make a link which pops up the usual form asking for the
  ;; details.
  (let ((use-cp (or (climwi::incomplete-command-p command)
                    (not (presentation-typep command '(command :return-type
                                                       (or web-application command)))))))
    (html (<> :a :href (if use-cp
                           "#"
                           (present-to-string command 'command :view view))
                 :title (link-title view)
                 :onclick (when use-cp
                            (ps:ps*
                             `(progn
                                ,@ (when (link-require-confirmation view)
                                     '((unless (confirm "Are you sure?")
                                         (return nil))))

                                ,(command-option (find-command command climwi::*active-command-table*)
                                                 :parenscript)
                                (event.stop-propagation)
                                (event.prevent-default)
                                ,(if (link-output-target view)
                                     `(progn
                                        ,@ (when (link-replace-output view)
                                             `((setf (slot-value ($e ,(link-output-target view))
                                                                 'inner-h-t-m-l) "")))
                                        (ex2 ,(present-to-string command 'command) ,(link-output-target view)))
                                     `(ex ,(present-to-string command 'command))))))
                 :style (link-style view)
                 :class (link-class view)
              
                 (dhtml (awhen (link-icon view)
                          (html (:i :class (:format "fa fa-~A" it))
                                (:noescape "&nbsp;"))))
              
                 (dhtml (aif (or (typespec-option type :link-text)
                                 (link-text view))
                             (html it)
                             (present command 'command :view (make-instance 'textual-view)
                                                       :stream :html :sensitive nil)))))))






;; This reads a command from a URL...
;; (essentially it accepts a command which was presented using url-view - cool eh?)
(define-presentation-method accept ((type command) (stream (eql :web-monad))
                                    (view url-view)
                                    &key &allow-other-keys)
  (with-web-monad
    ;; first get the command name which is in the url
    ;; we have to use the url-prefix from the options OR the view
    request <- (mrequest)

    ;; I think this means we will have to tend to not put prefixes in app commands
    (string-upcase
     (regex-replace (s "^.*?(~A.*)"
                       (or (typespec-option type :url-prefix)
                           (url-prefix view)))
                    (regex-replace "\\?.*" (s "~A" (request-uri request)) "")
                    "\\1"))
    :== command-name

    (first (find-commands-in-table (lambda (command)
                                     (equal (symbol-name
                                             (command-symbol-name command))
                                            command-name))
                                   (active-command-table)))
    :== cte
    
    parameters <- (if cte
                      (with-presentation-type-decoded (name parameters)
                          (climwi::command-parameters-type cte)
                        (declare (ignore name))
                        (mmapcar :web-monad
                                 (lambda (p)
                                   (with-web-monad
                                     value <- (mquery (string-downcase (symbol-name (typespec-option p :name))))
                                     (unit (awhen (or value
                                                      (typespec-option p :default))
                                             (accept-from-string p it)))))
                             
                                 parameters))
                      (unit nil))
    
    (unit (if cte
              (cons (command-symbol-name cte) parameters)
              nil))))


(define-presentation-method present ((x string) (type string) (stream stream) (view url-view)
                                     &key acceptably)
  acceptably
  (write-sequence x stream))

(define-presentation-method present (x (type t) (stream (eql :html)) (view url-view)
                                       &key acceptably)
  acceptably
  (aif (link-command view)
       (let ((command (cond ((functionp it)
                             (funcall it x))
                            ((listp it) (append it (list x)))
                            (t (cons it (list x))))))
         (present (add-incomplete-markers command)
                  `((command) ,@(with-presentation-type-decoded (n p o)
                                                                type
                                                                (declare (ignore n p))
                                                                o))
                  :stream stream :view view :sensitive nil))
       (<> :a :href (present-to-string x type :view view)
           :target (typespec-option type :target)
           (dhtml (aif (typespec-option type :label)
                       (html it)
                       (html-present x type :view (make-instance 'textual-view) :sensitive nil))))))



;; now, for arbitrary objects we can make links as follows...
(define-presentation-method present ((x t) (type t) (stream t) (view url-view)
                                     &key acceptably)
  (if (link-command view)
      (present (if (functionp (link-command view))
                   (funcall (link-command view) x)
                   (append (link-command view) (list x)))
               'command
               :stream stream :view view :acceptably acceptably :sensitive nil)
      (error "Cannot generate a link for ~A with view ~A"
             (present-to-string x type)
             view)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Applications

;; !!! I could add things in here to run client side code without recourse to the server
;; that would be useful for certain things...
(defun event-gesture-commands (&optional (command-table (active-command-table)))
  (let ((commands (find-commands-in-table (lambda (x)
                                            (command-option x :event))
                                          command-table)))
    (mmapcar :web-monad
             (lambda (command)
               (let ((first-param (with-presentation-type-decoded (name params)
                                              (climwi::command-parameters-type command)
                                            (declare (ignore name))
                                            (first params))))
                 ;; execute some client side parenscript AND do an ajax call
                 (awhen (command-option command :parenscript)
                   (event-handler (list :presentation first-param)
                                  (command-option command :event)
                                  it))
                 (event-handler (list :presentation first-param)
                                (command-option command :event)
                                (lambda (object-and-type)
                                  (if (presentation-subtypep (command-return-type command)
                                                             'parenscript)
                                      (execute-command (list (command-symbol-name command)
                                                             (first object-and-type)))
                                      `(progn
                                         ,@(when (command-option command :exclusive-event)
                                                 '((event.prevent-default)
                                                   (event.stop-propagation)))
                                   
                                         (ex (+ ,(command-name command)
                                                " "
                                                ,(present-to-string (first object-and-type)
                                                                    (second object-and-type)
                                                                    :acceptably t))))))))
               )
             commands)))



;; (url-for-application `(view-quote ,(db 'db-quote 12345)))


;; now, to handle some administrative gubbins...
(defclass clim-web-manager ()
  ;; Should we have a session object as a slot?
  ())

(defmethod clim-styles-and-scripts ((self clim-web-manager) &key lispm)
  (mhtml (:link :rel "stylesheet" :href "/pub/animate.min.css")
         (:style "#cmd:focus {outline: none;} #cmd {background-color: #f6f6f6}"
                 (when lispm
                   (html "@font-face {
  font-family: 'lispm';
  src: url('/pub/LispM-Font/LispM-Monospace.otf') format('opentype');
  src: url('/pub/LispM-Font/LispM-Monospace.ttf') format('truetype');
}
"))
                 (when lispm
                   ".command-prompt {
  display: inline-block;
  float: left;
}")

                 "
.command {
  display: flex;
}

.command input {
  flex-grow: 1;
}

#cmd__hints {
   position: absolute;
   padding-top:5px;
}

.command, #cmd, .command-prompt {
  font-family: lispm, Menlo, Monaco;
  font-size: 13px;"
                 (when lispm (html "-webkit-font-smoothing: none;"))
                 "white-space: pre;
}

#_console {
  background-color:#fff;

}

.command {
  margin-top: 8px;
}
#cmd {
  border: none;
}
.command .presentation {
  display:inline-block;
  vertical-align: top;
  font-family: lispm, Menlo, Monaco;
}

/* Sensitive tables */
table.sensitive-rows tbody tr:hover {
   background-color:rgba(0,0,0,0.06);
}

table.clim th {
  text-align: left; font-weight: normal;
}

table.clim {
  border:none;
  border-bottom: 1px solid #666;
  border-top: 1px solid #666;
  margin-top: 20px;
  margin-bottom: 20px;
  margin-left: 40px;
  width: 80%;
}

table.clim thead tr {
  border-bottom: 1px solid #666;
}

table.clim td, table.clim th {
   padding: 5px 10px 5px 10px;
}


/* Dialogs (nice) */

#__CMP__dialog .dialog-container, #welcome .dialog-container {
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    display: flex;
    justify-content: center;
}

#welcome .dialog .command {display:none;}
#welcome .dialog table.clim {width:90%;}
#welcome .dialog {
   background-color:white !important;
}

#__CMP__dialog .dialog-container .dialog, #welcome .dialog-container .dialog {
   padding: 10px 20px 10px 10px;
   background-color: #eee;
   box-shadow: 5px 5px 45px rgba(0,0,0,0.4);
   top:50%;
   left:50%;
   z-index:1000;
   border: 1px solid rgb(202,202,202);
   border-radius:4px;
   outline: none;
   margin: auto;
   max-height: 100%;
   overflow: auto;
}

#__CMP__dialog .field {
   margin-top:10px;
}

#__CMP__dialog .label {
    float: left;
    width: 250px;
    text-align: right;
    padding-top: 3px;
    padding-right: 12px;
}

#__CMP__dialog input {
    width:200px;
}

#__CMP__dialog h2 {
   font-size: 26px; font-weight: lighter;
   margin:0;
   padding-left: 5px;
}

#__CMP__OK, #__CMP__Cancel {
   font-weight: 200;
   font-size: 16px;
   background: none;
   border: 1px solid #eee;
   margin-top: 10px;
}

#__CMP__OK {
   float: right;
}

#__CMP__OK:hover, #__CMP__Cancel:hover {
   background-color: #ddd;
}

#__CMP__dialog .buttons {
   clear:both;
   border-top:1px solid #ddd;
   margin-top:30px;
}

")))

(defmethod initial-command-table ((x clim-web-manager))
  *base-command-table*)

;; !!! This could inspect the session to figure out what command table to use
(defmethod current-command-table ((x clim-web-manager))
  ;; *base-commands*
  ;; if you want a different command table then store it in the session and I will look for it there
  (or (session-datum :command-table)
      (initial-command-table x)))

;; !!! This might need augmenting
(defmethod draw-clim-page ((x clim-web-manager) &key &allow-other-keys)
  ;; If there were some kind of header then a method like this would be expected to know how to draw it.
  (mweb-page))

;; I won't specify a default one
(defparameter *clim-web-manager* nil)

(defparameter *clim-debug-on* nil)

(defmethod clim-request-handler ((self clim-web-manager))
  (let ((package *package*))
    (lambda (request entity)
      (let* ((http-response::*request* request)
             (http-response::*session* (session:get-request-session "<default>" request))
             (*package* package)
             (*m-entity* entity)
             (*clim-web-manager* self)
             (climwi::*active-command-table* (current-command-table self)))
        (flet ((run-command ()
                 (web-monad-handle-request
                    (with-web-monad
                      command
                      <- (maccept "command"
                                  'command :view (make-instance 'url-view :url-prefix "/app/"))
                      (if command
                          (execute-command command)
                          ;; !!! Show a 404 here:-
                          (error "Please log in again to use this system")))

                    request)))
          (let ((result
                  (if *clim-debug-on*
                      (run-command)
                      (handler-case
                          (run-command) 
                        (error (c)
                          (web-monad-handle-request
                           (error-page c (mhtml (:style "h1 {display: none;} p {font-size: 30px; font-weight: lighter; color: #555; font-family: Helvetica, sans; text-align: center;}")))
                           request))))))

            ;; !!! Need to handle display errors too
            (render-web-thunk result (web-thunk-event-handlers result)
                              ;; ajax-id
                              :renderer (if (testable-query-value "__AJAX" request)
                                            #'web-thunk-ajax-render
                                            #'web-thunk-render))))))))

;; should the default web manager provide anything as standard?
(defmethod web-manager-context-property ((x t) name)
  (declare (ignore name))
  nil)

(defmethod web-manager-context-property ((x list) name)
  (second (member name x)))

(defun context-property (name)
  (web-manager-context-property *clim-web-manager* name))


;; this will show all the extra bits which make a web page including the clim shell stuff
;; generally this should be called as the result of a command...
;; !!! Maybe this could catch errors? We should be able to trap all 'normal' error here I guess.
(defun show-application (monad &key
                                 (include-console t)
                                 (show-console-initially nil)
                                 (show-command-prompt t)
                                 (include-context-menu t)
                                 ;; not yet implemented:-
                                 ;; (include-touch-events t)
                                 (include-drag-and-drop t)
                                 (clickable-commands t)
                                 (draw-header t)
                                 (title nil)
                                 (always-redisplay t)
                                 (needs-redisplay (lambda () nil))
                                 (console-height 350)
                                 (generate-page t)
                                 (command-handler (if include-console
                                                      (append-result-to-transcript-or-redirect)
                                                      (handle-command-without-transcript))))
  
  (with-web-monad

    (mmapcar :web-monad
             (lambda (x)
               (if (first x) (second x) (unit nil)))
             (loop for (option thing) on things by #'cddr
                collect (list option thing)))
    :== (options &rest things)
    
    req <- (mrequest)
    
    - (if (and req generate-page)
          (draw-clim-page *clim-web-manager* :draw-header draw-header :title title)
          ;; I'll do CLIM stuff myself
          (unit nil))

    - (if show-console-initially
          (mhtml (:style "#_console {display: block;}"))
          (mhtml (:style "#_console {display: none;}")))

    
    - (mhtml (:script (:noescape "document.body.onkeyup=function(e) {
        if (e.ctrlKey && (e.keyCode==27 || e.keyCode==219)) {
           var e = $e('_console');
           var s = window.getComputedStyle(e); e.style.display = (s.display==='none')?'block':'none';
           $e('cmd').focus();
        }}")))
    
    - (clim-styles-and-scripts *clim-web-manager*)

    - (if show-command-prompt (unit nil)
          (mhtml (:style ".command-prompt {display:none;}")))
    
    ;; pass nil as console height and it won't be limited.
    - (if console-height
          (mhtml (:style (:format "#_console {
    box-shadow: 0 0 25px rgba(0,0,0,0.5);
    position: fixed;
    left: 0;
    right: 0;
    bottom: 0;
    min-height: 34px;
    max-height: ~Apx;
    /* background-color: green; */
    overflow-y: auto;
    border-top: 1px solid #ddd;
    overflow-x: hidden;}
    #clim_root {padding-bottom: 40px;}
"
                                  console-height)))
          (mhidden-field "__big_console" :default "yes"))
    
    - (command-processor :command-handler command-handler)

    ;; Do I need to check what this returns? If it returns a command then I should probably act on
    ;; said command (perhaps redirecting to another page)
    ;; I'm not sure about that at the moment. I would like commands to just execute without having to pass out of the outer monad.
    - (draw-context "clim_root"
                    monad
                    :redraw (or always-redisplay (funcall needs-redisplay)))

    - (if (not show-console-initially)
          (mhtml (:div :id "welcome")) ; this will be where dialogs appear
          (unit nil))

    - (event-gesture-commands)

    - (if include-context-menu
          (context-menu)
          ;; This might seem strange but I need the command handlers still
          (contextual-menu))
    
    - (options include-console
               (melement
                   (mprogn
                     (mhtml (:div :id "output")) ; should probably be parameterised
                     (melement (command-input-field "cmd")
                       (:div :class "command"
                             (:span :class "command-prompt"
                                    "Command:  ")
                             (draw it)))
                     (mhtml
                       (:script "$e('cmd').focus(); /* document.onclick = function() {$e('cmd').focus()}; */")))
                 (:div :id "_console"
                       (draw it)))
               
               include-drag-and-drop (command-drop-handlers (active-command-table))

               ;; !!! Probably generalise this
               clickable-commands (event-handler '(:presentation command-with-unevaluated-parameters)
                                                 :onclick
                                                 (lambda (obj)
                                                   `(ev "__CMP__c" ,(present-to-string (first obj)
                                                                                       (second obj)
                                                                                       :acceptably t)))))
    
    (unit nil)))




;; This should be enough to get things started
;; (publish-prefix :prefix "/app/" (clim-request-handler (make-instance 'clim-web-manager)))






;; demonstration very simple application
;; the symbol name here is the url (part of)

;; There is an overriden version of this in original clim directory
(define-command (/app/listener :command-table *base-command-table* ; *developer-commands*
                               :name "Listener"
                               :return-type 'web-application)
    ()
  ;; !!! We don't in fact need to draw anything here!
  (show-application
   (mhtml) ; empty
   
   :draw-header nil
   :show-console-initially t
   :console-height nil
   ))



;; this can be used when presenting things which might return commands
;; examples include forms and buttons and things

;; if the monad passed has a value which is a valid command it will be executed immediately
;; we will also ensure to reload the page (if non ajax) or the desired application returned by the
;; command

;; Execute a command returned by a web monad immediately
;; does this need to work via AJAX as well as POST? It probably might as well
(defun mexecute-command (monad)
  (with-web-monad
    value <- monad
    ajaxp <- (mquery "__AJAX")

    (if (and (presentation-typep value 'command)
             (not (climwi::incomplete-command-p value)))

        ;; find the command and look at its type
        (if ajaxp

            ;; this is a bit circular. If we try to redraw the root view we then trigger the command again
            ;; since the command is triggered INSIDE the root view.
            ;; What to do about that?

            ;; It means I have to take over the response here. I'll leave this as is for the moment
            ;; and revisit it if it is a problem in practice

            ;; appending to the transcript works fine. Commands which return commands or apps work fine
            ;; only commands which do other things don't, and those work fine when invoked via the
            ;; normal command procesor

            ;; in ajax context we can use the normal command processor...
            ;; (should we append to output?)
            (mprogn
              (mhttp-response)
              (funcall (append-result-to-transcript-or-redirect) value)
              (end-request))
            (seq
              (let ((target (if (consp x)
                                (present-to-string x 'command :view (make-instance 'url-view))
                                x)))
                (mprogn
                  (mhttp-redirect target)
                  (end-request)))
              :== (redirect-to x)
          
              (climwi::command-return-type (find-command (first value) (active-command-table)))
              :== return-type

              
              (cond ((eq return-type 'web-application)
                     (redirect-to value))
                
                    ((eq return-type 'command)
                     (redirect-to (execute-command value)))
                
                    (t (execute-command value)
                       (seq
                         request <- (mrequest)
                         (redirect-to (s "~A" (request-uri request))))))))
        
        (unit nil))))



;; to make 'normal' pages into CLIM pages the following will work as the POST target for the main form
(define-command (/app/command-processor :name "Command Processor"
                                        :return-type 'web-application
                                        :command-table *base-command-table*
                                        :options '(:exclude-from-menu t))
    ()
  (show-application (with-web-monad (unit nil))
                    :always-redisplay nil))

(define-command (/app/command-runner :name "Command Runner"
                                     :return-type 'web-monad
                                     :command-table *base-command-table*
                                     :options '(:exclude-from-menu t))
    ()

  (with-web-monad
    command <- (mquery (list "__CMP" "c"))

    - (funcall (append-result-to-transcript-or-redirect :mhttp-response t) command)
    - (clim-styles-and-scripts *clim-web-manager*)
    - (context-menu)
    - (command-drop-handlers (active-command-table))
    - (event-handler '(:presentation command-with-unevaluated-parameters)
                     :onclick
                     (lambda (obj)
                       `(ev "__CMP__c" ,(present-to-string (first obj)
                                                           (second obj)
                                                           :acceptably t))))

    (unit nil)))

(defun mreload-page ()
  (with-web-monad
    - (mhttp-response)
    - (mhtml "window.location=window.location")
    (end-request)))

