
(in-package :clim-web)

(defparameter *highlight-color* "#6FAEFE")
(defparameter *selected-outline* (s "3px solid ~A" *highlight-color*))

;; !!! It would be nice if this could offer a context menu in the event that there are >1 possible commands

;; output the drag/drop bits for the command...
;; OTOH, maybe we can just output different handlers for the different commands
;; the data type will be the command name rather than anything else.

;; !!! I could/should factor out the event handlers
(defmethod command-drop-handlers ((command climwi::command-table-entry))
  (with-presentation-type-decoded (name parameters)
      (climwi::make-parameter-list
       (climwi::command-parameters command))
    name
    (let ((t1 (first parameters))
          (t2 (second parameters)))
      (with-web-monad
        ;; (s "~S" (cons t1 t2)) :== typespec
        (string-downcase (command-name command)) :== typespec
     
        - (event-handler (list :presentation t1) :draggable "true")
        - (event-handler (list :presentation t1) :ondragstart
                         (lambda (object)
                           `(progn
                              (event.stop-propagation)
                              (event.data-transfer.set-data ,typespec
                                                            ,(present-to-string (first object)
                                                                                (second object)
                                                                                :acceptably t))
                              ;; setting text/plain allows us to drag and drop to the command bar
                              ;; for ease of referencing, which is pretty darned handy.
                              ;; In fact, you can drag/drop to any field without special support
                              (event.data-transfer.set-data "text/plain"
                                                            ,(present-to-string (first object)
                                                                                (second object)
                                                                                :acceptably t)))))

      
        (cond ((climwi::command-option command :drag-above)
               `(setf this.style.border-top ,(s "8px solid ~A" *highlight-color*)))
              ((climwi::command-option command :drag-below)
               `(setf this.style.border-bottom ,(s "8px solid ~A" *highlight-color*)))
              ((climwi::command-option command :drag-left)
               `(setf this.style.border-left ,(s "8px solid ~A" *highlight-color*)))
              (t `(setf this.style.box-shadow ,(s "0 0 15px ~A" *highlight-color*))))
        :== highlight

        (cond ((climwi::command-option command :drag-above)
               `(setf this.style.border-top ""))
              ((climwi::command-option command :drag-below)
               `(setf this.style.border-bottom ""))
              ((climwi::command-option command :drag-left)
               `(setf this.style.border-left ""))
              (t `(setf this.style.box-shadow "")))
        :== unhighlight
      
        ;; now to configure viable drop zones...
        - (event-handler (list :presentation t2)
                         :ondrop
                         (lambda (object)
                           `(progn
                              (event.prevent-default)
                              (event.stop-propagation)
                              ,unhighlight
                              (let ((data (event.data-transfer.get-data ,typespec)))
                                (when ,(cond ((climwi::command-option command :drag-above)
                                              `(and data
                                                    (< event.offset-y
                                                       (/ event.target.client-height 2))))
                                             ((climwi::command-option command :drag-below)
                                              `(and data
                                                    (> event.offset-y
                                                       (/ event.target.client-height 2))))
                                             ((climwi::command-option command :drag-left)
                                              `(and data
                                                    (> event.offset-x
                                                       (/ event.target.client-left 2))))
                                             (t `data))

                                  ;; !!! I think I might like to change this to generate the command more
                                  ;; robustly, although I kind of need this to be able to work anyway
                                  (ex (+ ,(command-name command)
                                         " "
                                         data
                                         " " ,(present-to-string (first object)
                                                                 (second object)
                                                                 :acceptably t))))))))
      
        - (event-handler (list :presentation t2)
                         :ondragover
                         ;; only prevent default action when we can get the data we want
                         ;; - unfortunately I can't seem to get the data at this point in time
                         ;; which means I can't only activate drop zones which will accept this command
                         `(if ,(let ((base `(> (event.data-transfer.types.index-of
                                                ,typespec)
                                               -1)))
                                    (cond ((climwi::command-option command :drag-above)
                                           `(and ,base
                                                 (< event.offset-y
                                                    (/ event.target.client-height 2))))
                                          ((climwi::command-option command :drag-below)
                                           `(and ,base
                                                 (> event.offset-y
                                                    (/ event.target.client-height 2))))
                                          ((climwi::command-option command :drag-left)
                                           `(and ,base
                                                 (> event.offset-x
                                                    (/ event.target.client-left 2))))
                                          (t base)))
                              (progn
                                (setf _ev event)
                                ,highlight
                                (event.prevent-default)
                                (event.stop-propagation))
                              ,unhighlight))
      

        - (event-handler (list :presentation t2)
                         :ondragleave
                         `(progn
                            ,unhighlight
                            (event.stop-propagation)
                            (event.prevent-default)))
      
      
        (unit nil)))))


(defmethod command-drop-handlers ((c command-table))
  ;; !!! DEFINE JS support functions here
  (mmapcar :web-monad
           #'command-drop-handlers
           (find-commands-in-table (lambda (x)
                                     (eq (climwi::command-option x :gesture)
                                         :drag-and-drop))
                                   c)))




