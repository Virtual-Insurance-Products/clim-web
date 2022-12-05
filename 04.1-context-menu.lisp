
(in-package :clim-web)


;; This will be very useful for admin and dev purposes. Less so
;; (though perhaps still a bit) for broker use and definitely not for
;; end users.

;; NOT HANDLED: put partially applied commands into the context menu. I think that will be useful.

;; !!! Should I prefix this? Will worry about later. Probably not a problem
(defun contextual-menu ()
  (mhtml
    (:style "
#_cm {
 position: fixed;
 padding-top: 5px;
 padding-bottom: 5px;
 background-color: #f2f2f2;
 border-radius: 5px;
 border: 1px solid #aaa;
 opacity: 0.98;
 background-filter: blur(10px);
 -webkit-backdrop-filter: blur(10px);
 backdrop-filter: blur(10px);
 box-shadow: 0px 5px 15px rgba(0,0,0,0.4);
 z-index: 1000;
}

#_cmc {
 display: none;
 position: fixed;
 top: 0;
 bottom: 0;
 left: 0;
 right: 0;
 background-color: rgba(0,0,0,0.0);
 z-index: 999;
}

.mi {
  padding-left: 20px;
  padding-right: 20px;
  padding-top: 2px;
  padding-bottom: 2px;
  font-family: Helvetica; 
  font-size: 14px;
  display:block; text-decoration:none;
  color: #333;
  cursor: pointer;
}

.mi:hover {
background-color: #4485FE;
color:white;
}
")
    (:div :id "_cmc"
          :onclick "this.style.display='none';_clr()"
          (:div :id "_cm"))
        
        
    ;; !!! Is redisplay useful? I think I had something in mind here - simple auto redisplay
    (:script (:noescape "
  window.ex = function(command, redisplay, needConfirmation) {
     if (needConfirmation && !confirm(\"Are you sure you want to \" + command + \"?\")) {
       return;
     }
 
     // console.log('Execute '+command);
     ev('__CMP__c', command);    
     _clr();   
  }

  window.ex2 = function(command,id) {
    var x = allFormSubmitValues();
    x['__CMP__c']=command;
    x['__AJAX'] = 1;
    x['__ID'] = id;
    rcall('/app/command-processor', glow.data.encodeUrl(x));
  }

  window._clr = function() {
    $e('_cmc').style.display='none';
    $e('_cm').innerHTML='';
  };

  window._cs = function(event) {
    // show the contextual menu...
    var event = event || window.event;
    event.preventDefault();
    event.stopPropagation();
    var x = event.clientX;
    var y = event.clientY;
    var el = $e('_cm');
    var c = $e('_cmc');

    el.style.top = y+'px';
    el.style.left = x+'px';
    c.style.display='block';
  };

  // add context menu item
  window._ci = function(label, command, redisplay, needConfirmation) {
    var el = $e('_cm');
    var node = document.createElement('DIV');
    if(label) {
      node.className = 'mi';
      node.innerText = label;
      node.onmouseup = function(event) {ex(command, redisplay, needConfirmation)};
    } else {
      node.innerHTML='<hr>';
    }
    el.appendChild(node);
    if (window.innerHeight < el.offsetTop + el.offsetHeight) {
      el.style.top = (window.innerHeight - el.offsetHeight) + 'px';
    }
    if (window.innerWidth < el.offsetLeft + el.offsetWidth) {
      el.style.left = (window.innerWidth - el.offsetWidth) + 'px';
    }
  };

  window._cl = function(label, link) {
    var el = $e('_cm');
    var node = document.createElement('A');
    if(label) {
      node.className = 'mi';
      node.innerText = label;
      node.href = link;
    }
    el.appendChild(node);
  };

  window._cr = function() {
     // sort the context menu items
     var list = document.getElementById('_cm');

     var items = list.childNodes;
     var itemsArr = [];
     for (var i in items) {
         if (items[i].nodeType == 1) { // get rid of the whitespace text nodes
             itemsArr.push(items[i]);
         }
     }

     itemsArr.sort(function(a, b) {
       return a.innerText == b.innerText
               ? 0
               : (a.innerText > b.innerText ? 1 : -1);
     });
     
     for (i = 0; i < itemsArr.length; ++i) {
       list.appendChild(itemsArr[i]);
     }
   }

"))))

(defun command-table-entry-first-parameter-type (entry)
  (second (climwi::make-parameter-list
           (climwi::command-parameters entry))))

;; (command-table-entry-first-parameter-type (find-command 'com-hello *developer-commands*))

(defun context-menu-commands ()
  (with-web-monad
    (climwi::find-commands-in-table
     (lambda (entry)
       (not (climwi::command-option entry :no-context-menu))))
    :== commands
    
    (cons 'or
          (mapcar #'command-table-entry-first-parameter-type commands))
    :== applicable-types
    
    ;; activate the menu when any of those things are clicked on
    - (event-handler (list :presentation applicable-types)
                     :oncontextmenu
                     `(progn
                        (_cs event)))
    
    ;; then output the commands for the menu item...
    - (mmapcar :web-monad
               (lambda (command)
                 (let ((p-type (command-table-entry-first-parameter-type command))
                       (linkp (and (eq 'web-application (command-return-type command))
                                   (not (cddr (climwi::make-parameter-list
                                               (climwi::command-parameters command))))))
                       (need-confirmation (climwi:command-option command :need-confirmation)))
                   (if p-type
                       (event-handler
                        (list :presentation p-type)
                        :oncontextmenu
                        (lambda (object-and-type)
                          `(,(if linkp
                                 '_cl '_ci)
                             ,(if (cdr (climwi::command-parameters command))
                                  (s "~A..." (command-name command))
                                  (command-name command))
                             ,(if linkp
                                  (present-to-string (list (command-symbol-name command)
                                                           (first object-and-type))
                                                     'command
                                                     :view (make-instance 'url-view))
                                  (present-to-string (add-incomplete-markers
                                                      (list (command-symbol-name command)
                                                            (first object-and-type)))
                                                     'command
                                                     :acceptably t))
                             nil
                             ,need-confirmation)))
                       
                       (unit nil))))
               
               commands)

    - (event-handler (list :presentation applicable-types) :oncontextmenu `(_cr))
    
    (unit applicable-types)))



;; !!! This originally allowed more flexibility
(defun context-menu ()
  (with-web-monad
    - (contextual-menu)
    (context-menu-commands)))


;; one possible slight issue with this is that if we put contextual commands in for things
;; with certain parameters they won't be limited to a contained thing.
;; Something to bear in mind.

;; (show-command-table *test-commands-2*)


;; !!! Maybe I should make it possible to have commands which execute locally via JS too?

