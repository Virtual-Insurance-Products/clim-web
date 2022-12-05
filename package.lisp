
(in-package :cl-user)

(defpackage :clim-web
  (:use :cl :climwi :web-monad :com.gigamonkeys.html
        :web-transpiler :vip-utils :net.aserve :anaphors
        :monads
        :web-utils
        ;; :parenscript
        :cl-ppcre)
  (:export #:clim-request-handler
           #:clim-web-manager
           #:clim-styles-and-scripts
           #:/app/listener
           #:/app/command-processor
           #:/app/exec
           #:help
           #:object-html-element
           #:object-html-class
           #:show-application
           #:context-menu
           #:html-present
           #:present-command-if-able
           #:mpresent
           #:maccept
           #:minput-acceptor
           #:form-view
           #:searchable-drop-down-view
           #:url-view
           #:web-application
           #:initial-command-table
           #:draw-clim-page
           #:request-handler
           #:mreload-page
           #:mexecute-command
           #:append-result-to-transcript-or-redirect
           #:current-command-table
           #:set-active-command-table
           #:command-table-entry-first-parameter-type

           #:show-redrawable-object
           #:object-needs-redisplay

           #:web-manager-context-property
           #:context-property

           #:ajax-fade-in

           #:menu-view

           #:editable-value
           #:html
           #:additional-parameters
           #:wrap-input-field

           #:user-form
           #:wrap
           #:field-wrapper
           #:label-wrapper

           #:qname #:form-qname))


