
(asdf:defsystem :clim-web
  :description "Bits to do CLIM web pages"
  :author "VIP"
  :serial t
  :depends-on ("vip-utils" "aserve" "html" 
                           "http-response" "session"
                           "anaphors" "parenscript"
                           "vip-clim-core"
                           "web-utils"
                           "web-transpiler"
                           "web-monad"
                           "cl-ppcre")
  
  :components ((:file "package")
               (:file "01-web-presentations")
               (:file "02.2-accept-web-monad")
               (:file "03.1-mcommand")
               (:file "04-shells")
               (:file "04.1-context-menu")
               (:file "04.2-drag-and-drop")
               (:file "04.3-touch-events")
               (:file "04.4-applications")

               (:file "ajax-views")

               (:file "more-web-presentations")
               
               )
  )
