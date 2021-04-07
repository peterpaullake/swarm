;;;; swarm.asd

(asdf:defsystem #:swarm
  :description "Describe swarm here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-who #:hunchentoot #:parenscript)
  :components ((:file "package")
               (:file "swarm")))
