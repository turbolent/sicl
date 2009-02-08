(defpackage #:sicl-system
  (:use #:asdf #:cl))

(in-package :sicl-system)

(defsystem sicl
  :version "0.4.0"
  :depends-on (lexer yacc cl-ppcre puri rdf-store cl-interpol)
  :components ((:file "package")
               (:file "lexer" :depends-on ("package"))
	       (:file "parser" :depends-on ("lexer"))
	       (:file "execute" :depends-on ("parser"))))