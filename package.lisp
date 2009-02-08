;;;;
;;;; SPARQL processor
;;;; v. 0.4.0
;;;; Copyright 2005 Richard Newman
;;;; Copyright 2008 Bastian Müller
;;;;
;;;;
;;;; <http://www.w3.org/TR/rdf-sparql-query/>

(defpackage :sicl
  (:export :parse-sparql)
  (:use :cl :lexer :yacc 
	:cl-ppcre :puri
	:rdf-store
	:cl-interpol))