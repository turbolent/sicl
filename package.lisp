;;
;; Copyright 2009 Bastian Mueller 
;;
;; This file is part of sicl.
;;
;; sicl is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; sicl is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with sicl.  If not, see <http://www.gnu.org/licenses/>.

(defpackage :sicl
  (:export :parse-sparql)
  (:use :cl :lexer :yacc 
	:cl-ppcre :puri
	:rdf-store
	:cl-interpol))