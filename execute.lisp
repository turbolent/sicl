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

(in-package :sicl)

(defun sparql-transform (element)
  (multiple-value-bind (results filters)
      (sparql-transform* (car element)
			 (cdr element))
    (if filters
	(loop for result in results
	   if (every #'(lambda (filter)
			 (funcall filter result))
		     filters)
	   collect result)
	results)))

(defmethod sparql-transform* ((type (eql 'triple)) triple)
  (match-bgp triple))

(defmethod sparql-transform* ((type (eql 'union)) elements)
  (loop for element in elements
     append (sparql-transform element)))

(defmethod sparql-transform* ((type (eql 'graph-pattern)) elements)
  (let (results filters)
    (dolist (element elements)
      (case (car element)
	(filter (push (cadr element) filters))
	(optional (multiple-value-bind (results* filters*)
		      (sparql-transform (cadr element))
		    (setf results 
			  (left-join results results*
				     filters*))))
	(t (multiple-value-bind (results* filters*)
	       (sparql-transform element)
	     (setf filters (nconc filters filters*))
	     (setf results (if results 
			      (join results results*) 
			      results*))))))
    (values results filters)))

(defmethod sparql ((type (eql :select)) 
		   &key vars from where order
		   distinct (offset 0) limit)
  (let ((results (sparql-transform where))
	(steps (list 
		;; Step 1 : ToList
		;; done
		;; Step 2 : ORDER BY
		#'(lambda (results)
		    ;; TODO
		    results)
		;; Step 3 : Projection
		#'(lambda (results)
		    (if vars 
			(projection results vars)
			results))
		;; Step 4 : DISTINCT
		#'(lambda (results)
		    (if distinct
			(remove-duplicates results :test #'equal)
			results))
		;; Step 5 : REDUCED
		;; Step 6 : OFFSET and LIMIT
		#'(lambda (results)
		    (let ((limit (or limit 
				     (- (length results)
					offset))))
		      (subseq results offset (+ offset limit)))))))
    (reduce #'(lambda (input step)
		(funcall step input))
	    steps
	    :initial-value results)))