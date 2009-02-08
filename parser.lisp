;;
;; Copyright 2006 Richard Newman 
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

(defparameter *sparql-final* nil)

(defparameter *parse-base* nil)

(defparameter *used-blank-nodes* nil)

(defparameter *bgp-blank-nodes* nil)

(defparameter unicode-scanner
  (create-scanner "(?:\\\\u([0-9A-Fa-f]{4}))|(?:\\\\U([0-9A-Fa-f]{8}))"))

(defun resolve-unicode (input start end
			      match-start match-end
			      reg-starts reg-ends)
  (declare (ignore start end match-start match-end))
  (flet ((reg-or (reg)
	    (or (aref reg 0)
		(aref reg 1))))
    (let ((chode (parse-integer input
				:radix 16
				:start (reg-or reg-starts)
				:end (reg-or reg-ends))))
      (coerce (list (code-char chode)) 'string))))

(defun process-unicode (string)
  (regex-replace-all unicode-scanner string
		     #'resolve-unicode))

(defun maybe-use-base (iri)
  (let ((uri (uri iri)))
    (if (and *parse-base* 
	     (or (eq #\# (aref iri 0)) ; hack :(
		 (eq (first (uri-parsed-path uri))
		     :relative)))
	(concatenate 'string *parse-base* iri)
	iri)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun prefix-it-up (one two-and-three)
    "Take (4 (+ 5)), return (+ 4 5).
     UNLESS! It's a funcall.
     (prefix-it-up 'a '(funcall-sparql-uri !some-uri param1))
     =>
     '(funcall-sparql-uri !some-uri a param1)"
    (if (eq (car two-and-three)
            'funcall-sparql-uri)
	(concatenate 'list (butlast two-and-three) (list one) (last two-and-three))
	(list (car two-and-three) one (cadr two-and-three))))
  
  (defun strip-brackets (br1 exp br2)
    (declare (ignore br1 br2))
    exp)

  (defun record-blank-node (node)
    (if (member node *used-blank-nodes*)
	(error "Blank node used in different BGPs.")
      (progn (push node *bgp-blank-nodes*)
	     node))))

(defmethod print-object ((object yacc::parser) stream)
  (format stream "#<parser, ~A states>" (yacc::parser-states object)))

(defun record-prefix (prefix uri)
  (declare (type string prefix uri))
  (register-namespace (subseq prefix 0 (- (length prefix) 1))
		      uri))

;;; Parser
(defun parse-sparql (string &optional 
		     (default-prefixes (namespaces))
		     (default-base nil))
  "Parse the given SPARQL query into s-expressions.
   Return the s-expression, prefix dictionary, and base URI (if there is one).
   Optionally take a hash of prefixes and a default base URI. These will be 
   overridden by the contents of the query."
  (declare (special *sparql-final*)
           (type string string))
  ;; http://www.w3.org/TR/rdf-sparql-query/#queryString
  (setf string (process-unicode string))  

  (let ((rdf-store::*namespaces* (make-hash-table :test 'equal))
	(*parse-base* default-base)
	(*used-blank-nodes* (list))
	(*bgp-blank-nodes* (list)))
    (loop for (prefix . uri) in default-prefixes
       do (register-namespace prefix uri))
    (let ((s-exp
	   (yacc:parse-with-lexer
	    (sparql-lexer string
			  :ignores '(ws pn-chars-base pn-chars-u
					pn-chars comment))
	    *sparql-final*)))
      (values s-exp
	      (namespaces)
	      *parse-base*))))

(let ((*readtable* (copy-readtable nil)))
  (yacc:define-parser
      *sparql-final*
    (:start-symbol Query)
    
    (:terminals
     (DOT STAR COMMA SEMICOLON OPEN-PAREN CLOSE-PAREN OPEN-BRACE CLOSE-BRACE
	  OPEN-BRACKET CLOSE-BRACKET OR AND EQUAL UNEQUAL LESS-THAN 
	  GREATER-THAN LESS-THAN-OR-EQUAL GREATER-THAN-OR-EQUAL NOT PLUS 
	  MINUS DIVIDE TYPE BASE PREFIX SELECT CONSTRUCT DESCRIBE ASK 
	  ORDER-BY LIMIT OFFSET DISTINCT REDUCED FROM NAMED WHERE GRAPH 
	  OPTIONAL UNION K-FILTER A STR LANG LANGMATCHES DATATYPE BOUND 
	  SAMETERM ISURI ISIRI ISLITERAL ISBLANK REGEX TRUE FALSE ASC DESC
	  
	  IRI-REF PNAME-NS PNAME-LN BLANK-NODE-LABEL VAR1 VAR2 LANGTAG
	  INTEGER DECIMAL DOUBLE INTEGER-POSITIVE DECIMAL-POSITIVE
	  DOUBLE-POSITIVE INTEGER-NEGATIVE DECIMAL-NEGATIVE DOUBLE-NEGATIVE
	  EXPONENT STRING-LITERAL1 STRING-LITERAL2 STRING-LITERAL-LONG1
	  STRING-LITERAL-LONG2 ECHAR K-NIL WS ANON PN-CHARS-BASE PN-CHARS-U
	  PN-CHARS PN-PREFIX PN-LOCAL))
    
    (:precedence nil)
    
    ;; [1] Query ::= Prologue ( SelectQuery | ConstructQuery | 
    ;;                          DescribeQuery | AskQuery )
    (Query
     (Prolog Query-aux
	     #'(lambda (prolog query)
		 (declare (ignore prolog))
		 (cons 'sparql query))))
    
    (Query-aux
     SelectQuery
     ConstructQuery
     DescribeQuery
     AskQuery)
    
    ;; [2] Prologue ::= BaseDecl? PrefixDecl*
    (Prolog
     nil
     (BaseDecl? PrefixDecl* 
		#'(lambda (b p)
		    (declare (ignore b p))
		    nil)))
    
    (BaseDecl?
     nil
     BaseDecl)
    
    ;; [3] BaseDecl ::= 'BASE' IRI_REF
    (BaseDecl
     (BASE IRI-REF
	   #'(lambda (base iri) 
	       (declare (ignore base))
	       #+:debug (format t "Found base: ~S~%" iri)
	       (setf *parse-base* iri)
	       nil)))
    
    (PrefixDecl*
     nil
     (PrefixDecl* PrefixDecl))
    
    ;; [4] PrefixDecl ::= 'PREFIX' PNAME_NS IRI_REF
    (PrefixDecl
     (PREFIX PNAME-NS IRI-REF
	     #'(lambda (prefix ns iri)
		 (declare (ignore prefix))
		 #+:debug (format t "Found prefix ~S ~S~%" ns iri)
		 (record-prefix ns iri)
		 nil)))
    
    ;; [5] SelectQuery ::= 'SELECT' ( 'DISTINCT' | 'REDUCED' )? ( Var+ | '*' ) 
    ;;                              DatasetClause* WhereClause SolutionModifier
    (SelectQuery
     (SELECT DISTINCT-or-REDUCED? Var+-or-STAR
	     DatasetClause* WhereClause SolutionModifier
	     #'(lambda (select distinct vars dataset where modifier)
		 (declare (ignore select))  
		 (nconc (list ':select)
			(when distinct
			  (list ':distinct 't))
			(when vars
			  (list ':vars (list 'quote vars)))
			(when dataset
			  (list ':from (list 'quote dataset)))
			(list ':where (list 'quote where))
			(when modifier
			  modifier)))))
    
    (DISTINCT-or-REDUCED?
     nil
     REDUCED
     DISTINCT)
    
    (Var+-or-STAR
     Var+
     (STAR (constantly nil)))
    
    (Var+
     (Var)
     (Var+ Var #'(lambda (vs v) 
		   #+:debug (format t "Var+: ~S ~S~%" vs v)
		   (nconc vs (list v)))))
    
    (DatasetClause*
     nil
     (DatasetClause* DatasetClause
		     #'(lambda (x y)
			 (nconc x (list y)))))
    
    ;; [6] ConstructQuery ::= 'CONSTRUCT' ConstructTemplate DatasetClause* 
    ;;                                    WhereClause SolutionModifier
    (ConstructQuery
     (CONSTRUCT ConstructTemplate DatasetClause*
		WhereClause SolutionModifier
		#'(lambda (verb template dataset where modifier)
		    (declare (ignore verb modifier))
		    (nconc (list ':construct)
			   (list ':construct-pattern
				 (list 'quote
				       template))
			   (when dataset
			     (list ':from (list 'quote dataset)))
			   (list ':where (list 'quote where))))))
    
    ;; [7] DescribeQuery ::= 'DESCRIBE' ( VarOrIRIref+ | '*' ) DatasetClause* 
    ;;                                  WhereClause? SolutionModifier
    (DescribeQuery
     (DESCRIBE VarOrIRIref+-or-STAR DatasetClause* 
	       WhereClause? SolutionModifier
	       #'(lambda (describe vars-or-refs-or-star dataset where modifier)
		   (declare (ignore describe))
		   (let (vars nodes)
		     (unless (eq vars-or-refs-or-star 'STAR)
		       (loop for x in vars-or-refs-or-star do
                            (if (symbolp x)
				(push x vars)
				(push x nodes))))
		     (nconc (list ':describe)
			    (when vars
			      (list ':vars (list 'quote vars)))
			    (when nodes
			      (list ':targets (list 'quote nodes)))
			    (when dataset
			      (list ':from (list 'quote dataset)))
			    (when where
			      (list ':where (list 'quote where)))
			    (when modifier
			      modifier))))))
    
    (WhereClause?
     nil
     WhereClause)
    
    (VarOrIRIref+-or-STAR
     VarOrIRIref+
     (STAR (constantly nil)))
    
    (VarOrIRIref+
     (VarOrIRIref)
     (VarOrIRIref+ VarOrIRIref
		   #'(lambda (vars var)
		       #+:debug (format t "~%  Var: ~S~%" var)
		       (nconc vars (list var)))))
    
    ;; [8] AskQuery ::= 'ASK' DatasetClause* WhereClause
    (AskQuery
     (ASK DatasetClause* WhereClause
	  #'(lambda (verb dataset where)
	      (declare (ignore verb))
	      (nconc (list ':ask)
		     (when dataset
		       (list ':from (list 'quote dataset)))
		     (list ':where (list 'quote where))))))
    
    ;; [9] DatasetClause ::= 'FROM' ( DefaultGraphClause | NamedGraphClause )
    (DatasetClause
     (FROM DefaultGraphClause 
	   #'(lambda (x y) 
	       (declare (ignore x)) y))
     (FROM NamedGraphClause 
	   #'(lambda (x y) 
	       (declare (ignore x)) y)))
    
    ;; [10] DefaultGraphClause ::= SourceSelector
    (DefaultGraphClause 
	SourceSelector)
    
    ;; [11] NamedGraphClause ::= 'NAMED' SourceSelector
    (NamedGraphClause 
     (NAMED SourceSelector))
    
    ;; [12] SourceSelector ::= IRIref
    (SourceSelector 
     IRIref)
    
    ;; [13] WhereClause ::= 'WHERE'? GroupGraphPattern
    (WhereClause
     (WHERE GroupGraphPattern 
	    #'(lambda (where gp)
		(declare (ignore where)) gp))
     GroupGraphPattern)
    
    ;; [14] SolutionModifier ::= OrderClause? LimitOffsetClauses?
    (SolutionModifier
     (OrderClause? LimitOffsetClauses?
		   #'(lambda (order limit-offset)
		       (nconc order limit-offset))))
    
    (LimitOffsetClauses?
     nil
     LimitOffsetClauses)

    ;; [15] LimitOffsetClauses ::= ( LimitClause OffsetClause? | OffsetClause LimitClause? )
    (LimitOffsetClauses
     (LimitClause OffsetClause?
		  #'(lambda (limit offset)
		      (nconc limit offset)))
     (OffsetClause LimitClause?
		   #'(lambda (offset limit)
		       (nconc offset limit))))
    
    (OrderClause? 
     nil 
     OrderClause)
    
    (LimitClause?
     nil 
     LimitClause)
    
    (OffsetClause?
     nil 
     OffsetClause)
    
    ;; [16] OrderClause ::= 'ORDER' 'BY' OrderCondition+
    (OrderClause 
     (ORDER-BY OrderCondition+
	       #'(lambda (order condition) 
		   (declare (ignore order))
		   (list :order (list 'quote condition)))))
    
    (OrderCondition+
     (OrderCondition)
     (OrderCondition+ OrderCondition
		      #'(lambda (x y)
			  (nconc x (list y)))))

    ;; [17] OrderCondition ::= ( ( 'ASC' | 'DESC' ) BrackettedExpression ) |
    ;;                         ( Constraint | Var ) 
    (OrderCondition
     (ASC-or-DESC BrackettedExpression)
     Constraint-or-Var)
    
    (ASC-or-DESC 
     ASC 
     DESC)

    (Constraint-or-Var
     Constraint
     (Var #'(lambda (var) 
	      (list 'asc var))))
    
    ;; [18] LimitClause ::= 'LIMIT' INTEGER
    (LimitClause
     (LIMIT INTEGER
	    #'(lambda (limit value)
		(declare (ignore limit))
		#+:debug (format t "~%  Limit: ~S~%" value)
		(list :limit value))))
    
    ;; [19] OffsetClause ::= 'OFFSET' INTEGER
    (OffsetClause
     (OFFSET INTEGER
	     #'(lambda (offset value)
		 (declare (ignore offset))
		 #+:debug (format t "~%  Offset: ~S~%" value)
		 (list :offset value))))
    
    ;; [20] GroupGraphPattern ::= '{' TriplesBlock? 
    ;;                                ( ( GraphPatternNotTriples | Filter ) 
    ;;                                    '.'? TriplesBlock? )* '}'
    ;; TODO: blank node tracking
    (GroupGraphPattern
     (OPEN-BRACE TriplesBlock? GraphPattern* CLOSE-BRACE
		  #'(lambda (b1 tb graph b2)
		      (declare (ignore b1 b2))
		      #+:debug (format t "~%  GroupGraphPattern: ~S~%" graph)
		      (dolist (node *bgp-blank-nodes*)
			(push node *used-blank-nodes*))
		      (setf *bgp-blank-nodes* (list))
		      (nconc (cons 'graph-pattern tb) graph))))
    
    (TriplesBlock?
     nil
     TriplesBlock)
    
    (GraphPattern*
     nil
     (GraphPattern* GraphPattern-aux
	       #'(lambda (triples aux)
		   #+:debug (format t "GraphPattern: ~S, ~S (nconc)~%"
				    triples aux)
		   (nconc triples aux))))
    
   
    (GraphPattern-aux
     (GraphPatternNotTriples-OR-Filter DOT? TriplesBlock?
	#'(lambda (nt dot tb)
	    (declare (ignore dot))
	    #+:debug (format t "GraphPattern-aux: ~S~%    ~S~%"
			     nt tb)
	    (nconc (list nt) tb))))
    
    (GraphPatternNotTriples-OR-Filter
     GraphPatternNotTriples
     Filter)

    (DOT?
     nil
     DOT)
 
    ;; [21] TriplesBlock ::= TriplesSameSubject ( '.' TriplesBlock? )?
    (TriplesBlock
     TriplesSameSubject
     (TriplesSameSubject DOT TriplesBlock?
			 #'(lambda (ts dot tb)
			     (declare (ignore dot))
			     (nconc ts tb))))
    
    ;; [22] GraphPatternNotTriples ::= OptionalGraphPattern | 
    ;;                                 GroupOrUnionGraphPattern |
    ;;                                 GraphGraphPattern
    (GraphPatternNotTriples
     OptionalGraphPattern
     GroupOrUnionGraphPattern
     GraphGraphPattern)

    ;; [23] OptionalGraphPattern ::= 'OPTIONAL' GroupGraphPattern
    (OptionalGraphPattern
     (OPTIONAL GroupGraphPattern
	       #'(lambda (optional ggp)
		   (declare (ignore optional))
		   #+:debug (format t "~%  OptionalGraphPattern: OPTIONAL ~S~%" ggp)
		   (list 'optional ggp))))
    
    ;; [24] GraphGraphPattern ::= 'GRAPH' VarOrIRIref GroupGraphPattern
    (GraphGraphPattern
     (GRAPH VarOrIRIref GroupGraphPattern))
    
    ;; [25] GroupOrUnionGraphPattern ::= GroupGraphPattern ( 'UNION' GroupGraphPattern )*
    (GroupOrUnionGraphPattern
     (GroupGraphPattern GroupOrUnionGraphPattern-aux*
			#'(lambda (ggp gougp)
			    #+:debug (format t "~%  GroupOrUnionGraphPattern: ~S ~S~%" 
					     ggp gougp)
			    (if gougp
				(nconc (list 'UNION ggp) gougp)
				ggp))))
    
    ;; For convenience we only return the pattern from GroupOrUnionGraphPattern-aux*.
    ;; THESE ARE ALWAYS UNION PATTERNS.
    (GroupOrUnionGraphPattern-aux*
     nil
     (GroupOrUnionGraphPattern-aux* GroupOrUnionGraphPattern-aux
                                    #'(lambda (ggp gougp)
                                        #+:debug
                                        (format t "~%  GroupOrUnionGraphPattern-aux*: ~S ~S~%"
                                                ggp gougp)
                                        (if ggp
					    (nconc ggp gougp)
					    gougp))))
    
    (GroupOrUnionGraphPattern-aux
     (UNION GroupGraphPattern
	    #'(lambda (union ggp)
		(declare (ignore union))
		#+:debug
		(format t "~%  GroupOrUnionGraphPattern-aux UNION ~S~%    Returning just pattern.~%" ggp)
		(list ggp))))
    
    ;; [26] Filter ::= 'FILTER' Constraint
    (Filter
     (K-FILTER Constraint
	       #'(lambda (filter constraint) 
		   (declare (ignore filter))
		   (list 'filter constraint))))

    ;; [27] Constraint ::= BrackettedExpression | BuiltInCall | FunctionCall
    (Constraint
     BrackettedExpression
     BuiltInCall
     FunctionCall)
 
    ;; [28] FunctionCall ::= IRIref ArgList
    (FunctionCall
     (IRIref ArgList
	     #'(lambda (iri args)
		 (nconc (list 'funcall-sparql-uri iri)
			args))))
    
    ;; [29] ArgList ::= ( NIL | '(' Expression ( ',' Expression )* ')' )
    (ArgList
     (K-NIL
      #'(lambda (empty)
	  (declare (ignore empty))
	  nil))
     (OPEN-PAREN Expression ArgList-aux* CLOSE-PAREN
		 #'(lambda (br1 exp args br2)
		     (declare (ignore br1 br2))
		     (if args
			 (nconc (list exp)
				args)
			 (list exp)))))
    
    (ArgList-aux*
     nil
     (ArgList-aux* ArgList-aux
		   #'(lambda (x y)
		       (push y x))))
    
    (ArgList-aux
     (COMMA Expression
	    #'(lambda (comma expression)
		(declare (ignore comma))
		expression)))

    ;; [30] ConstructTemplate ::= '{' ConstructTriples? '}'
    (ConstructTemplate
     (OPEN-BRACE ConstructTriples? CLOSE-BRACE
		 #'(lambda (b1 graph b2)
		     (declare (ignore b1 b2))
		     (push 'graph-pattern graph))))

    (ConstructTriples?
     nil
     ConstructTriples)

    ;; [31] ConstructTriples ::= TriplesSameSubject ( '.' ConstructTriples? )?
    (ConstructTriples
     (TriplesSameSubject 
      #'(lambda (tr) 
	  #+:debug (format t "ConstructTriples clause 1: ~S~%" tr)
	  tr))
     (TriplesSameSubject DOT
			 #'(lambda (tr do)
			     (declare (ignore do))
			     tr))
     (TriplesSameSubject DOT ConstructTriples
			 #'(lambda (tr do ct)
			     (declare (ignore do))
			     (nconc tr ct))))
    
    #|
    (parse-sparql "SELECT * {
      ?x ?y [ ?p ?q ; 
              ?r ?s , ?t ] .
      ?u ?v ?w ;
         ?vv ?ww , ?xx .
      [ ?b ?c ] ?d ?e ;
                ?f ?g , ?h . }")
    
    Yields:
    ?x ?y <n>
    <n> ?p ?q
    <n> ?r ?s
    <n> ?r ?t
    ?u ?v ?w
    ?u ?vv ?ww
    ?u ?vv ?xx
    <m> ?b ?c
    <m> ?d ?e
    <m> ?f ?g
    <m> ?f ?h
    |#

    ;; [32] TriplesSameSubject ::= VarOrTerm PropertyListNotEmpty | 
    ;;                             TriplesNode PropertyList
    (TriplesSameSubject
     (VarOrTerm PropertyListNotEmpty
		#'(lambda (var prop-pairs) 
		    #+:debug (format t "TriplesSameSubject, clause 1: ~S, ~S~%"
				     var prop-pairs)
		    (let ((res (list)))
		      (loop for (property object) in prop-pairs do
			 ;; object can be a list of triples with the same bnode subject.
			   (flet ((add ()
				    (push (list 'triple var
						property object)
					  res)))
			     (if (consp object)
				 ;; If it's valid...
				 (cond ((and (listp (car object))
					     (eq 'triple (caar object)))
					;; This sequence of nreverses and interpolations results in preserved ordering.
					(setf res (nconc (nreverse object) 
							 (list (list 'triple var property
								     (second (car object)))) res)))
				       ((eq 'literal (car object))
					(add))
				       (t (warn "Non-triple found where symbol expected: Triples1, var: ~S, prop-pairs: ~S, object: ~S" 
						var prop-pairs object)))
				 (add))))
		      (nreverse res))))
    
     (TriplesNode PropertyList
		  ;; Receives something like 
		  ;;   - [ ?b ?c ] ?d ?e . = ((triple |BLANK| |b| |c|)), ((|a| . |b|))
		  ;;   - [ ?b ?c ; ?d ?e ] = 
		  ;;        ((triple |BLANK| |b| |c|) (triple|BLANK| |d| |e|)), nil
		  ;;   - ( [ ?b ?c ] ) ?d ?e = 
		  ;;        ((triple rdf:first |BLANK|) (|BLANK| |b| |c|) 
		  ;;         (triple rdf:rest rdf:nil)), ((|d| . |e|))
		  ;; => triples-node is always a list of triples
		  ;;    (no differentiation between blank-node triples
		  ;;     or collection triples -- important is subject
		  ;;     of the first triple!)
		  ;; => property-list is optional
		  (lambda (list prop-pairs) 
		    #+:debug (format t "TriplesSameSubject, clause 2: ~S, ~S~%"
				     list prop-pairs)
		    (nconc list
			   (when prop-pairs
			     (let ((subject (cadar list)))
			       (loop for (property object) in prop-pairs
				  collect (list 'triple subject property
						(if (listp object)
						    (cadar object)
						  object))
				  if (listp object)
				  append object)))))))

     
    ;; [33] PropertyListNotEmpty ::= Verb ObjectList 
    ;;                               ( ';' ( Verb ObjectList )? )*    
    (PropertyListNotEmpty
     (Verb ObjectList 
	   #'(lambda (verb objects)
	       (mapcar #'(lambda (o)
			   (list verb o))
		       objects)))
     (Verb ObjectList SEMICOLON PropertyList
	   #'(lambda (verb objects sc prop-pairs-or-nil)
	       (declare (ignore sc))
	       (nconc 
		(mapcar #'(lambda (o)
			    (list verb o))
			objects)
		prop-pairs-or-nil))))

    ;; [34] PropertyList ::= PropertyListNotEmpty?
    (PropertyList
     nil
     PropertyListNotEmpty)

    ;; [35] ObjectList ::= Object ( ',' Object )*
    (ObjectList
     (Object)
     (Object COMMA ObjectList
	     #'(lambda (n c os)
		 (declare (ignore c))
		 (push n os))))
    
    ;; [36] Object ::= GraphNode
    (Object
     GraphNode)

    ;; [37] Verb ::= VarOrIRIref | 'a'
    (Verb
     VarOrIRIref
     A)
    
    ;; [38] TriplesNode ::= Collection | BlankNodePropertyList
    (TriplesNode
     Collection 
     BlankNodePropertyList)
    
    ;; [39] BlankNodePropertyList ::= '[' PropertyListNotEmpty ']'
    (BlankNodePropertyList 
     (OPEN-BRACKET PropertyListNotEmpty CLOSE-BRACKET
		   #'(lambda (br1 prop-pairs br2)
		       (declare (ignore br1 br2))
		       (let ((var (blank)))
			 #+:debug (format t "BlankNodePropertyList: ~S, ~S~%"
					  var prop-pairs)
			 (mapcar #'(lambda (pair)
				     (apply 'list 'triple var pair))
				 prop-pairs)))))
    
    ;; [40] Collection ::= '(' GraphNode+ ')'
    (Collection 
     (OPEN-PAREN GraphNode+ CLOSE-PAREN
		 #'(lambda (open collection close)
		     (declare (ignore open close))
		     (let* ((nested-triples (remove-if-not #'listp collection))
			    (triples (let ((blank (blank)))
				       (loop for nested = -1
					  for (first . rest) on collection
					  collect (list 'triple blank
							!rdf:first
					                ; bnode-triples/collection-triples?
							(if (listp first)
							    (cadar (nth (incf nested)
									nested-triples))
							  first))
					  collect (list 'triple blank
							!rdf:rest
							(if (null rest)
							    !rdf:nil
							  (setf blank (blank))))))))
		       (dolist (nested nested-triples)
			 (nconc triples nested))
		       triples))))
    
    (GraphNode+
     (GraphNode)
     (GraphNode GraphNode+
		#'(lambda (node nodes)
		    (cons node nodes))))

    ;; [41] GraphNode ::= VarOrTerm | TriplesNode
    (GraphNode 
     VarOrTerm 
     TriplesNode)
    
    ;; [42] VarOrTerm ::= Var | GraphTerm
    (VarOrTerm 
     Var 
     GraphTerm)

    ;; [43] VarOrIRIref ::= Var | IRIref
    (VarOrIRIref
     Var 
     IRIref)
    
    ;; [44] Var ::= VAR1 | VAR2
    (Var 
     VAR1
     VAR2)
    
    ;; [45] GraphTerm ::= IRIref | RDFLiteral | NumericLiteral | 
    ;;                    BooleanLiteral | BlankNode | NIL
    (GraphTerm
     IRIref
     RDFLiteral
     NumericLiteral
     BooleanLiteral
     BlankNode
     K-NIL)
    
    ;; [46] Expression ::= ConditionalOrExpression
    (Expression
     ConditionalOrExpression)
    
    ;; [47] ConditionalOrExpression ::= ConditionalAndExpression 
    ;;                                  ( '||' ConditionalAndExpression )*
    (ConditionalOrExpression
     (ConditionalAndExpression ConditionalOrExpression-aux
			       #'(lambda (ca co)
				   (if co
				       `(,ca ,co)
				       ca))))
    
    (ConditionalOrExpression-aux
     nil
     (ConditionalOrExpression-aux OR ConditionalAndExpression
				  #'(lambda (or bars and)
				      (declare (ignore bars))
				      (if or
					  `(OR ,@(cdr or) ,and)
					  and))))
    
    ;; [48] ConditionalAndExpression ::= ValueLogical ( '&&' ValueLogical )*
    (ConditionalAndExpression
     (ValueLogical ConditionalAndExpression-aux
		   #'(lambda (v c)
		       (if c
			   `(AND ,v ,@(cdr c))
			   v))))
    
    (ConditionalAndExpression-aux
     nil
     (ConditionalAndExpression-aux AND ValueLogical
				   #'(lambda (and ampersands val)
				       (declare (ignore ampersands))
				       (if and
					   `(AND ,@(cdr and) ,val)
					   val))))
    ;; [49] ValueLogical ::= RelationalExpression
    (ValueLogical RelationalExpression)
    
    ;; [50] RelationalExpression ::= NumericExpression 
    ;;                               ( '=' NumericExpression | 
    ;;                                 '!=' NumericExpression |
    ;;                                 '<' NumericExpression |
    ;;                                 '>' NumericExpression |
    ;;                                 '<=' NumericExpression |
    ;;                                 '>=' NumericExpression )?
    (RelationalExpression
     NumericExpression
     (NumericExpression RelationalExpression-aux
			#'prefix-it-up))

    (RelationalExpression-aux
     (EQUAL NumericExpression
	    #'(lambda (eq ne)
		(declare (ignore eq))
		(list 'funcall-sparql-uri 'RDFterm-equal ne)))
     (UNEQUAL NumericExpression
	      #'(lambda (neq ne)
		  (declare (ignore neq))
		  (list 'not (list 'funcall-sparql-uri 'numeric-less-than ne))))
     (LESS-THAN NumericExpression 
		#'(lambda (lt ne)
		    (declare (ignore lt))
		    (list 'funcall-sparql-uri 'numeric-less-than ne)))
     (GREATER-THAN NumericExpression
		   #'(lambda (gt ne)
		       (declare (ignore gt))
		       (list 'funcall-sparql-uri 'numeric-greater-than ne)))
     (LESS-THAN-OR-EQUAL NumericExpression
			 #'(lambda (lteq ne)
			     (declare (ignore lteq))
			     (list 'or 
				   (list 'funcall-sparql-uri 'numeric-less-than ne)
				   (list 'funcall-sparql-uri 'numeric-equal ne))))
     (GREATER-THAN-OR-EQUAL NumericExpression
			    #'(lambda (gteq ne)
				(declare (ignore gteq))
				(list 'or 
				      (list 'funcall-sparql-uri 'numeric-greater-than ne)
				      (list 'funcall-sparql-uri 'numeric-equal ne)))))
    
    ;; [51] NumericExpression ::= AdditiveExpression
    (NumericExpression
     AdditiveExpression)
    
    ;; [52] AdditiveExpression ::= MultiplicativeExpression 
    ;;                             ( '+' MultiplicativeExpression |
    ;;                               '-' MultiplicativeExpression | 
    ;;                               NumericLiteralPositive | 
    ;;                               NumericLiteralNegative )*
    (AdditiveExpression
     (MultiplicativeExpression AdditiveExpression-aux*
			       #'(lambda (m a)
				   (if a
				       (list m a)
				       m))))
    
    (AdditiveExpression-aux*
     nil
     (AdditiveExpression-aux* AdditiveExpression-aux
			      #'(lambda (s a)
				  (if s
				      (list s a)
				      a))))
    
    (AdditiveExpression-aux
     (PLUS MultiplicativeExpression)
     (MINUS MultiplicativeExpression)
     NumericLiteralPositive
     NumericLiteralNegative)

    ;; [53] MultiplicativeExpression ::= UnaryExpression 
    ;;                                   ( '*' UnaryExpression |
    ;;                                     '/' UnaryExpression )*
    (MultiplicativeExpression
     (UnaryExpression MultiplicativeExpression-aux*
		      #'(lambda (m a)
			  (if a
			      (list m a)
			      m))))
    
    (MultiplicativeExpression-aux*
     nil
     (MultiplicativeExpression-aux* MultiplicativeExpression-aux
				    #'(lambda (s a)
					(if s
					    (list s a)
					    a))))
    
    (MultiplicativeExpression-aux
     (STAR UnaryExpression)
     (DIVIDE UnaryExpression))
    
    ;; [54] UnaryExpression ::= '!' PrimaryExpression | 
    ;;                          '+' PrimaryExpression |
    ;;                          '-' PrimaryExpression |
    ;;                          PrimaryExpression
    (UnaryExpression
     (NOT PrimaryExpression #'(lambda (exc exp)
				(declare (ignore exc))
				(list 'funcall-sparql-uri 'not exp)))
     (PLUS PrimaryExpression 
	   #'(lambda (plus exp) 
	       (declare (ignore plus)) exp))
     (MINUS PrimaryExpression 
	    #'(lambda (minus exp) 
		(declare (ignore minus)) 
		(list '- exp)))
     PrimaryExpression)
    
    ;; [55] PrimaryExpression ::= BrackettedExpression | BuiltInCall | 
    ;;                            IRIrefOrFunction | RDFLiteral | 
    ;;                            NumericLiteral | BooleanLiteral | Var
    (PrimaryExpression
     BrackettedExpression
     BuiltInCall
     IRIrefOrFunction
     RDFLiteral
     NumericLiteral
     BooleanLiteral
     BlankNode
     Var)
      
    ;; [56] BrackettedExpression ::= '(' Expression ')'
    (BrackettedExpression 
     (OPEN-PAREN Expression CLOSE-PAREN
		 #'strip-brackets))

    ;; [57] BuiltInCall ::= 'STR' '(' Expression ')'
    ;;                      | 'LANG' '(' Expression ')'
    ;;                      | 'LANGMATCHES' '(' Expression ',' Expression ')'
    ;;                      | 'DATATYPE' '(' Expression ')'
    ;;                      | 'BOUND' '(' Var ')'
    ;;                      | 'sameTerm' '(' Expression ',' Expression ')'
    ;;                      | 'isIRI' '(' Expression ')'
    ;;                      | 'isURI' '(' Expression ')'
    ;;                      | 'isBLANK' '(' Expression ')'
    ;;                      | 'isLITERAL' '(' Expression ')'
    ;;                      | RegexExpression
    (BuiltInCall
     (STR OPEN-PAREN Expression CLOSE-PAREN
	  #'(lambda (str obr exp cbr)
	      (declare (ignore str obr cbr))
	      (list 'funcall-sparql-uri 'str exp)))
     (LANG OPEN-PAREN Expression CLOSE-PAREN
	   #'(lambda (lang obr exp cbr)
	       (declare (ignore lang obr cbr))
	       (list 'funcall-sparql-uri 'lang exp)))
     (LANGMATCHES OPEN-PAREN Expression COMMA Expression CLOSE-PAREN
		  #'(lambda (langmatches obr exp1 comma exp2 cbr)
		      (declare (ignore langmatches obr comma cbr))
		      (list 'funcall-sparql-uri 'langMatches exp1 exp2)))
     (DATATYPE OPEN-PAREN Expression CLOSE-PAREN
	       #'(lambda (data obr exp cbr)
		   (declare (ignore data obr cbr))
		   (list 'funcall-sparql-uri 'datatype exp)))
     (BOUND OPEN-PAREN Var CLOSE-PAREN
	    #'(lambda (bound obr exp cbr)
		(declare (ignore bound obr cbr))
		`(is-bound (quote ,exp))))
     (ISIRI OPEN-PAREN Expression CLOSE-PAREN
	    #'(lambda (isiri obr exp cbr)
		(declare (ignore isiri obr cbr))
		(list 'funcall-sparql-uri 'isIRI exp)))
     (ISURI OPEN-PAREN Expression CLOSE-PAREN
	    #'(lambda (isuri obr exp cbr)
		(declare (ignore isuri obr cbr))
		(list 'funcall-sparql-uri 'isIRI exp)))
     (ISBLANK OPEN-PAREN Expression CLOSE-PAREN
	      #'(lambda (isblank obr exp cbr)
		  (declare (ignore isblank obr cbr))
		  (list 'funcall-sparql-uri 'isBlank exp)))
     (ISLITERAL OPEN-PAREN Expression CLOSE-PAREN
		#'(lambda (isliteral obr exp cbr)
		    (declare (ignore isliteral obr cbr))
		    (list 'funcall-sparql-uri 'isLiteral exp)))
     RegexExpression)
    
    ;; [58] RegexExpression ::= 'REGEX' '(' Expression ',' Expression ( ',' Expression )? ')'
    (RegexExpression 
     (REGEX OPEN-PAREN Expression COMMA Expression Expression-aux CLOSE-PAREN
	    #'(lambda (regex ob exp1 comma exp2 exp-aux cb)
		(declare (ignore regex ob comma cb))
		(list 'funcall-sparql-uri 'regex exp1 exp2 exp-aux))))
    
    (Expression-aux
     nil
     (COMMA Expression
	    #'(lambda (comma ex)
		(declare (ignore comma))
		ex)))
 
    ;; [59] IRIrefOrFunction ::= IRIref ArgList?
    (IRIrefOrFunction 
     IRIref 
     (IRIref ArgList
	     #'(lambda (iri args)
		 (nconc (list 'funcall-sparql-uri iri)
			args))))

    ;; [60] RDFLiteral ::= String ( LANGTAG | ( '^^' IRIref ) )?
    (RDFLiteral
     (String
      #'string)
     (String LANGTAG
	     #'make-language-literal)
     (String TYPE IRIref
	     #'(lambda (value type iri)
		 (declare (ignore type))
		 (make-datatype-literal value iri))))
   
    ;; [61] NumericLiteral ::= NumericLiteralUnsigned | 
    ;;                         NumericLiteralPositive |
    ;;                         NumericLiteralNegative
    (NumericLiteral
     NumericLiteralUnsigned
     NumericLiteralPositive
     NumericLiteralNegative)
    
    ;; [62] NumericLiteralUnsigned ::= INTEGER | DECIMAL | DOUBLE
    (NumericLiteralUnsigned
     INTEGER
     DECIMAL
     DOUBLE)

    ;; [63] NumericLiteralPositive ::= INTEGER_POSITIVE | 
    ;;                                 DECIMAL_POSITIVE |
    ;;                                 DOUBLE_POSITIVE
    (NumericLiteralPositive
     INTEGER-POSITIVE 
     DECIMAL-POSITIVE
     DOUBLE-POSITIVE)

    ;; [64] NumericLiteralNegative ::= INTEGER_NEGATIVE | 
    ;;                                 DECIMAL_NEGATIVE | 
    ;;                                 DOUBLE_NEGATIVE
    (NumericLiteralNegative
     INTEGER-NEGATIVE
     DECIMAL-NEGATIVE
     DOUBLE-NEGATIVE)

    ;; [65] BooleanLiteral ::= 'true' | 'false'
    (BooleanLiteral
     TRUE
     FALSE)

    ;; [66] String ::= STRING_LITERAL1 | STRING_LITERAL2 | 
    ;;                 STRING_LITERAL_LONG1 | STRING_LITERAL_LONG2
    (String
     STRING-LITERAL1
     STRING-LITERAL2
     STRING-LITERAL-LONG1
     STRING-LITERAL-LONG2)
   
    ;; [67] IRIref ::= IRI_REF | PrefixedName
    (IRIref
     (IRI-REF
      #'(lambda (iri)
	  (intern-uri
	   (uri (maybe-use-base iri)))))
     (PrefixedName
      #'(lambda (name)
	  (intern-uri 
	   (uri (maybe-use-base (expand-qname name)))))))
    
    ;; [68] PrefixedName ::= PNAME_LN | PNAME_NS
    (PrefixedName
     PNAME-LN
     PNAME-NS)
    
    ;; [69] BlankNode ::= BLANK_NODE_LABEL | ANON
    (BlankNode
     (BLANK-NODE-LABEL #'record-blank-node)
     (ANON #'record-blank-node))))