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

(enable-interpol-syntax)

;; from <http://www.w3.org/TR/rdf-sparql-query/#sparqlGrammar>
(deflexer sparql-lexer

  ;; other 
  
  (dot "\\.") 
  (star "\\*") 
  (comma ",") 
  (semicolon ";")
  (open-paren "\\(")
  (close-paren "\\)")
  (open-brace "\\{") 
  (close-brace "\\}") 
  (open-bracket "\\[")
  (close-bracket "\\]")
  (or "\\|\\|")
  (and "&&")
  (equal "=")
  (unequal "!=")
  (less-than "<")
  (greater-than ">")
  (less-than-or-equal "<=")
  (greater-than-or-equal ">=")
  (not "!")
  (plus "\\+")
  (minus "-")
  (divide "/")
  (type "\\^\\^")

  (comment "#[^\\r\\n]*")


  ;; keywords
  
  ;; "Keywords are matched in a case-insensitive manner with 
  ;;  the exception of the keyword 'a' [...]"
  
  (base "(?i)BASE")
  (prefix "(?i)PREFIX")

  (select "(?i)SELECT")
  (construct "(?i)CONSTRUCT")
  (describe "(?i)DESCRIBE")
  (ask "(?i)ASK")

  (order-by "(?i)ORDER BY")
  (limit "(?i)LIMIT")
  (offset "(?i)OFFSET")
  (distinct "(?i)DISTINCT")
  (reduced "(?i)REDUCED")

  (from "(?i)FROM")
  (named "(?i)NAMED"
	 (constantly 'named))
  (where "(?i)WHERE")

  (graph "(?i)GRAPH" 
	 (constantly 'graph))
  (optional "(?i)OPTIONAL")
  (union "(?i)UNION")
  (k-filter "(?i)FILTER")
  (a "a"
     (constantly !rdf:type))

  (str "(?i)STR")
  (lang "(?i)LANG")
  (langmatches "(?i)LANGMATCHES")
  (datatype "(?i)DATATYPE")
  (bound "(?i)BOUND")
  (sameterm "(?i)sameTERM")

  (isuri "(?i)isURI")
  (isiri "(?i)isIRI")
  (isliteral "(?i)isLITERAL")
  (isblank "(?i)isBLANK")
  (regex "(?i)REGEX")
  (true "(?i)true"
	(constantly t))
  (false "(?i)false" 
	 (constantly nil))

  (asc "(?i)ASC" 
       (constantly 'asc))
  (desc "(?i)DESC"
	(constantly 'desc))


  ;; terminals

  ;; [77] INTEGER ::= [0-9]+
  (integer "[0-9]+" #'parse-integer)
  
  ;; [78] DECIMAL ::= [0-9]+ '.' [0-9]* | '.' [0-9]+
  (decimal "(?:[0-9]+\\.[0-9]*)|(?:\\.[0-9]+)" 
	   #'read-from-string)
  
  ;; [86] EXPONENT ::= [eE] [+-]? [0-9]+
  (exponent "[eE][+-]?[0-9]+")
  
  ;; [79] DOUBLE ::= [0-9]+ '.' [0-9]* EXPONENT | '.' ([0-9])+ EXPONENT | ([0-9])+ EXPONENT
  (double ("(?:[0-9]+\\.[0-9]*" exponent ")|"
	   "(?:\\.[0-9]+" exponent ")|"
	   "(?:[0-9]+" exponent ")")
	  #'read-from-string)
  
  ;; [80] INTEGER_POSITIVE ::= '+' INTEGER
  (integer-positive ("\\+" integer)
		    #'read-from-string)
  
  ;; [81] DECIMAL_POSITIVE ::= '+' DECIMAL
  (decimal-positive ("\\+" decimal)
		    #'read-from-string)
  
  ;; [82] DOUBLE_POSITIVE  ::= '+' DOUBLE
  (double-positive ("\\+" double)
		   #'read-from-string)

  ;; [83] INTEGER_NEGATIVE ::= '-' INTEGER
  (integer-negative ("-" integer)
		    #'read-from-string)

  ;; [84] DECIMAL_NEGATIVE ::= '-' DECIMAL
  (decimal-negative ("-" decimal)
		    #'read-from-string)

  ;; [85] DOUBLE_NEGATIVE  ::= '-' DOUBLE
  (double-negative ("-" double)
		   #'read-from-string)

  ;; [70] IRI_REF ::= '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
  (iri-ref "<([^<>\"{}|^`\\x00-\\x20\\\\]*)>")

  ;; [95] PN_CHARS_BASE ::= [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | 
  ;;                        [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] |
  ;;                        [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | 
  ;;                        [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
  (pn-chars-base ("[A-Za-z\\xC0-\\xD6\\xD8-\\xF6"
		  #?"\x{f8}-\x{2ff}\x{370}-\x{37D}\x{37F}-\x{1FFF}\x{200C}-\x{200D}"
		  #?"\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}"
		  #?"\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]"))

  ;; [96] PN_CHARS_U ::= PN_CHARS_BASE | '_'
  (pn-chars-u (pn-chars-base "|_"))

  ;; [98] PN_CHARS ::= PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
  (pn-chars (pn-chars-u "|-|[0-9]|" #?"\xB7|[\x{300}-\x{36F}\x{203F}-\x{2040}]"))
      
  ;; [99] PN_PREFIX ::= PN_CHARS_BASE ((PN_CHARS|'.')* PN_CHARS)?
  (pn-prefix (pn-chars-base 
	      "(?:(?:" pn-chars "|\\.)*" pn-chars ")?"))

  ;; [71] PNAME_NS ::= PN_PREFIX? ':'
  (pname-ns (pn-prefix "?:"))
  
  ;; [100] PN_LOCAL ::= ( PN_CHARS_U | [0-9] ) ((PN_CHARS|'.')* PN_CHARS)?
  ;; "Note that SPARQL local names allow leading digits while XML local names do not."
  (pn-local ("(?:" pn-chars-u "|[0-9])"
	     "(?:(?:" pn-chars "|\\.)*" pn-chars ")?"))

  ;; [72] PNAME_LN ::= PNAME_NS PN_LOCAL
  (pname-ln (pname-ns pn-local))

  ;; [73] BLANK_NODE_LABEL ::= '_:' PN_LOCAL
  (blank-node-label ("_:(" pn-local ")")
		    #'blank-node)
  
  ;; [97] VARNAME ::= ( PN_CHARS_U | [0-9] ) 
  ;;                  ( PN_CHARS_U | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040] )*
  (varname ("(?:" pn-chars-u "|[0-9])"
	    "(?:" pn-chars-u "|[0-9]|\\xB7|"
	    #?"[\x{300}-\x{36F}\x{203F}-\x{2040}])*"))

  ;; [74] VAR1 ::= '?' VARNAME
  (var1 ("\\?(" varname ")") #'intern)

  ;; [75] VAR2 ::= '$' VARNAME
  (var2 ("\\$(" varname ")") #'intern)

  ;; [76] LANGTAG ::= '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
  (langtag "@([a-zA-Z]+(?:-[a-zA-z0-9]+)*)")
     
  ;; [91] ECHAR ::= '\' [tbnrf\"']
  (echar "\\\\[tbnrf\\\\\"']")

  ;; [87] STRING_LITERAL1 ::= "'" ( ([^#x27#x5C#xA#xD]) | ECHAR )* "'"
  (string-literal1 ("'((?:[^\\x27\\x5C\\x0A\\x0D]|" echar ")*)'"))

  ;; [88] STRING_LITERAL2 ::= '"' ( ([^#x22#x5C#xA#xD]) | ECHAR )* '"'
  (string-literal2 ("\"((?:[^\\x22\\x5C\\x0A\\x0D]|" echar ")*)\""))

  ;; [89] STRING_LITERAL_LONG1 ::= "'''" ( ( "'" | "''" )? ( [^'\] | ECHAR ) )* "'''"
  (string-literal-long1 ("'''((?:(?:'{1,2})?(?:[^'\\\\]|" echar "))*)'''"))

  ;; [90] STRING_LITERAL_LONG2 ::= '"""' ( ( '"' | '""' )? ( [^"\] | ECHAR ) )* '"""'
  (string-literal-long2 ("\"\"\"((?:(?:\"{1,2})?(?:[^\"\\\\]|" echar "))*)\"\"\""))

  ;; [93] WS ::= #x20 | #x9 | #xD | #xA
  (ws "[\\x20\\x9\\xD\\xA]")

  ;; [94] ANON ::= '[' WS* ']'
  (anon ("\\[" ws "*\\]")
	#'(lambda (match)
	    (declare (ignore match))
	    (blank)))

  ;; [92] NIL ::= '(' WS* ')'
  (k-nil ("\\(" ws "*\\)")
	 (constantly !rdf:nil)))