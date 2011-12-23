(load "astl.scm")

(def matcher
  (catch (FunctionDecl fdecl)
    (check-func (: fdecl getBody))))

(def (check-func body)
  (walk body
    (DeclStmt check-decl)
    (BinaryOperator check-asgn)
    (CallExpr fail-on-realloc)
    (else check-func)))

(def (check-decl decl)
  (let* ((sub-cast (cast (child decl) CastExpr))
  	 (target-buf (get-target-buf sub-cast)))
    (check-for-leak
      (: target-buf getFoundDecl)
      (cast (: decl getSingleDecl) NamedDecl) 
      (: target-buf getLocation))))

(def (check-asgn asgn)
  (: asgn isAssignmentOp)
  (let* ((rhs (cast (: asgn getRHS) CastExpr)) 
  	 (target-buf (get-target-buf rhs)))
    (check-for-leak
      (: target-buf getFoundDecl) 
      (: (cast (: asgn getLHS) DeclRefExpr) getFoundDecl)
      (: target-buf getLocation))))

(def (fail-on-realloc call)
  (if (get-realloc call)
    (errs "On line "
    	  (line-number (: call getRParenLoc))
    	  ", the value returned by realloc() may be leaked.\n")))

(def (get-realloc call)
  (== "realloc"
      (: call getDirectCallee getIdentifier getName))
  call)

(def (get-target-buf sub-cast)
  (let* ((call (get-realloc (cast (child sub-cast) CallExpr)))
  	 (wrap-cast (cast (: 0 => call getArg) CastExpr))
         (last-cast (cast (child wrap-cast) CastExpr)))
    (cast (child last-cast) DeclRefExpr)))

(def (check-for-leak lhs rhs loc)
  (== (: lhs getQualifiedNameAsString)
      (: rhs getQualifiedNameAsString))
  (errs "On line "
  	(line-number loc)
  	", the original contents of "
  	(: lhs getQualifiedNameAsString)
  	" may be leaked.\n"))

(plugin "realloc-verifier"
	"Check calls to realloc() for leaks."
	matcher)
