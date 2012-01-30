#lang racket

(require "../astl.scm")

(define matcher
  (catch (FunctionDecl fdecl)
    check-func -> (: fdecl getBody)))

(defn check-func (body)
  (walk body
    (DeclStmt check-decl)
    (BinaryOperator check-asgn)
    (CallExpr fail-on-realloc)
    (else check-func)))

(defn check-decl (decl)
  (let* ((sub-cast (cast (child decl) CastExpr))
         (target-buf (get-target-buf sub-cast)))
    (check-for-leak
      (: target-buf getFoundDecl)
      (cast (: decl getSingleDecl) NamedDecl)
      (: target-buf getLocation))))

(defn check-asgn (asgn)
  (: asgn isAssignmentOp)
  (let* ((rhs (cast (: asgn getRHS) CastExpr)) 
         (target-buf (get-target-buf rhs)))
    (check-for-leak
      (: target-buf getFoundDecl) 
      (: (cast (: asgn getLHS) DeclRefExpr) getFoundDecl)
      (: target-buf getLocation))))

(defn fail-on-realloc (call)
  (get-realloc call)
  (errs "On line "
        (line-number (: call getRParenLoc))
        ", the value returned by realloc() may be leaked.\n"))

(defn get-realloc (call)
  (== "realloc"
      (: call getDirectCallee getIdentifier getName))
  call)

(defn get-target-buf (sub-cast)
  (let* ((call (get-realloc (cast (child sub-cast) CallExpr)))
         (wrap-cast (cast (: call => 0) CastExpr))
         (last-cast (cast (child wrap-cast) CastExpr)))
    (cast (child last-cast) DeclRefExpr)))

(defn check-for-leak (lhs rhs loc)
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
