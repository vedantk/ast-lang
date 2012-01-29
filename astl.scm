; astl.scm
; Vedant Kumar <vsk@berkeley.edu>

#lang racket

(define-syntax def
  (syntax-rules ()
    [(_ var . body)
     (define var . body)]))

(define-syntax def-storage
  (syntax-rules ()
    [(_ name (arg ...))
     (define name
       (lambda (arg ...)
         (let ((table (make-hash)))
           (hash-set! table (quote arg) arg)
           ...
           (lambda (msg)
              (hash-ref table msg)))))]))

(def-storage :> (match-type type-dispatcher))

(define-syntax catch
  (syntax-rules ()
    [(_ (type var-name) . body)
     (lambda (
        
     

(define (emit . lines)
  (map displayln lines))

(define (emit-header name)
  (emit "/*"
        (string-append " * " name)
        "*/"
        ""))

(define (emit-boilerplate)
  (emit "#include \"clang/AST/AST.h\""
        "#include \"clang/AST/ASTConsumer.h\""
        "#include \"clang/Frontend/CompilerInstance.h\""
        "#include \"clang/Frontend/FrontendPluginRegistry.h\""
        ""
        "using namespace std;"
        "using namespace llvm;"
        "using namespace clang;"))

(define (emit-ast-consumer consumer-class)
  (emit (string-append "class " consumer-class " : public ASTConsumer {")
        "private:"
        "  SourceManager* sm;"
        ""
        "public:"
        "  virtual void Initialize(ASTContext& Context) {"
        "    sm = &Context.getSourceManager();"
        "  }"
        ""
        "  virtual void HandleTopLevelDecl(DeclGroupRef DG) {"
        "    for (auto i=DG.begin(), e=DG.end(); i != e; ++i) {"
        "
        "};"))
        



(define (plugin name desc matcher)
  (emit-header name)
  (emit-boilerplate)
  (define consumer-class "MyConsumer")
  
  'ok)
