; astl.scm
; Vedant Kumar <vsk@berkeley.edu>

#lang racket

(define-syntax def
  (syntax-rules ()
    [(_ var . body)
     (define var . body)]))

(define-syntax catch
  (syntax-rules ()
    [(_ (type var) . body)
     (lambda (msg)
       (case msg
         ['catch-var 'var]
         ['catch-type 'type]
         ['catch-body
          (lambda () . body)]
         [else 'error]))]))

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

(define (emit-ast-consumer consumer-class matcher)
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
(format "      if (auto ~a = dyn_cast_or_null<~a>(*i)) {" (matcher 'catch-var) (matcher 'catch-type))
        "        /***/"
        ((matcher 'catch-body))
        "        /***/"
        "      }"
        "    }"
        "  }"
        "};"))

(define (str:scheme->c str)
  (string-titlecase (list->string (filter char-alphabetic? (string->list str)))))

(define (sym:scheme->c sym)
  (str:scheme->c (symbol->string sym)))
  
(define (plugin name desc matcher)
  (emit-header name)
  (emit-boilerplate)
  
  (define consumer-class (string-append "Consumer_" (str:scheme->c name)))
  (emit-ast-consumer consumer-class matcher)
  
  
  'ok)
