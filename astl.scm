; astl.scm
; Vedant Kumar <vsk@berkeley.edu>

#lang racket

(provide def catch :)

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
          (lambda (var) . body)]))]))

(define-syntax :
  (syntax-rules (=>)
    [(_ obj field)
     (let ((var (genvar)))
       (emit (format "auto ~a = ~a->~a;" var obj field)
             (format "if (NULL == ~a) { return NULL; }" var))
       var)]
    [(_ call => idx)
     (let ((var (genvar)))
       (emit (format "auto ~a = ~a->getArg(~a);" var call idx)
             (format "if (NULL == ~a) { return NULL; }" var))
       var)]
    [(_ obj field ...)
     (foldl (lambda (field var)
              (: var field))
            obj
            (list field ...))]))

(define (emit . lines)
  (map displayln lines))

(define (emit-header name)
  (emit "/*"
(format " * ~a" name)
        " */"
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
        ((matcher 'catch-body) (matcher 'catch-var))
        "        /***/"
        "      }"
        "    }"
        "  }"
        "};"))

(define (str:scheme->c str)
  (string-titlecase (list->string (filter char-alphabetic? (string->list str)))))

(define (sym:scheme->c sym)
  (str:scheme->c (symbol->string sym)))

(define (genvar)
  (sym:scheme->c (gensym)))
  
(define (plugin name desc matcher)
  (emit-header name)
  (emit-boilerplate)
  (define consumer-class
    (string-append "Consumer_" (str:scheme->c name)))
  (emit-ast-consumer consumer-class matcher)
  
  
  'ok)
