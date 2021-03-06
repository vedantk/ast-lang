; astl.scm
; Vedant Kumar <vsk@berkeley.edu>

#lang racket

(provide defn
         catch
         :
         walk
         cast
         child
         errs
         line-number
         ==
         plugin)

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
        "using namespace clang;"
        ""))

(emit-boilerplate)

(define-syntax defn
  (syntax-rules ()
    [(_ name args . body)

       #|(let ((func-name (sym:scheme->c 'name))
             (func-args (map genvar 'args)))
         (emit (format "void* ~a(~a) {" func-name func-args)
               "  #define BAD_RETURN() { return NULL; }"
               
               "  #undef BAD_RETURN"
               "}"
               "")|#
       (apply (lambda args . body) (map genvar 'args))
       (define name
         (lambda args
           (format "~a(~a);" (sym:scheme->c 'name) (string-join args ", ")))))]))

(define-syntax catch
  (syntax-rules (->)
    [(_ (type var) func -> arg)
     (lambda (msg)
       (case msg
         ['catch-var 'var]
         ['catch-type 'type]
         ['catch-body
          (lambda ()
            (let ((obj ((lambda (var) arg) 'var)))
              (printf "~a(~a);~n" (sym:scheme->c 'func) obj)))]))]))

(define-syntax :
  (syntax-rules (=> >>)
    [(_ obj field)
     (let ((var (genvar)))
       (emit (format "auto ~a = ~a->~a();" var obj 'field)
             (format "if (NULL == ~a) { BAD_RETURN(); }" var))
       var)]
    [(_ call => idx)
     (let ((var (genvar)))
       (emit (format "auto ~a = ~a->getArg(~a);" var 'call idx)
             (format "if (NULL == ~a) { BAD_RETURN(); }" var))
       var)]
    [(_ obj . fields)
     (foldl (lambda (field var)
              (: var field))
            obj 'fields)]))

; XXX
(define-syntax walk
  (syntax-rules ()
    [(_ body (type dispatcher) ...)
     ; (emit "...")
     (emit (format "auto it = ~a->child_begin();" body)
           (format "auto stop = ~a->child_end();" body)
           "for (; it != stop; ++it) {"
           "  ..."
           "}")]))

(define-syntax cast
  (syntax-rules ()
    [(_ arg type)
     (let ((var (genvar)))
       (emit (format "auto ~a = dyn_cast_or_null<~a>(~a);" var 'type arg)
             (format "if (NULL == ~a) { BAD_RETURN(); }" var))
       var)]))

(define-syntax child
  (syntax-rules ()
    [(_ arg)
     (let ((var (genvar)))
       (emit (format "auto ~a = *(~a->child_begin());" var arg)
             (format "if (NULL == ~a) { BAD_RETURN(); }" var))
       var)]))

(define-syntax errs
  (syntax-rules ()
    [(_ . args)
     (emit args)]))

(define-syntax line-number
  (syntax-rules ()
    [(_ loc)
     (let ((var (genvar)))
       (emit (format "~a = ~a.getExpansionLineNumber();" var loc))
       var)]))

(define-syntax ==
  (syntax-rules ()
    [(_ lhs rhs)
     (emit (format "if (~a != ~a) { BAD_RETURN(); }" lhs rhs))]))

(define (emit-ast-consumer consumer-class matcher)
  (emit (format "class ~a : public ASTConsumer {" consumer-class)
        "private:"
        "  SourceManager* sm;"
        ""
        "public:"
        "  virtual void Initialize(ASTContext& Context) {"
        "    sm = &Context.getSourceManager();"
        "  }"
        ""
        "  virtual void HandleTopLevelDecl(DeclGroupRef DG) {"
        "    #define BADRETURN { return; }"
        "    for (auto i=DG.begin(), e=DG.end(); i != e; ++i) {"
(format "      if (auto ~a = dyn_cast_or_null<~a>(*i)) {" (matcher 'catch-var) (matcher 'catch-type))
        "        /***/")
  ((matcher 'catch-body))
  (emit "        /***/"
        "      }"
        "    }"
        "    #undef BADRETURN"
        "  }"
        "};"
        ""))

(define (emit-action action-class consumer-class)
  (emit (format "class ~a : public PuglinASTAction {" action-class)
        "protected:"
        "  ASTConsumer* CreateASTConsumer(CompilerInstance&, StringRef) {"
(format "    return new ~a();" consumer-class)
        "  }"
        ""
        "  bool ParseArgs(const CompilerInstance&, const vector<string>&) {"
        "    return true;"
        "  }"
        "};"
        ""))

(define (emit-registration action-class name desc)
  (emit (format "static FrontendPluginRegistry::Add<~a>" action-class)
        (format "X(\"~a\", \"~a\");" name desc)
        ""))

(define (str:scheme->c str)
  (string-titlecase (list->string
    (filter (lambda (c) (or (char-alphabetic? c) (char-numeric? c)))
            (string->list str)))))

(define (sym:scheme->c sym)
  (str:scheme->c (symbol->string sym)))

(define (genvar . _)
  (sym:scheme->c (gensym)))
  
(define (plugin name desc matcher)
  (emit-header name)
  (define name-prefix (str:scheme->c name))
  (define consumer-class (string-append "Consumer_" name-prefix))
  (emit-ast-consumer consumer-class matcher)
  (define action-class (string-append "Action_" name-prefix))
  (emit-action action-class consumer-class)
  (emit-registration action-class name desc))