name: realloc-verifier
desc: Check calls to realloc() for leaks.

# child: <node> -> ...
# matching: <node>{<field> = <val>, ...}
# field access: <node> . <field>
# multiple children: <1>, ..., <n>
# advanced block: <node> => <name> { ... }
# adv. block assign: <name> := <expr>
# adv. block checks: <expr> <op> <expr>
# adv. block casting: [<expr> :: Type]

# store fragments of patterns
# combine pattern fragments (||, &&)

# === Parsing Overview ===
# - Look for the required name and desc tags.
# - Read patterns, assemble them into a meaningful structure.
# 	(Note: there is only one main pattern that gets matched.)
# - Generate an ASTConsumer that visits top-level declarations, hoping to match the beginning of the main pattern. 
# - The first item in the pattern to be matched will be some subclass of Decl. The "|>" notation should get some n-ary tree like representation of the data. Something that supports iterating down the AST recursively.
# - For FunctionDecl, "|>" would map to getBody()[Stmt*]. 
# - It may be necessary to also handle Expr* trees? (Really?)
# - The syntax for navigating a Stmt tree is "->". Specifically, the token means "try matching on all the children of the parent stmt, sequentially."
# - The algorithm that does the matching stops as soon as one of the checks in the patterns failed. If all checks have passed, it stops traversal and indicates that the flaw was found.
# - There needs to some more sophisticated error reporting mechanism.
# - The algorithm should never generate code that can segfault. Every time we do a dyn_cast or extract a pointer out of some parent container, do a NULL check. If it fails, then clearly the rest of the pattern is going to match, and you can say there is no error.
# Some functions like fcall.getArg(N) are trickier. As long as llvm is compiled with asserts enabled, there will be acceptable error messages.
# - Comments. #.

FunctionDecl |>
    BinaryOperator => asgn {
        asgn.getOpcode() == BO_Assign
    } ->
        DeclRefExpr => lhsBuf,
        CastExpr ->
            CallExpr => fcall {
                fdecl := fcall.getDirectCallee()
                fdecl.getIdentifier().getName() == "realloc"
                targetBuf := [fcall.getArg(0) :: DeclRefExpr]
                lhsBuf.getName() == targetBuf.getName()
            }

FunctionDecl -> DeclStmt -> CastExpr -> CallExpr

# === Alternate approach ===
# Build a DSL within C++.

auto asgn = Stmt<BinaryOperator>
