/*
 * ReallocVerifier.cpp
 *
 * Vedant Kumar <vsk@berkeley.edu>
 */

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendPluginRegistry.h"

using namespace std;
using namespace llvm;
using namespace clang;

class ReallocVerifierConsumer : public ASTConsumer {
public:
	virtual void Initialize(ASTContext& Context) {
		sm = &Context.getSourceManager();
	}

	virtual void HandleTopLevelDecl(DeclGroupRef DG) {
		for (auto i=DG.begin(), e=DG.end(); i != e; ++i) {
			if (auto fdecl = dyn_cast_or_null<FunctionDecl>(*i)) {
				if (Stmt* s_tree = fdecl->getBody()) {
					check_function(s_tree);
				}
			}
		}
	}

private:
	SourceManager* sm;

	void check_function(Stmt* s_tree) {
		auto it = s_tree->child_begin();
		for (auto end = s_tree->child_end(); it != end; ++it) {
			if (auto asgn = dyn_cast_or_null<BinaryOperator>(*it)) {
				check_assignment(asgn);
			} else if (auto vdecl = dyn_cast_or_null<DeclStmt>(*it))
			{
				check_declaration(vdecl);
			} else if (auto call = extract_realloc(*it)) {
				FullSourceLoc floc(call->getRParenLoc(), *sm);
				errs()	<< "On line "
					<< floc.getExpansionLineNumber()
					<< ", the value returned by realloc() "
					<< "may be leaked.\n";
			} else if (*it) {
				check_function(*it);
			}
		}
	}

	void check_assignment(BinaryOperator* asgn) {
		CastExpr* rhs = NULL;
		DeclRefExpr *target = NULL, *lhs = NULL;
		if (asgn->isAssignmentOp()
		    && (rhs = dyn_cast_or_null<CastExpr>(asgn->getRHS()))
		    && (target = extract_target(rhs))
		    && (lhs = dyn_cast_or_null<DeclRefExpr>(asgn->getLHS())))
		{
			compare_vars(lhs->getFoundDecl(),
				     target->getFoundDecl(),
				     target->getLocation());
		}
	}

	void check_declaration(DeclStmt* vdecl) {
		NamedDecl* ndecl = NULL;
		CastExpr* castexpr = NULL;
		DeclRefExpr* target = NULL;
		auto it = vdecl->child_begin();
		if ((castexpr = dyn_cast_or_null<CastExpr>(*it))
		    && (target = extract_target(castexpr))
		    && (ndecl = dyn_cast_or_null<NamedDecl>
				(vdecl->getSingleDecl())))
		{
			compare_vars(ndecl,
				     target->getFoundDecl(),
				     target->getLocation());
		}
	}

	CallExpr* extract_realloc(Stmt* s_tree) {
		CallExpr* call = NULL;
		if ((call = dyn_cast_or_null<CallExpr>(s_tree))
		    && ("realloc" ==
			call->getDirectCallee()->getIdentifier()->getName()))
		{
			return call;
		}
		return NULL;
	}

	DeclRefExpr* extract_target(CastExpr* expr) {
		CastExpr* sub_cast = NULL;
		DeclRefExpr* target = NULL;
		auto it = expr->child_begin();
		if (auto call = extract_realloc(*it)) {
			auto buf = call->getArg(0);
			if ((sub_cast = dyn_cast_or_null<CastExpr>(buf))) {
				auto cast_iter = sub_cast->child_begin();
				if ((sub_cast = dyn_cast<CastExpr>(*cast_iter)))
				{
					cast_iter = sub_cast->child_begin();
					if ((target = dyn_cast<DeclRefExpr>
						      (*cast_iter)))
					{
						return target;
					}
				}
			}
		}
		return NULL;
	}

	void compare_vars(NamedDecl* lhs, NamedDecl* target, SourceLocation loc)
	{
		if (lhs->getQualifiedNameAsString() ==
		    target->getQualifiedNameAsString())
		{
			FullSourceLoc floc(loc, *sm);
			errs()  << "On line " << floc.getExpansionLineNumber()
				<< ", the original contents of "
				<< lhs->getQualifiedNameAsString()
				<< " may be leaked.\n";
		}
	}
};

class ReallocVerifierAction: public PluginASTAction {
protected:
	ASTConsumer* CreateASTConsumer(CompilerInstance&, StringRef)
	{
		return new ReallocVerifierConsumer();
	}

	bool ParseArgs(const CompilerInstance&, const vector<string>&) {
		return true;
	}
};

static FrontendPluginRegistry::Add<ReallocVerifierAction>
X("realloc-verifier", "Check calls to realloc() for leaks.");
