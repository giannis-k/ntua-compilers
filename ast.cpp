#include <memory>
#include <deque>
#include <unordered_map>
#include <utility>

#include "ast.hpp"

//the implementation of compileBlock and compileScope is based on the one found here: https://git.softlab.ntua.gr/spirusdn/Alan-Compiler-CXX

llvm::Type *LLVM_Type(std::shared_ptr<Type> type, PassMode mode);

class compileBlock {
public:
	compileBlock()
	{
		fun = nullptr;
		curBlock = nullptr;
		ret = false;
	}

	void setFun(llvm::Function *func)
	{
		fun = func;
	}

	llvm::Function* getFun()
	{
		return fun;
	}

	void setBlock(llvm::BasicBlock *b)
	{
		curBlock = b;
		ret = false;
	}

	llvm::BasicBlock* getBlock()
	{
		return curBlock;
	}

	void addParameter(std::string id, std::shared_ptr<Type> type, PassMode mode)
	{
		llvm::Type *t = LLVM_Type(type, mode);
		parameters.push_back(t);
		vars[id] = std::make_pair(t, mode);
	}

	void addVar(std::string id, std::shared_ptr<Type> type, PassMode mode)
	{
		vars[id] = std::make_pair(LLVM_Type(type, mode), mode);
	}

	llvm::Type* getVar(std::string id)
	{
		return vars[id].first;
	}

	void addAlloc(std::string id, llvm::AllocaInst *val)
	{
		allocs[id] = val;
	}

	llvm::AllocaInst* getAlloc(std::string id)
	{
		return allocs[id];
	}

	void addAddr(std::string id, llvm::AllocaInst *addr)
	{
		addrs[id] = addr;
	}

	llvm::AllocaInst* getAddr(std::string id)
	{
		return addrs[id];
	}

	void addRet()
	{
		ret = true;
	}

	bool hasRet()
	{
		return ret;
	}

	const std::vector<llvm::Type*>& getParams() const
	{
		return parameters;
	}

	bool isRef(std::string id)
	{
		return vars[id].second == REF;
	}



private:
	llvm::Function *fun;
	std::vector<llvm::Type*> parameters;
	std::unordered_map<std::string, std::pair<llvm::Type*, PassMode>> vars;
	std::unordered_map<std::string, llvm::AllocaInst*> allocs, addrs;
	llvm::BasicBlock *curBlock;
	bool ret;
};


class compileScope {
public:
	compileScope(){}

	void open()
	{
		funcs.push_front(std::unordered_map<std::string, llvm::Function*>());
	}

	void close()
	{
		if(funcs.empty()) return;
		funcs.pop_front();
	}

	void addFun(std::string id, llvm::Function *fun)
	{
		funcs.front()[id] = fun;
	}

	llvm::Function* getFun(std::string id)
	{
		for(auto it=funcs.begin(); it!=funcs.end(); it++)
		{
			if((*it).find(id)!=(*it).end())
				return (*it)[id];
		}
		return nullptr;
	}

private:
	std::deque<std::unordered_map<std::string, llvm::Function*>> funcs;
};

static std::deque<std::shared_ptr<compileBlock>> blocks;
static compileScope scopes;

std::string postfix = "7";

llvm::LLVMContext AST::TheContext;
llvm::IRBuilder<> AST::Builder(TheContext);
std::unique_ptr<llvm::Module> AST::TheModule;
std::unique_ptr<llvm::legacy::FunctionPassManager> AST::TheFPM;

llvm::Function *AST::ThePuti;
llvm::Function *AST::ThePutb;
llvm::Function *AST::ThePutc;
llvm::Function *AST::ThePuts;
llvm::Function *AST::TheGeti;
llvm::Function *AST::TheGetb;
llvm::Function *AST::TheGetc;
llvm::Function *AST::TheGets;
llvm::Function *AST::TheAbs;
llvm::Function *AST::TheOrd;
llvm::Function *AST::TheChr;
llvm::Function *AST::TheStrlen;
llvm::Function *AST::TheStrcmp;
llvm::Function *AST::TheStrcpy;
llvm::Function *AST::TheStrcat;
llvm::Function *AST::TheInit;
llvm::Function *AST::TheMalloc;

llvm::Type *AST::i1 = llvm::IntegerType::get(TheContext, 1);
llvm::Type *AST::i8 = llvm::IntegerType::get(TheContext, 8);
llvm::Type *AST::i16 = llvm::IntegerType::get(TheContext, 16);
llvm::Type *AST::i32 = llvm::IntegerType::get(TheContext, 32);
llvm::Type *AST::i64 = llvm::IntegerType::get(TheContext, 64);
llvm::PointerType *AST::List_t;

llvm::Type *LLVM_Type(std::shared_ptr<Type> type, PassMode mode)
{
	llvm::Type *t;
	switch (type->getType())
	{
		case INT:
			t = llvm::Type::getInt16Ty(AST::TheContext);
			break;
		case CHAR:
			t = llvm::Type::getInt8Ty(AST::TheContext);
			break;
		case BOOL:
			t = llvm::Type::getInt8Ty(AST::TheContext);
			break;
		case VOID:
			t = llvm::Type::getVoidTy(AST::TheContext);
			break;
		case ARRAY:
			t = llvm::PointerType::get(LLVM_Type(type->refType(), VAL), 0);
			break;
		case LIST:
			t = AST::List_t;
			break;
	}
	if(mode==REF && type->getType()!=ARRAY && type->getType()!=LIST)
		t=llvm::PointerType::getUnqual(t);
	return t;
}

void AST::begin_compilation(bool opt)
{
	TheModule = llvm::make_unique<llvm::Module>("My Tony program", TheContext);
	TheFPM = llvm::make_unique<llvm::legacy::FunctionPassManager>(TheModule.get());
	if (opt)
	{
		TheFPM->add(llvm::createPromoteMemoryToRegisterPass());
		TheFPM->add(llvm::createInstructionCombiningPass());
		TheFPM->add(llvm::createReassociatePass());
		TheFPM->add(llvm::createGVNPass());
		TheFPM->add(llvm::createCFGSimplificationPass());
	}
	TheFPM->doInitialization();

	llvm::StructType *NodeType = llvm::StructType::create(TheContext, "nodetype");
	NodeType->setBody({i64, llvm::PointerType::get(NodeType, 0)});
	List_t = llvm::PointerType::get(NodeType, 0);

	scopes.open();
	llvm::FunctionType* puti_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i16}, false);
	ThePuti = llvm::Function::Create(puti_type, llvm::Function::ExternalLinkage, "_puti", TheModule.get());
	scopes.addFun("puti", ThePuti);
	llvm::FunctionType* putb_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i8}, false);
	ThePutb = llvm::Function::Create(putb_type, llvm::Function::ExternalLinkage, "_putb", TheModule.get());
	scopes.addFun("putb", ThePutb);
	llvm::FunctionType* putc_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i8}, false);
	ThePutc = llvm::Function::Create(putc_type, llvm::Function::ExternalLinkage, "_putc", TheModule.get());
	scopes.addFun("putc", ThePutc);
	llvm::FunctionType* puts_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {llvm::PointerType::get(i8, 0)}, false);
	ThePuts = llvm::Function::Create(puts_type, llvm::Function::ExternalLinkage, "_puts", TheModule.get());
	scopes.addFun("puts", ThePuts);
	llvm::FunctionType* geti_type = llvm::FunctionType::get(llvm::Type::getInt16Ty(TheContext), {}, false);
	TheGeti = llvm::Function::Create(geti_type, llvm::Function::ExternalLinkage, "_geti", TheModule.get());
	scopes.addFun("geti", TheGeti);
	llvm::FunctionType* getb_type = llvm::FunctionType::get(llvm::Type::getInt8Ty(TheContext), {}, false);
	TheGetb = llvm::Function::Create(getb_type, llvm::Function::ExternalLinkage, "_getb", TheModule.get());
	scopes.addFun("getb", TheGetb);
	llvm::FunctionType* getc_type = llvm::FunctionType::get(llvm::Type::getInt8Ty(TheContext), {}, false);
	TheGetc = llvm::Function::Create(getc_type, llvm::Function::ExternalLinkage, "_getc", TheModule.get());
	scopes.addFun("getc", TheGetc);
	llvm::FunctionType* gets_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i16, llvm::PointerType::get(i8, 0)}, false);
	TheGets = llvm::Function::Create(gets_type, llvm::Function::ExternalLinkage, "_gets", TheModule.get());
	scopes.addFun("gets", TheGets);
	llvm::FunctionType* abs_type = llvm::FunctionType::get(llvm::Type::getInt16Ty(TheContext), {i16}, false);
	TheAbs = llvm::Function::Create(abs_type, llvm::Function::ExternalLinkage, "_abs", TheModule.get());
	scopes.addFun("abs", TheAbs);
	llvm::FunctionType* ord_type = llvm::FunctionType::get(llvm::Type::getInt16Ty(TheContext), {i8}, false);
	TheOrd = llvm::Function::Create(ord_type, llvm::Function::ExternalLinkage, "_ord", TheModule.get());
	scopes.addFun("ord", TheOrd);
	llvm::FunctionType* chr_type = llvm::FunctionType::get(llvm::Type::getInt8Ty(TheContext), {i16}, false);
	TheChr = llvm::Function::Create(chr_type, llvm::Function::ExternalLinkage, "_chr", TheModule.get());
	scopes.addFun("chr", TheChr);
	llvm::FunctionType* strlen_type = llvm::FunctionType::get(llvm::Type::getInt16Ty(TheContext), {llvm::PointerType::getUnqual(i8)}, false);
	TheStrlen = llvm::Function::Create(strlen_type, llvm::Function::ExternalLinkage, "_strlen", TheModule.get());
	scopes.addFun("strlen", TheStrlen);
	llvm::FunctionType* strcmp_type = llvm::FunctionType::get(llvm::Type::getInt16Ty(TheContext), {llvm::PointerType::getUnqual(i8), llvm::PointerType::getUnqual(i8)}, false);
	TheStrcmp = llvm::Function::Create(strcmp_type, llvm::Function::ExternalLinkage, "_strcmp", TheModule.get());
	scopes.addFun("strcmp", TheStrcmp);
	llvm::FunctionType* strcpy_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {llvm::PointerType::getUnqual(i8), llvm::PointerType::getUnqual(i8)}, false);
	TheStrcpy = llvm::Function::Create(strcpy_type, llvm::Function::ExternalLinkage, "_strcpy", TheModule.get());
	scopes.addFun("strcpy", TheStrcpy);
	llvm::FunctionType* strcat_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {llvm::PointerType::getUnqual(i8), llvm::PointerType::getUnqual(i8)}, false);
	TheStrcat = llvm::Function::Create(strcat_type, llvm::Function::ExternalLinkage, "_strcat", TheModule.get());
	scopes.addFun("strcat", TheStrcat);
	llvm::FunctionType* malloc_type = llvm::FunctionType::get(llvm::PointerType::get(i8, 0), {i64}, false);
	TheMalloc = llvm::Function::Create(malloc_type, llvm::Function::ExternalLinkage, "GC_malloc", TheModule.get());
	llvm::FunctionType* init_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {}, false);
	TheInit = llvm::Function::Create(init_type, llvm::Function::ExternalLinkage, "GC_init", TheModule.get());

	llvm::Value* program = compile();
	llvm::Function *program_main = TheModule->getFunction("main");
	llvm::Value* main_fun = TheModule->getFunction(program->getName());
	if(program_main!=nullptr)
		program_main->setName("__main__");
	llvm::FunctionType* main_type = llvm::FunctionType::get(i32, {}, false);
	llvm::Function* TheMain = llvm::Function::Create(main_type, llvm::Function::ExternalLinkage, "main", TheModule.get());
	llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", TheMain);
	Builder.SetInsertPoint(BB);
	Builder.CreateCall(TheInit, {});
	Builder.CreateCall(main_fun, {});
	Builder.CreateRet(c32(0));

	if(llvm::verifyModule(*TheModule, &llvm::errs()))
	{
		std::cerr<<"The IR is bad!\n";
		TheModule->print(llvm::errs(), nullptr);
		exit(1);
	}

	TheFPM->run(*TheMain);
	TheModule->print(llvm::outs(), nullptr);
}

llvm::Value* Int::compile()
{
	return c16(value);
}

llvm::Value* Char::compile()
{
	return c8(value);
}

llvm::Value* Bool::compile()
{
	return c8(value);
}

llvm::Value* String::compile()
{
	std::vector<llvm::Constant*> chars;
	for(char c: value)
		chars.push_back(c8(c));
	chars.push_back(c8('\0'));
	llvm::ArrayType *str_t = llvm::ArrayType::get(i8, chars.size());
	llvm::GlobalVariable *my_string = new llvm::GlobalVariable(*TheModule, str_t, true, llvm::GlobalValue::InternalLinkage, llvm::ConstantArray::get(str_t, chars), "string");
	return llvm::GetElementPtrInst::CreateInBounds(str_t, my_string, {c32(0), c32(0)}, "string_pointer", Builder.GetInsertBlock());
}

llvm::Value* Var::compile()
{
	std::shared_ptr<Type> j(new ArrayType(nullptr));
	std::shared_ptr<Type> k(new ListType(nullptr));
	if(blocks.front()->isRef(id))
	{
		if(llvm::PointerType::classof(blocks.front()->getAddr(id)->getType()) && !equals(t, j, false) && !equals(t, k, false))
			return Builder.CreateLoad(blocks.front()->getAddr(id));
		else
			return blocks.front()->getAddr(id);
	}
	else
		return blocks.front()->getAlloc(id);
}

llvm::Value* Arr::compile()
{
	llvm::Value* arr = atom->compile();
	llvm::Value* idx = pos->compile();
	if(llvm::PointerType::classof(idx->getType()))
		idx = Builder.CreateLoad(idx, "idx");
	llvm::Value* index = Builder.CreateSExt(idx, i32, "idx32");
	llvm::Value* val;
	if(atom->isLval())
		val = Builder.CreateLoad(arr, "val");
	else
		val = arr;
	return Builder.CreateInBoundsGEP(val, index, "arr");
}

llvm::Value* VarDef::compile()
{
	llvm::Type* ty = LLVM_Type(t, VAL);
	for(auto it=var_names.begin(); it!=var_names.end(); it++)
	{
		blocks.front()->addVar(*it, t, VAL);
		blocks.front()->addAlloc(*it, Builder.CreateAlloca(ty, nullptr, *it));
	}
	return nullptr;
}

llvm::Value* Nil::compile()
{
	return llvm::ConstantPointerNull::get(List_t);
}

llvm::Value* BinOp::compile()
{
	llvm::Value* left = lhs->compile();
	llvm::Value* right = rhs->compile();
	if(lhs->isLval() && llvm::PointerType::classof(left->getType()))
		left = Builder.CreateLoad(left, "left");
	if(rhs->isLval() && llvm::PointerType::classof(right->getType()))
		right = Builder.CreateLoad(right, "right");
	llvm::Value* val;
	switch(op)
	{
		case PLUS:
			return Builder.CreateAdd(left, right, "plus");
		case MINUS:
			return Builder.CreateSub(left, right, "minus");
		case MUL:
			return Builder.CreateMul(left, right, "mul");
		case DIV:
			return Builder.CreateSDiv(left, right, "div");
		case MOD:
			return Builder.CreateSRem(left, right, "mod");
		case EQ:
			val = Builder.CreateICmpEQ(left, right, "eq");
			return Builder.CreateZExt(val, i8, "bool_val");
		case NOT_EQ:
			val = Builder.CreateICmpNE(left, right, "not_eq");
			return Builder.CreateZExt(val, i8, "bool_val");
		case LESS:
			val = Builder.CreateICmpSLT(left, right, "less");
			return Builder.CreateZExt(val, i8, "bool_val");
		case GREATER:
			val = Builder.CreateICmpSGT(left, right, "greater");
			return Builder.CreateZExt(val, i8, "bool_val");
		case LESS_EQ:
			val = Builder.CreateICmpSLE(left, right, "less_eq");
			return Builder.CreateZExt(val, i8, "bool_val");
		case GREATER_EQ:
			val = Builder.CreateICmpSGE(left, right, "greater_eq");
			return Builder.CreateZExt(val, i8, "bool_val");
		case AND:
			val = Builder.CreateAnd(left, right, "and");
			return Builder.CreateZExt(val, i8, "bool_val");
		case OR:
			val = Builder.CreateOr(left, right, "or");
			return Builder.CreateZExt(val, i8, "bool_val");
		case CONS:
			llvm::Value* n = Builder.CreateCall(TheMalloc, {c64(16)}, "malloc");
			llvm::Value* node = Builder.CreateBitCast(n, List_t, "node");
			llvm::Value* head = Builder.CreateGEP(node, {c32(0), c32(0)}, "head");
			llvm::Value* val;
			if(llvm::PointerType::classof(left->getType()))
				val = Builder.CreatePtrToInt(left, i64, "val");
			else
				val = Builder.CreateSExt(left, i64, "val");
			Builder.CreateStore(val, head);
			llvm::Value* tail = Builder.CreateGEP(node, {c32(0), c32(1)}, "tail");
			Builder.CreateStore(right, tail);
			return node;
	}
}

llvm::Value* UnOp::compile()
{
	llvm::Value* right = rhs->compile();
	if(rhs->isLval() && llvm::PointerType::classof(right->getType()))
		right = Builder.CreateLoad(right, "right");
	llvm::Value* val;
	switch(op)
	{
		case UPLUS:
			return right;
		case UMINUS:
			return Builder.CreateNeg(right, "uminus");
		case NOT:
			val = Builder.CreateICmpEQ(right, c8(0), "eq");
			return Builder.CreateZExt(val, i8, "not");
		case NILQ:
			val = Builder.CreatePtrToInt(right, i64, "list");
			val = Builder.CreateICmpEQ(val, c64(0), "is_nil");
			return Builder.CreateZExt(val, i8, "nilq");
		case HEAD:
			val = Builder.CreateGEP(right, {c32(0), c32(0)}, "head_ptr");
			val = Builder.CreateLoad(val, "head");
			if(llvm::PointerType::classof(LLVM_Type(rhs->getType()->refType(), VAL)))
				return Builder.CreateIntToPtr(val, LLVM_Type(rhs->getType()->refType(), VAL), "head_val");
			else
				return Builder.CreateTrunc(val, LLVM_Type(rhs->getType()->refType(), VAL), "head_val");
		case TAIL:
			val = Builder.CreateGEP(right, {c32(0), c32(1)}, "tail_ptr");
			return Builder.CreateLoad(val, "tail");
	}
}

llvm::Value* New::compile()
{
	llvm::Value* s = size->compile();
	if(llvm::PointerType::classof(s->getType()))
		s = Builder.CreateLoad(s, "size");
	llvm::Value* mem = Builder.CreateMul(s, c16(t->refType()->getSize()), "mem");
	llvm::Value* val = Builder.CreateCall(TheMalloc, {Builder.CreateSExt(mem, i64, "mem_ext")}, "malloc");
	return Builder.CreateBitCast(val, llvm::PointerType::getUnqual(LLVM_Type(t->refType(), VAL)), "arr_addr");
}

llvm::Value* Assign::compile()
{
	// if(Builder.GetInsertBlock()->getTerminator())
	// 	return nullptr;
	llvm::Value* left = lhs->compile();
	llvm::Value* right = rhs->compile();
	if(rhs->isLval() && llvm::PointerType::classof(right->getType()))
		right = Builder.CreateLoad(right, "right");
	return Builder.CreateStore(right, left);
}

llvm::Value* Skip::compile()
{
	return nullptr;
}

llvm::Value* Call::compile()
{
	std::shared_ptr<Type> j(new ArrayType(nullptr));
	std::shared_ptr<Type> k(new ListType(nullptr));
	llvm::Function* TheFunction = scopes.getFun(id);
	std::vector<llvm::Value*> Args;
	std::vector<std::shared_ptr<Entry>> p = fun->getParameters();
	for(unsigned i=0; i<parameters.size(); ++i)
	{
		if((p[i]->getPassMode()==REF && !equals(parameters[i]->getType(), j, false) && !equals(parameters[i]->getType(), k, false)) || parameters[i]->isString())
			Args.push_back(parameters[i]->compile());
		else if(parameters[i]->isLval())
		{
			llvm::Value* par = parameters[i]->compile();
			if(llvm::PointerType::classof(par->getType()))
				par = Builder.CreateLoad(par, "par");
			Args.push_back(par);
		}
		else
			Args.push_back(parameters[i]->compile());
	}

	std::vector<std::shared_ptr<Entry>> pr = fun->getPrevScopeVars();
	for(unsigned i=0; i<pr.size(); ++i)
	{
		AST* tmp = new Var(pr[i]->getId());
		tmp->setType(pr[i]->getType());
		if(!equals(pr[i]->getType(), j, false) && !equals(pr[i]->getType(), k, false))
			Args.push_back(tmp->compile());
		else
		{
			llvm::Value* par = tmp->compile();
			if(llvm::PointerType::classof(par->getType()))
				par = Builder.CreateLoad(par, "par");
			Args.push_back(par);
		}
	}

	return Builder.CreateCall(TheFunction, Args);;
}

llvm::Value* Exit::compile()
{
	// if(Builder.GetInsertBlock()->getTerminator())
	// 	return nullptr;
	blocks.front()->addRet();
	return Builder.CreateRetVoid();
}

llvm::Value* Ret::compile()
{
	// if(Builder.GetInsertBlock()->getTerminator())
	// 	return nullptr;
	blocks.front()->addRet();
	llvm::Value* ret = rhs->compile();
	if(rhs->isLval() && llvm::PointerType::classof(ret->getType()))
		ret = Builder.CreateLoad(ret, "ret");
	return Builder.CreateRet(ret);
}

llvm::Value* Elsif::compile()
{
	// if(Builder.GetInsertBlock()->getTerminator())
	// 	return nullptr;
	for(unsigned i=0; i<elsif_list.size(); ++i)
		elsif_list[i]->compile();
	return nullptr;
}

llvm::Value* If::compile()
{
	// if(Builder.GetInsertBlock()->getTerminator())
	// 	return nullptr;
	llvm::Value* c = cond->compile();
	if(llvm::PointerType::classof(c->getType()))
		c = Builder.CreateLoad(c, "c");
	llvm::Value* condition = Builder.CreateICmpNE(c, c8(0), "condition");
	llvm::Function* TheFunction = blocks.front()->getFun();
	llvm::BasicBlock* ThenBB = llvm::BasicBlock::Create(TheContext, "then", TheFunction);
	llvm::BasicBlock* ElseBB = llvm::BasicBlock::Create(TheContext, "else", TheFunction);
	llvm::BasicBlock* AfterBB = llvm::BasicBlock::Create(TheContext, "after", TheFunction);
	Builder.CreateCondBr(condition, ThenBB, ElseBB);
	Builder.SetInsertPoint(ThenBB);
	blocks.front()->setBlock(ThenBB);
	for(unsigned i=0; i<if_stmt_list.size(); ++i)
		if_stmt_list[i]->compile();
	// if(!Builder.GetInsertBlock()->getTerminator())
	if(!blocks.front()->hasRet())
		Builder.CreateBr(AfterBB);
	Builder.SetInsertPoint(ElseBB);
	blocks.front()->setBlock(ElseBB);
	for(unsigned i=0; i<elsif_list.size(); ++i)
	{
		AST* c0 = elsif_list[i]->get_condition();
		llvm::Value* c = c0->compile();
		if(llvm::PointerType::classof(c->getType()))
			c = Builder.CreateLoad(c, "c");
		llvm::Value* condition = Builder.CreateICmpNE(c, c8(0), "condition");
		llvm::BasicBlock* ThenBB = llvm::BasicBlock::Create(TheContext, "then", TheFunction);
		llvm::BasicBlock* NextBB = llvm::BasicBlock::Create(TheContext, "next", TheFunction);
		Builder.CreateCondBr(condition, ThenBB, NextBB);
		Builder.SetInsertPoint(ThenBB);
		blocks.front()->setBlock(ThenBB);
		elsif_list[i]->compile();
		// if(!Builder.GetInsertBlock()->getTerminator())
		if(!blocks.front()->hasRet())
			Builder.CreateBr(AfterBB);
		Builder.SetInsertPoint(NextBB);
		blocks.front()->setBlock(NextBB);
	}
	for(unsigned i=0; i<else_stmt_list.size(); ++i)
		else_stmt_list[i]->compile();
	// if(!Builder.GetInsertBlock()->getTerminator())
	if(!blocks.front()->hasRet())
		Builder.CreateBr(AfterBB);
	Builder.SetInsertPoint(AfterBB);
	blocks.front()->setBlock(AfterBB);
	return nullptr;
}

llvm::Value* For::compile()
{
	// if(Builder.GetInsertBlock()->getTerminator())
	// 	return nullptr;
	llvm::BasicBlock* PrevBB = Builder.GetInsertBlock();
	llvm::Function* TheFunction = PrevBB->getParent();
	llvm::BasicBlock* LoopBB = llvm::BasicBlock::Create(TheContext, "loop", TheFunction);
	llvm::BasicBlock* BodyBB = llvm::BasicBlock::Create(TheContext, "body", TheFunction);
	llvm::BasicBlock* AfterBB = llvm::BasicBlock::Create(TheContext, "endfor", TheFunction);
	for(unsigned i=0; i<init.size(); ++i)
		init[i]->compile();
	Builder.CreateBr(LoopBB);
	Builder.SetInsertPoint(LoopBB);
	blocks.front()->setBlock(LoopBB);
	llvm::Value* c = cond->compile();
	if(llvm::PointerType::classof(c->getType()))
		c = Builder.CreateLoad(c, "c");
	llvm::Value* condition = Builder.CreateICmpSGT(c, c8(0), "condition");
	Builder.CreateCondBr(condition, BodyBB, AfterBB);
	Builder.SetInsertPoint(BodyBB);
	blocks.front()->setBlock(BodyBB);
	for(unsigned i=0; i<loop.size(); ++i)
		loop[i]->compile();
	for(unsigned i=0; i<steps.size(); ++i)
		steps[i]->compile();
	Builder.CreateBr(LoopBB);
	Builder.SetInsertPoint(AfterBB);
	blocks.front()->setBlock(AfterBB);
	return nullptr;
}

llvm::Value* Parameter::compile()
{
	return nullptr;
}

llvm::Value* Header::compile()
{
	return nullptr;
}

llvm::Value* Decl::compile()
{
	llvm::Function* TheFunction = TheModule->getFunction(name);
	std::string id = name;
	if(TheFunction)
	{
		id = name+postfix;
		postfix += "7";
	}
	std::shared_ptr<compileBlock> bl = std::make_shared<compileBlock>();
	blocks.push_front(bl);
	for(unsigned i=0; i<parameters.size(); ++i)
	{
		blocks.front()->addParameter(parameters[i]->getId(), parameters[i]->getType(), parameters[i]->getPassMode());
		blocks.front()->addVar(parameters[i]->getId(), parameters[i]->getType(), parameters[i]->getPassMode());
	}

	prev_params = func->getPrevScopeVars();
	for(unsigned i=0; i<prev_params.size(); ++i)
	{
		blocks.front()->addParameter(prev_params[i]->getId(), prev_params[i]->getType(), REF);
		blocks.front()->addVar(prev_params[i]->getId(), prev_params[i]->getType(), REF);
	}

	llvm::FunctionType* fun_type = llvm::FunctionType::get(LLVM_Type(t, VAL), blocks.front()->getParams(), false);
	TheFunction = llvm::Function::Create(fun_type, llvm::Function::ExternalLinkage, id, TheModule.get());
	blocks.front()->setFun(TheFunction);
	scopes.addFun(name, TheFunction);
	scopes.open();
	auto it = TheFunction->arg_begin();
	for(unsigned i=0; i<parameters.size(); ++i)
	{
		it->setName(parameters[i]->getId());
		it++;
	}

	for(unsigned i=0; i<prev_params.size(); ++i)
	{
		it->setName(prev_params[i]->getId());
		it++;
	}

	blocks.pop_front();
	scopes.close();
	return TheFunction;
}

llvm::Value* Func::compile()
{
	llvm::Function* TheFunction;
	std::string id = name;
	if(declared)
		TheFunction = scopes.getFun(name);
	else if(TheModule->getFunction(name))
	{
		id = name+postfix;
		postfix += "7";
	}
	std::shared_ptr<compileBlock> bl = std::make_shared<compileBlock>();
	blocks.push_front(bl);
	for(unsigned i=0; i<parameters.size(); ++i)
	{
		blocks.front()->addParameter(parameters[i]->getId(), parameters[i]->getType(), parameters[i]->getPassMode());
		blocks.front()->addVar(parameters[i]->getId(), parameters[i]->getType(), parameters[i]->getPassMode());
	}

	prev_params = func->getPrevScopeVars();
	for(unsigned i=0; i<prev_params.size(); ++i)
	{
		blocks.front()->addParameter(prev_params[i]->getId(), prev_params[i]->getType(), REF);
		blocks.front()->addVar(prev_params[i]->getId(), prev_params[i]->getType(), REF);
	}

	if(!declared)
	{
		llvm::FunctionType* fun_type = llvm::FunctionType::get(LLVM_Type(t, VAL), blocks.front()->getParams(), false);
		TheFunction = llvm::Function::Create(fun_type, llvm::Function::ExternalLinkage, id, TheModule.get());
	}
	blocks.front()->setFun(TheFunction);
	scopes.addFun(name, TheFunction);
	scopes.open();
	auto it = TheFunction->arg_begin();
	if(!declared)
	{
		for(unsigned i=0; i<parameters.size(); ++i)
		{
			it->setName(parameters[i]->getId());
			it++;
		}

		for(unsigned i=0; i<prev_params.size(); ++i)
		{
			it->setName(prev_params[i]->getId());
			it++;
		}

	}
	llvm::BasicBlock* FunBB = llvm::BasicBlock::Create(TheContext, "entry", TheFunction);
	Builder.SetInsertPoint(FunBB);
	blocks.front()->setBlock(FunBB);
	it = TheFunction->arg_begin();
	for(unsigned i=0; i<parameters.size(); ++i)
	{
		llvm::AllocaInst* a = Builder.CreateAlloca(it->getType(), nullptr, it->getName());
		if(parameters[i]->getPassMode()==REF)
			blocks.front()->addAddr(it->getName(), a);
		else
			blocks.front()->addAlloc(it->getName(), a);
		Builder.CreateStore(it, a);
		it++;
	}

	for(unsigned i=0; i<prev_params.size(); ++i)
	{
		llvm::AllocaInst* a = Builder.CreateAlloca(it->getType(), nullptr, it->getName());
		blocks.front()->addAddr(it->getName(), a);
		Builder.CreateStore(it, a);
		it++;
	}

	for(unsigned i=0; i<def_list.size(); ++i)
	{
		Builder.SetInsertPoint(FunBB);
		def_list[i]->compile();
	}
	Builder.SetInsertPoint(FunBB);
	for(unsigned i=0; i<stmts.size(); ++i)
		stmts[i]->compile();
	llvm::BasicBlock* CurBB = Builder.GetInsertBlock();
	if (!CurBB->getTerminator())
	{
		if(t->getType()==VOID)
			Builder.CreateRetVoid();
		else if(!CurBB->getFirstNonPHI() && pred_begin(CurBB)==pred_end(CurBB))
			CurBB->eraseFromParent();
		else
		{
			switch(t->getType())
			{
				case INT:
					Builder.CreateRet(c16(0));
					break;
				case CHAR:
				case BOOL:
					Builder.CreateRet(c8(0));
					break;
				case ARRAY:
					Builder.CreateRet(llvm::Constant::getNullValue(LLVM_Type(t->refType(), VAL)));
					break;
				case LIST:
					Builder.CreateRet(llvm::ConstantPointerNull::get(List_t));
					break;
				default:
					;
			}
		}
	}
	blocks.pop_front();
	scopes.close();
	if(!main)
		Builder.SetInsertPoint(blocks.front()->getBlock());
	verifyFunction(*TheFunction, &llvm::errs());
	TheFPM->run(*TheFunction);
	return TheFunction;
}
