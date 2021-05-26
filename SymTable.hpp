#ifndef __SYMTABLE_HPP__
#define __SYMTABLE_HPP__

//the implementation is based on the one found here: https://git.softlab.ntua.gr/spirusdn/Alan-Compiler-CXX

#include <memory>
#include <vector>
#include <deque>
#include <unordered_map>
#include "ast.hpp"

enum BOperator {
	PLUS,
	MINUS,
	MUL,
	DIV,
	MOD,
	EQ,
	NOT_EQ,
	LESS,
	GREATER,
	LESS_EQ,
	GREATER_EQ,
	AND,
	OR,
	CONS
};

enum UOperator {
	UPLUS,
	UMINUS,
	NOT,
	NILQ,
	HEAD,
	TAIL
};

enum Type_tag {
	VOID,
	INT,
	CHAR,
	BOOL,
	ARRAY,
	LIST
};

enum Entry_tag {
	VAR,
	FUN,
	PAR
};

enum PassMode {
	VAL,
	REF
};

enum SearchScope {
	CUR,
	ALL
};

class Type {
public:
	Type_tag getType()
	{
		return this->t;
	}

	virtual int getSize() = 0;

	virtual std::shared_ptr<Type> refType()
	{
		return nullptr;
	}

	virtual std::ostream& print(std::ostream &out) const = 0;

	friend std::ostream& operator<<(std::ostream &out, const Type &t)
	{
		return t.print(out);
	}

protected:
	Type_tag t;
};


class VoidType : public Type {
public:
	VoidType()
	{
		this->t = VOID;
	}

	virtual int getSize() override
	{
		std::cerr<<"Void doesn't have size"<<'\n';
		exit(1);
	}

	virtual std::ostream& print(std::ostream &out) const override
	{
		return (out<<"void");
	}
};

class IntType : public Type {
public:
	IntType()
	{
		this->t = INT;
	}

	virtual int getSize() override
	{
		return 4;
	}

	virtual std::ostream& print(std::ostream &out) const override
	{
		return (out<<"int");
	}
};

class CharType : public Type {
public:
	CharType()
	{
		this->t = CHAR;
	}

	virtual int getSize() override
	{
		return 1;
	}

	virtual std::ostream& print(std::ostream &out) const override
	{
		return (out<<"char");
	}
};

class BoolType : public Type {
public:
	BoolType()
	{
		this->t = BOOL;
	}

	virtual int getSize() override
	{
		return 1;
	}

	virtual std::ostream& print(std::ostream &out) const override
	{
		return (out<<"bool");
	}
};

class ArrayType : public Type {
public:
	ArrayType(std::shared_ptr<Type> r)
	{
		this->t = ARRAY;
		this->ref = r;
	}

	virtual int getSize() override
	{
		return 8;
	}

	virtual std::shared_ptr<Type> refType() override
	{
		return this->ref;
	}

	virtual std::ostream& print(std::ostream &out) const override
	{
		return (out<<"array of "<<*(this->ref));
	}

private:
	std::shared_ptr<Type> ref;
};

class ListType : public Type {
public:
	ListType(std::shared_ptr<Type> r)
	{
		this->t = LIST;
		this->ref = r;
	}

	virtual int getSize() override
	{
		return 8;
	}

	virtual std::shared_ptr<Type> refType() override
	{
		return this->ref;
	}

	virtual std::ostream& print(std::ostream &out) const override
	{
		if(this->ref==nullptr) return (out<<"nil");
		return (out<<"list of "<<*(this->ref));
	}

private:
	std::shared_ptr<Type> ref;
};

class Entry {
public:
	std::string getId()
	{
		return this->id;
	}

	virtual bool isPrevScope()
	{
		return false;
	}

	void setNesting(int nesting)
	{
		this->nesting=nesting;
	}

	int getNesting()
	{
		return this->nesting;
	}

	Entry_tag getEntryType()
	{
		return this->entry_type;
	}

	std::shared_ptr<Type> getType()
	{
		return this->type;
	}

	virtual void setOffset(int offset)
	{
		std::cerr<<'\"'<<this->id<<'\"'<<" is not a variable or a parameter."<<'\n';
		exit(1);
	}

	virtual int getOffset()
	{
		std::cerr<<'\"'<<this->id<<'\"'<<" is not a variable or a parameter."<<'\n';
		exit(1);
	}

	virtual void plusRet()
	{
		std::cerr<<'\"'<<this->id<<'\"'<<" is not a function."<<'\n';
		exit(1);
	}

	virtual int numRet()
	{
		std::cerr<<'\"'<<this->id<<'\"'<<" is not a function."<<'\n';
		exit(1);
	}

	virtual PassMode getPassMode()
	{
		std::cerr<<'\"'<<this->id<<'\"'<<" is not a parameter."<<'\n';
		exit(1);
	}

	virtual void addParameter(std::shared_ptr<Entry> parameter)
	{
		std::cerr<<'\"'<<this->id<<'\"'<<" is not a function."<<'\n';
		exit(1);
	}

	virtual const std::vector<std::shared_ptr<Entry>>& getParameters() const
	{
		std::cerr<<'\"'<<this->id<<'\"'<<" is not a function."<<'\n';
		exit(1);
	}

	virtual void addPrevScopeVar(std::shared_ptr<Entry> variable)
	{
		std::cerr<<'\"'<<this->id<<'\"'<<" is not a function."<<'\n';
		exit(1);
	}

	virtual const std::vector<std::shared_ptr<Entry>>& getPrevScopeVars() const
	{
		std::cerr<<'\"'<<this->id<<'\"'<<" is not a function."<<'\n';
		exit(1);
	}

	virtual void UnDecl()
	{
		std::cerr<<'\"'<<this->id<<'\"'<<" is not a function."<<'\n';
		exit(1);
	}

	virtual const bool isDecl() const
	{
		std::cerr<<'\"'<<this->id<<'\"'<<" is not a function."<<'\n';
		exit(1);
	}

	virtual std::ostream& print(std::ostream &out) const = 0;

	friend std::ostream& operator<<(std::ostream &out, const Entry &e)
	{
		return e.print(out);
	}

protected:
	int nesting;
	std::string id;
	Entry_tag entry_type;
	std::shared_ptr<Type> type;
};

class VariableEntry : public Entry {
public:
	VariableEntry(std::string id, std::shared_ptr<Type> type)
	{
		this->id=id;
		this->type=type;
		this->entry_type=VAR;
	}

	void setOffset(int offset)
	{
		this->offset=offset;
	}

	int getOffset()
	{
		return this->offset;
	}

	virtual std::ostream& print(std::ostream &out) const override
	{
		return (out<<"variable: "<<'\"'<<this->id<<'\"'<<" with type "<<*type);
	}

private:
	int offset;
};

class ParameterEntry : public Entry {
public:
	ParameterEntry(std::string id, std::shared_ptr<Type> type, PassMode mode, bool prev=false)
	{
		if((type->getType()==ARRAY || type->getType()==LIST) && mode==VAL)
		{
			std::cerr<<"Lists and Arrays must always be passed by reference."<<'\n';
			exit(1);
		}
		this->id=id;
		this->type=type;
		this->mode=mode;
		this->entry_type=PAR;
		this->isPrev=prev;
	}

	virtual bool isPrevScope() override
	{
		return this->isPrev;
	}

	void setOffset(int offset)
	{
		this->offset=offset;
	}

	int getOffset()
	{
		return this->offset;
	}

	PassMode getPassMode()
	{
		return this->mode;
	}

	virtual std::ostream& print(std::ostream &out) const override
	{
		return (out<<isPrev<<"parameter: "<<'\"'<<this->id<<'\"');
	}

private:
	int offset;
	bool isPrev;
	PassMode mode;
};

class FunctionEntry : public Entry {
public:
	FunctionEntry(std::string id, std::shared_ptr<Type> type, bool decl)
	{
		this->id=id;
		this->type=type;
		this->decl=decl;
		this->returns=0;
		this->entry_type=FUN;
	}

	void plusRet()
	{
		this->returns++;
	}

	int num_returns()
	{
		return this->returns;
	}

	void addParameter(std::shared_ptr<Entry> parameter)
	{
		parameters.push_back(parameter);
	}

	const std::vector<std::shared_ptr<Entry>>& getParameters() const
	{
		return this->parameters;
	}

	void addPrevScopeVar(std::shared_ptr<Entry> variable)
	{
		prev_scope_vars.push_back(variable);
	}

	const std::vector<std::shared_ptr<Entry>>& getPrevScopeVars() const
	{
		return this->prev_scope_vars;
	}

	virtual void UnDecl()
	{
		decl=false;
	}

	virtual const bool isDecl() const
	{
		return decl;
	}

	virtual std::ostream& print(std::ostream &out) const override
	{
		if(decl) out<<"declared ";
		out<<"function: "<<'\"'<<this->id<<'\"'<<" nesting "<<nesting;
		if(parameters.size()==0) out<<"\n\tno parameters";
		else
		{
			out<<"\n\tparameters:";
			for(auto it=parameters.begin(); it!=parameters.end(); ++it)
			{
				out<<"\n\t\t"<<*(*it)<<" with type "<<*((*it)->getType());
			}
		}
		if(prev_scope_vars.size()==0) out<<"\n\tno previous scopes variables";
		else
		{
			out<<"\n\tprevious scopes variables:";
			for(auto it=prev_scope_vars.begin(); it!=prev_scope_vars.end(); ++it)
			{
				out<<"\n\t\t"<<*(*it)<<" with type "<<*((*it)->getType());
			}
		}
		return out;
	}

private:
	int returns;
	bool decl;
	std::vector<std::shared_ptr<Entry>> parameters, prev_scope_vars;
};

class Scope {
public:
	Scope(std::shared_ptr<Entry> function, int nesting)
	{
		this->function=function;
		this->nesting=nesting;
		this->offset=0;
	}

	int getNesting()
	{
		return nesting;
	}

	int getOffset()
	{
		return offset;
	}

	void setOffset(int offset)
	{
		this->offset=offset;
	}

	std::shared_ptr<Entry> getFunction()
	{
		return function;
	}

	int offset;
private:
	int nesting;
	std::shared_ptr<Entry> function;
};

class SymTable {
public:
	void openScope(std::shared_ptr<Entry> function)
	{
		if(scopes.empty()) scopes.push_front(std::make_shared<Scope>(function, 1));
		else scopes.push_front(std::make_shared<Scope>(function, scopes.front()->getNesting() + 1));
	}

	void closeScope()
	{
		if(scopes.empty()) return;
		int nesting = scopes.front()->getNesting();
		for(auto& it: this->entries)
		{
			std::deque<std::shared_ptr<Entry>>& sc = it.second;
			while(!sc.empty())
			{
				if(sc.front()->getNesting() < nesting) break;
				sc.pop_front();
			}
		}
		scopes.pop_front();
	}

	std::shared_ptr<Type> getScopeType()
	{
		return this->scopes.front()->getFunction()->getType();
	}

	void plusRet()
	{
		this->scopes.front()->getFunction()->plusRet();
	}

	void addParam(std::shared_ptr<Entry> e)
	{
		scopes.front()->getFunction()->addParameter(e);
	}

	void addPrevScopeVar(std::shared_ptr<Entry> e)
	{
		int nesting = e->getNesting();
		for(unsigned i = 0; i < scopes.size(); ++i)
		{
			if(scopes[i]->getNesting() > nesting) scopes[i]->getFunction()->addPrevScopeVar(e);
			else break;
		}
		for(int i = nesting+1;; ++i) {
			std::shared_ptr<Entry> tmp = std::make_shared<ParameterEntry>(e->getId(), e->getType(), REF, true);
			tmp->setNesting(i);
			entries[e->getId()].push_front(tmp);
			if (i == scopes.front()->getNesting()) break;
	    }
	}

	void insert(std::shared_ptr<Entry> e)
	{
		e->setNesting(scopes.front()->getNesting());
		if(e->getEntryType() == VAR || e->getEntryType() == PAR)
		{
			e->setOffset(scopes.front()->offset++);
		}
		entries[e->getId()].push_front(e);
		return;
	}

	void replaceTop(std::shared_ptr<Entry> e)
	{
		this->entries[e->getId()].pop_front();
		this->entries[e->getId()].push_front(e);
	}

	std::shared_ptr<Entry> lookup(std::string id, SearchScope s, bool error)
	{
		auto f = this->entries.find(id);
		if(f == this->entries.end())
		{
			if(error)
			{
				std::cerr<<"Unknown identifier \""<<id<<"\"\n";
				exit(1);
			}
			return nullptr;
		}
		std::deque<std::shared_ptr<Entry>> e = this->entries[id];
		if(s == CUR)
		{
			if(e.front()==nullptr) return nullptr;
			if(e.front()->getNesting() != this->scopes.front()->getNesting()) return nullptr;
			return e.front();
		}
		else return e.front();
		return nullptr;
	}

	std::shared_ptr<Scope> getScope()
	{
		return scopes.front();
	}

private:
	std::unordered_map<std::string, std::deque<std::shared_ptr<Entry>>> entries;
	std::deque<std::shared_ptr<Scope>> scopes;
};

inline std::shared_ptr<SymTable> TableInit()
{
	std::shared_ptr<SymTable> t = std::make_shared<SymTable>();
	auto global_fun = std::make_shared<FunctionEntry>("global", std::make_shared<VoidType>(), false);
    t->openScope(global_fun);

    auto puti = std::make_shared<FunctionEntry>("puti", std::make_shared<VoidType>(), false);
    puti->addParameter(std::make_shared<ParameterEntry>("n", std::make_shared<IntType>(), VAL));
    t->insert(puti);

    auto putb = std::make_shared<FunctionEntry>("putb", std::make_shared<VoidType>(), false);
    putb->addParameter(std::make_shared<ParameterEntry>("b", std::make_shared<BoolType>(), VAL));
    t->insert(putb);

    auto putc = std::make_shared<FunctionEntry>("putc", std::make_shared<VoidType>(), false);
    putc->addParameter(std::make_shared<ParameterEntry>("c", std::make_shared<CharType>(), VAL));
    t->insert(putc);

    auto puts = std::make_shared<FunctionEntry>("puts", std::make_shared<VoidType>(), false);
    puts->addParameter(std::make_shared<ParameterEntry>("c", std::make_shared<ArrayType>(std::make_shared<CharType>()), REF));
    t->insert(puts);

    auto geti = std::make_shared<FunctionEntry>("geti", std::make_shared<IntType>(), false);
    t->insert(geti);

    auto getb = std::make_shared<FunctionEntry>("getb", std::make_shared<BoolType>(), false);
    t->insert(getb);

    auto getc = std::make_shared<FunctionEntry>("getc", std::make_shared<CharType>(), false);
    t->insert(getc);

    auto gets = std::make_shared<FunctionEntry>("gets", std::make_shared<VoidType>(), false);
    gets->addParameter(std::make_shared<ParameterEntry>("n", std::make_shared<IntType>(), VAL));
    gets->addParameter(std::make_shared<ParameterEntry>("s", std::make_shared<ArrayType>(std::make_shared<CharType>()), REF));
    t->insert(gets);

    auto abs = std::make_shared<FunctionEntry>("abs", std::make_shared<IntType>(), false);
    abs->addParameter(std::make_shared<ParameterEntry>("n", std::make_shared<IntType>(), VAL));
    t->insert(abs);

    auto ord = std::make_shared<FunctionEntry>("ord", std::make_shared<IntType>(), false);
    ord->addParameter(std::make_shared<ParameterEntry>("c", std::make_shared<CharType>(), VAL));
    t->insert(ord);

    auto chr = std::make_shared<FunctionEntry>("chr", std::make_shared<CharType>(), false);
    chr->addParameter(std::make_shared<ParameterEntry>("n", std::make_shared<IntType>(), VAL));
    t->insert(chr);

    auto strlen = std::make_shared<FunctionEntry>("strlen", std::make_shared<IntType>(), false);
    strlen->addParameter(std::make_shared<ParameterEntry>("s", std::make_shared<ArrayType>(std::make_shared<CharType>()), REF));
    t->insert(strlen);

    auto strcmp = std::make_shared<FunctionEntry>("strcmp", std::make_shared<IntType>(), false);
    strcmp->addParameter(std::make_shared<ParameterEntry>("s1", std::make_shared<ArrayType>(std::make_shared<CharType>()), REF));
    strcmp->addParameter(std::make_shared<ParameterEntry>("s2", std::make_shared<ArrayType>(std::make_shared<CharType>()), REF));
    t->insert(strcmp);

    auto strcpy = std::make_shared<FunctionEntry>("strcpy", std::make_shared<VoidType>(), false);
    strcpy->addParameter(std::make_shared<ParameterEntry>("trg", std::make_shared<ArrayType>(std::make_shared<CharType>()), REF));
    strcpy->addParameter(std::make_shared<ParameterEntry>("src", std::make_shared<ArrayType>(std::make_shared<CharType>()), REF));
    t->insert(strcpy);

    auto strcat = std::make_shared<FunctionEntry>("strcat", std::make_shared<IntType>(), false);
    strcat->addParameter(std::make_shared<ParameterEntry>("trg", std::make_shared<ArrayType>(std::make_shared<CharType>()), REF));
    strcat->addParameter(std::make_shared<ParameterEntry>("src", std::make_shared<ArrayType>(std::make_shared<CharType>()), REF));
    t->insert(strcat);

	return t;
}

inline bool equals(std::shared_ptr<Type> a, std::shared_ptr<Type> b, bool check_ref = true)
{
	if(!check_ref)
		return a->getType() == b->getType();
	if (a->getType() == b->getType())
	{
		if(a->getType() == ARRAY)
			return equals(a->refType(), b->refType());
		if(a->getType() == LIST) 
		{
			if(a->refType() == nullptr || b->refType() == nullptr)
				return true;
			else
				return equals(a->refType(), b->refType());
		}
		return true;
	}
	return false;
}

#endif