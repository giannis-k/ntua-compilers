#ifndef __AST_HPP__
#define __AST_HPP__

#include <cstdlib>
#include <iostream>
#include <vector>
#include <memory>

#include <llvm/IR/Value.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils.h>

#include "SymTable.hpp"

class AST {
public:
  std::shared_ptr<Type> getType()
  {
    return t;
  }

  void setType(std::shared_ptr<Type> t1)
  {
    t=t1;
  }

  virtual bool isLval()
  {
    return false;
  }

  virtual bool isString()
  {
    return false;
  }

  virtual void sem(std::shared_ptr<SymTable> table)
  {
    return;
  }

  virtual AST* get_condition()
  {
    return nullptr;
  }

  virtual void set_prev_vars()
  {
    return;
  }

  virtual llvm::Value* compile() = 0;

  void begin_compilation(bool opt);

  static llvm::LLVMContext TheContext;
  static llvm::Type *i1;
  static llvm::Type *i8;
  static llvm::Type *i16;
  static llvm::Type *i32;
  static llvm::Type *i64;
  static llvm::PointerType *List_t;

protected:
  std::shared_ptr<Type> t;

  static std::unordered_map<std::string, std::vector<std::shared_ptr<AST>>> my_map;

  static llvm::IRBuilder<> Builder;
  static std::unique_ptr<llvm::Module> TheModule;
  static std::unique_ptr<llvm::legacy::FunctionPassManager> TheFPM;
  static llvm::Function *ThePuti;
  static llvm::Function *ThePutb;
  static llvm::Function *ThePutc;
  static llvm::Function *ThePuts;
  static llvm::Function *TheGeti;
  static llvm::Function *TheGetb;
  static llvm::Function *TheGetc;
  static llvm::Function *TheGets;
  static llvm::Function *TheAbs;
  static llvm::Function *TheOrd;
  static llvm::Function *TheChr;
  static llvm::Function *TheStrlen;
  static llvm::Function *TheStrcmp;
  static llvm::Function *TheStrcpy;
  static llvm::Function *TheStrcat;
  static llvm::Function *TheInit;
  static llvm::Function *TheMalloc;

  // static llvm::ConstantInt* c1(bool b) {
  //   return llvm::ConstantInt::get(TheContext, llvm::APInt(1, b, true));
  // }
  static llvm::ConstantInt* c8(char c) {
    return llvm::ConstantInt::get(TheContext, llvm::APInt(8, c, true));
  }
  static llvm::ConstantInt* c16(int n) {
    return llvm::ConstantInt::get(TheContext, llvm::APInt(16, n, true));
  }
  static llvm::ConstantInt* c32(int n) {
    return llvm::ConstantInt::get(TheContext, llvm::APInt(32, n, true));
  }
  static llvm::ConstantInt* c64(int n) {
    return llvm::ConstantInt::get(TheContext, llvm::APInt(64, n, true));
  }
};

class Int : public AST {
public:
  Int(int i)
  {
    value=i;
    t=std::make_shared<IntType>();
  }

  virtual llvm::Value* compile() override;

private:
  int value;
};

class Char : public AST {
public:
  Char(char c)
  {
    value=c;
    t=std::make_shared<CharType>();
  }

  virtual llvm::Value* compile() override;

private:
  char value;  
};

class String : public AST {
public:
  String(std::string s)
  {
    value=s;
    t=std::make_shared<ArrayType>(std::make_shared<CharType>());
  }

  virtual bool isString() override
  {
    return true;
  }

  virtual llvm::Value* compile() override;

private:
  std::string value;
};

class Bool : public AST {
public:
  Bool(bool b)
  {
    value=b;
    t=std::make_shared<BoolType>();
  }

  virtual llvm::Value* compile() override;

private:
  bool value;
};

class Var : public AST {
public:
  Var(std::string n)
  {
    id=n;
  }

  virtual void sem(std::shared_ptr<SymTable> table) override
  {
    std::shared_ptr<Entry> e = table->lookup(id, ALL, true);
    if(e==nullptr)
    {
      std::cerr<<"Unknown identifier "<<id<<'\n';
      exit(1);
    }
    if(e->getEntryType() == FUN)
    {
      std::cerr<<id<<" is not a variable"<<'\n';
      exit(1);
    }
    if(e->getNesting() < table->getScope()->getNesting())
      table->addPrevScopeVar(e);
    t = e->getType();
  }

  virtual bool isLval() override
  {
    return true;
  }

  virtual llvm::Value* compile() override;

private:
  std::string id;
};

class Arr : public AST {
public:
  Arr(AST* t1, AST* t2)
  {
    atom=t1;
    pos=t2;
  }

  virtual void sem(std::shared_ptr<SymTable> table) override
  {
    atom->sem(table);
    pos->sem(table);
    std::shared_ptr<Type> j(new ArrayType(nullptr));
    if(!equals(atom->getType(), j, false))
    {
      std::cerr<<"Left member of [] expression is not an array"<<'\n';
      exit(1);
    }
    std::shared_ptr<Type> i(new IntType);
    if(!equals(pos->getType(), i))
    {
      std::cerr<<"Array index must be integer"<<'\n';
      exit(1);
    }
    t = atom->getType()->refType();
  }

  virtual bool isLval() override
  {
    return true;
  }

  virtual bool isString() override
  {
    return atom->isString();
  }

  virtual llvm::Value* compile() override;

private:
  AST *atom, *pos;
};

class Nil : public AST {
public:
  Nil()
  {
    t=std::make_shared<ListType>(nullptr);
  }

  virtual llvm::Value* compile() override;
};

class BinOp : public AST {
public:
  BinOp(AST* t1, BOperator o, AST* t2)
  {
    lhs = t1;
    rhs = t2;
    op = o;
  }

  virtual void sem(std::shared_ptr<SymTable> table) override
  {
    lhs->sem(table);
    rhs->sem(table);
    std::shared_ptr<Type> i(new IntType);
    std::shared_ptr<Type> j(new ArrayType(nullptr));
    std::shared_ptr<Type> k(new ListType(nullptr));
    std::shared_ptr<Type> l(new BoolType);
    switch(op)
    {
      case PLUS:
      case MINUS:
      case MUL:
      case DIV:
      case MOD:
        if(!equals(lhs->getType(), i) || !equals(rhs->getType(), i))
        {
          std::cerr<<"One of the two operands of a numerical expression in not an integer"<<'\n';
          exit(1);
        }
        t=std::make_shared<IntType>();
        break;
      case EQ:
      case NOT_EQ:
      case LESS:
      case GREATER:
      case LESS_EQ:
      case GREATER_EQ:
        if(!equals(lhs->getType(), rhs->getType()))
        {
          std::cerr<<"The two operands of a comparison expression are not of the same type"<<'\n';
          exit(1);
        }
        if(equals(lhs->getType(), j, false) || equals(lhs->getType(), k, false))
        {
          std::cerr<<"You cannot compare arrays/lists"<<'\n';
          exit(1);
        }
        t=std::make_shared<BoolType>();
        break;
      case AND:
      case OR:
        if(!equals(lhs->getType(), l) || !equals(rhs->getType(), l))
        {
          std::cerr<<"One of the two operands of a logical expression is not a boolean"<<'\n';
          exit(1);
        }
        t=std::make_shared<BoolType>();
        break;
      case CONS:
      	std::shared_ptr<Type> k(new ListType(nullptr));
      	if(!equals(rhs->getType(), k, false))
  		{
          std::cerr<<"Second operand of the # operator must be a list"<<'\n';
          exit(1);
        }
        if(rhs->getType()->refType()!=nullptr && !equals(lhs->getType(), rhs->getType()->refType()))
        {
          std::cerr<<"Operands of the # operator must be of types t and list[t]"<<'\n';
          exit(1);
        }
        t=std::make_shared<ListType>(lhs->getType());
        break;
    }

  }

  virtual llvm::Value* compile() override;

private:
  AST *lhs, *rhs;
  BOperator op;
};

class UnOp : public AST {
public:
  UnOp(UOperator o, AST* t1)
  {
    rhs = t1;
    op = o;
  }

  virtual void sem(std::shared_ptr<SymTable> table) override
  {
    rhs->sem(table);
    std::shared_ptr<Type> i(new IntType);
    std::shared_ptr<Type> k(new ListType(nullptr));
    std::shared_ptr<Type> l(new BoolType);
    switch(op)
    {
      case UPLUS:
      case UMINUS:
        if(!equals(rhs->getType(), i))
        {
          std::cerr<<"Operand of a +/- expression in not an integer"<<'\n';
          exit(1);
        }
        t=std::make_shared<IntType>();
        break;
      case NOT:
        if(!equals(rhs->getType(), l))
        {
          std::cerr<<"Operand of a not expression in not a boolean"<<'\n';
          exit(1);
        }
        t=std::make_shared<BoolType>();
        break;
      case NILQ:
        if(!equals(rhs->getType(), k, false))
        {
          std::cerr<<"Operand of a nil? expression in not a list"<<'\n';
          exit(1);
        }
        t=std::make_shared<BoolType>();
        break;
      case HEAD:
        if(!equals(rhs->getType(), k, false))
        {
          std::cerr<<"Operand of a head expression is not a list"<<'\n';
          exit(1);
        }
        t=rhs->getType()->refType();
        if(t==nullptr)
        {
          std::cerr<<"Operand of a head expression cannot be nil"<<'\n';
          exit(1);
        }
        break;
      case TAIL:
        if(!equals(rhs->getType(), k, false))
        {
          std::cerr<<"Operand of a tail expression is not a list"<<'\n';
          exit(1);
        }
        t=rhs->getType();
        if(t->refType()==nullptr)
        {
          std::cerr<<"Operand of a tail expression cannot be nil"<<'\n';
          exit(1);
        }
        break;
    }
  }

  virtual llvm::Value* compile() override;

private:
  AST* rhs;
  UOperator op;
};

class New : public AST {
public:
  New(std::shared_ptr<Type> ty, AST* s)
  {
    size = s;
    t=std::make_shared<ArrayType>(ty);
  }

  virtual void sem(std::shared_ptr<SymTable> table) override
  {
    size->sem(table);
    std::shared_ptr<Type> i(new IntType);
    if(!equals(size->getType(), i))
    {
      std::cerr<<"Operator new expects integer size"<<'\n';
      exit(1);
    }
  }

  virtual llvm::Value* compile() override;

private:
  AST* size;
};

class Skip : public AST {
public:
  Skip(){}

  virtual llvm::Value* compile() override;
};

class Assign : public AST {
public:
  Assign(AST *t1, AST *t2)
  {
    lhs = t1;
    rhs = t2;
  }

  virtual void sem(std::shared_ptr<SymTable> table) override
  {
    if(!lhs->isLval())
    {
      std::cerr<<"Left side of assignment is not an L-value"<<'\n';
      exit(1);
    }
    lhs->sem(table);
    if(lhs->isString())
    {
      std::cerr<<"You cannot assign a const string"<<'\n';
      exit(1);
    }
    rhs->sem(table);
    if(!equals(lhs->getType(), rhs->getType()))
    {
      std::cerr<<"Type mismatch in assignment"<<'\n';
      exit(1);
    }
    // t = lhs->getType();
  }

  virtual llvm::Value* compile() override;

private:
  AST *lhs, *rhs;
};

class Call : public AST {
public:
	Call(std::string n, std::vector<AST*> par)
	{
		id = n;
		parameters = std::move(par);
	}

  virtual void sem(std::shared_ptr<SymTable> table) override
  {
    std::shared_ptr<Type> k(new ListType(nullptr));
    std::shared_ptr<Type> l(new ArrayType(nullptr));
    std::shared_ptr<Entry> e = table->lookup(id, ALL, true);
    if(e==nullptr)
    {
      std::cerr<<"Unknown identifier "<<id<<'\n';
      exit(1);
    }
    if(e->getEntryType() != FUN)
    {
      std::cerr<<id<<" is not a function"<<'\n';
      exit(1);
    }
    if(parameters.size() != e->getParameters().size())
    {
      std::cerr<<"Wrong number of arguments in call of function "<<id<<'\n';
      exit(1);
    }
    for(auto it=parameters.begin(); it!=parameters.end(); it++)
      (*it)->sem(table);
    std::vector<std::shared_ptr<Entry>> p = e->getParameters();
    for(unsigned i=0; i<parameters.size(); ++i)
    {
      if(!equals(parameters[i]->getType(), p[i]->getType()))
      {
        std::cerr<<"Type mismatch in parameter "<<p[i]->getId()<<" of function "<<id<<'\n';
        exit(1);
      }
      else if(p[i]->getPassMode()==REF && !parameters[i]->isLval() && !equals(parameters[i]->getType(), k, false) && !equals(parameters[i]->getType(), l, false))
      {
        std::cerr<<"Parameter "<<p[i]->getId()<<" of function "<<id<<" must be an L-value\n";
        exit(1);
      }
    }
    t = e->getType();
    fun = e;
  }

  virtual llvm::Value* compile() override;

private:
	std::vector<AST*> parameters;
	std::string id;
  std::shared_ptr<Entry> fun;
};

class Exit : public AST {
public:
	Exit(){}

  virtual void sem(std::shared_ptr<SymTable> table) override
  {
    std::shared_ptr<Type> i(new VoidType);
    if(!equals(table->getScopeType(), i))
    {
      std::cerr<<"Exit statement in a non void function"<<'\n';
      exit(1);
    }
  }

  virtual llvm::Value* compile() override;

};

class Ret : public AST {
public:
	Ret(AST *t)
	{
		rhs = t;
	}

  virtual void sem(std::shared_ptr<SymTable> table) override
  {
    rhs->sem(table);
    t = rhs->getType();
    if(!equals(table->getScopeType(), t))
    {
      std::cerr<<"Return type does not match function type"<<'\n';
      exit(1);
    }
  }

  virtual llvm::Value* compile() override;

private:
	AST *rhs;
};

class Elsif : public AST {
public:
	Elsif(AST *t1, std::vector<AST*> t2)
	{
		cond = t1;
		elsif_list = std::move(t2);
	}

  virtual void sem(std::shared_ptr<SymTable> table) override
  {
    cond->sem(table);
    std::shared_ptr<Type> l(new BoolType);
    if(!equals(cond->getType(), l))
    {
      std::cerr<<"Expression after elsif must be boolean"<<'\n';
      exit(1);
    }
    for(unsigned i=0; i<elsif_list.size(); ++i)
      elsif_list[i]->sem(table);
  }

  virtual AST* get_condition() override
  {
    return cond;
  }

  virtual llvm::Value* compile() override;

private:
	std::vector<AST*> elsif_list;
	AST *cond;
};

class If : public AST {
public:
	If(AST *t1, std::vector<AST*> t2, std::vector<AST*> t3, std::vector<AST*> t4 = std::vector<AST*>())
	{
		cond = t1;
		if_stmt_list = std::move(t2);
		elsif_list = std::move(t3);
		else_stmt_list = std::move(t4);
	}

  virtual void sem(std::shared_ptr<SymTable> table) override
  {
    cond->sem(table);
    std::shared_ptr<Type> l(new BoolType);
    if(!equals(cond->getType(), l))
    {
      std::cerr<<"Expression after if must be boolean"<<'\n';
      exit(1);
    }
    for(unsigned i=0; i<if_stmt_list.size(); ++i)
      if_stmt_list[i]->sem(table);
    for(unsigned i=0; i<elsif_list.size(); ++i)
      elsif_list[i]->sem(table);
    for(unsigned i=0; i<else_stmt_list.size(); ++i)
      else_stmt_list[i]->sem(table);
  }

  virtual llvm::Value* compile() override;

private:
	std::vector<AST*> if_stmt_list, elsif_list, else_stmt_list;
	AST *cond;
};

class For : public AST {
public:
	For(std::vector<AST*> t1, AST *t2, std::vector<AST*> t3, std::vector<AST*> t4)
	{
		init = std::move(t1);
		cond = t2;
		steps = std::move(t3);
		loop = std::move(t4);
	}

  virtual void sem(std::shared_ptr<SymTable> table) override
  {
    for(unsigned i=0; i<init.size(); ++i)
      init[i]->sem(table);
    cond->sem(table);
    std::shared_ptr<Type> l(new BoolType);
    if(!equals(cond->getType(), l))
    {
      std::cerr<<"Condition inside for statement is not boolean"<<'\n';
      exit(1);
    }
    for(unsigned i=0; i<steps.size(); ++i)
      steps[i]->sem(table);
    for(unsigned i=0; i<loop.size(); ++i)
      loop[i]->sem(table);
  }

  virtual llvm::Value* compile() override;

private:
	std::vector<AST*> init, steps, loop;
	AST *cond;
};

class VarDef : public AST {
public:
  VarDef(std::shared_ptr<Type> ty, std::vector<std::string> t2)
  {
    t = ty;
    var_names = std::move(t2);
  }

  virtual void sem(std::shared_ptr<SymTable> table) override
  {
    for(unsigned i=0; i<var_names.size(); ++i)
    {
      std::shared_ptr<Entry> e = table->lookup(var_names[i], CUR, false);
      if(e!=nullptr)
      {
      	if(!e->isPrevScope())
        {
          std::cerr<<"dubl "<<*e<<'\n';
          std::cerr<<"Dublicate identifier "<<var_names[i]<<'\n';
          exit(1);
        }
        table->replaceTop(std::make_shared<VariableEntry>(var_names[i], t));
        return;
      }
      table->insert(std::make_shared<VariableEntry>(var_names[i], t));
    }
  }

  virtual llvm::Value* compile() override;

private:
  std::vector<std::string> var_names;
};

class Parameter : public AST {
public:
  Parameter(std::shared_ptr<Type> ty, std::vector<std::string> t2, PassMode m)
  {
    t = ty;
    var_names.clear();
    var_names.push_back(t2.back());
    for(unsigned i=0; i<t2.size()-1; ++i)
      var_names.push_back(t2[i]);
    mode = m;
  }

  const std::vector<std::string>& getVarNames()
  {
    return var_names;
  }

  PassMode getMode()
  {
    return mode;
  }

  virtual llvm::Value* compile() override;

private:
  std::vector<std::string> var_names;
  PassMode mode;
};

class Header : public AST {
public:
  Header(std::shared_ptr<Type> ty, std::string t2, std::vector<Parameter*> t3 = std::vector<Parameter*>())
  {
    t = ty;
    name = t2;
    std::vector<Parameter*> vars;
    if(t3.size()!=0)
    {
      vars.push_back(t3.back());
      for(unsigned i=0; i<t3.size()-1; ++i)
        vars.push_back(t3[i]);
    }

    std::shared_ptr<Type> k(new ListType(nullptr));
    std::shared_ptr<Type> l(new ArrayType(nullptr));
    for(unsigned i=0; i<vars.size(); ++i)
    {
      std::vector<std::string> names = vars[i]->getVarNames();
      for(unsigned j=0; j<names.size(); ++j)
      {
        PassMode m = (equals(vars[i]->getType(), k, false) || equals(vars[i]->getType(), l, false) ) ? REF : vars[i]->getMode();
        parameters.push_back(std::make_shared<ParameterEntry>(names[j], vars[i]->getType(), m));
      }
    }
  }

  const std::vector<std::shared_ptr<Entry>>& getParams()
  {
    return parameters;
  }

  std::string getName()
  {
    return name;
  }

  virtual llvm::Value* compile() override;

private:
  std::string name;
  std::vector<std::shared_ptr<Entry>> parameters;
};

class Decl : public AST {
public:
  Decl(Header *t1)
  {
    t = t1->getType();
    name = t1->getName();
    parameters = t1->getParams();
  }

  virtual void sem(std::shared_ptr<SymTable> table) override
  {
    std::shared_ptr<Entry> e = table->lookup(name, CUR, false);
    if(e!=nullptr)
    {
      std::cerr<<"Dublicate identifier "<<name<<'\n';
      exit(1);
    }
    std::shared_ptr<Entry> fun = std::make_shared<FunctionEntry>(name, t, true);
    table->insert(fun);
    table->openScope(fun);
    for(unsigned i=0; i<parameters.size(); ++i)
    {
      // table->insert(parameters[i]);
      table->addParam(parameters[i]);
    }
    table->closeScope();
    func = fun;
  }

  virtual llvm::Value* compile() override;

private:
  std::string name;
  std::vector<std::shared_ptr<Entry>> parameters, prev_params;
  std::shared_ptr<Entry> func;
};

class Func : public AST {
public:
  Func(Header *t1, std::vector<AST*> t2, std::vector<AST*> t3)
  {
    main = false;
    declared = false;
    t = t1->getType();
    name = t1->getName();
    parameters = t1->getParams();
    def_list = std::move(t2);
    stmts = std::move(t3);
  }

  void setMain()
  {
    main = true;
  }

  virtual void sem(std::shared_ptr<SymTable> table) override
  {
    std::shared_ptr<Entry> e = table->lookup(name, CUR, false);
    std::shared_ptr<Entry> fun;
    if(e!=nullptr)
    {
      declared = true;
      if(e->getEntryType()!=FUN)
      {
        std::cerr<<"Dublicate identifier "<<name<<'\n';
        exit(1);
      }
      if(parameters.size()!=e->getParameters().size())
      {
        std::cerr<<"Definition for function "<<name<<" does not match the declaration\n";
        exit(1);
      }
      std::vector<std::shared_ptr<Entry>> p = e->getParameters();
      for(unsigned i=0; i<parameters.size(); ++i)
      {
        if(!equals(parameters[i]->getType(), p[i]->getType()) || parameters[i]->getPassMode()!=p[i]->getPassMode())
        {
          std::cerr<<"Definition for function "<<name<<" does not match the declaration\n";
          exit(1);
        }
      }
      fun = e;
      fun->UnDecl();
      table->openScope(fun);
      for(unsigned i=0; i<parameters.size(); ++i)
        table->insert(parameters[i]);
    }
    else
    {
      fun = std::make_shared<FunctionEntry>(name, t, false);
      table->insert(fun);
      table->openScope(fun);
      for(unsigned i=0; i<parameters.size(); ++i)
      {
        table->insert(parameters[i]);
        table->addParam(parameters[i]);
      }
    }
    for(unsigned i=0; i<def_list.size(); ++i)
      def_list[i]->sem(table);
    for(unsigned i=0; i<stmts.size(); ++i)
      stmts[i]->sem(table);
    e = table->lookup(name, ALL, false);
    for(auto it: e->getPrevScopeVars())
      prev_params.push_back(std::make_shared<ParameterEntry>(it->getId(), it->getType(), REF));
    table->closeScope();
    if(main)
    {
      if(parameters.size()!=0)
      {
        std::cerr<<"Main function "<<name<<" must not take any arguments\n";
        exit(1);
      }
      std::shared_ptr<Type> v(new VoidType);
      if(!equals(t, v, false))
      {
        std::cerr<<"Main function "<<name<<" must not have return type\n";
        exit(1);
      }
    }
    func = fun;
  }

  virtual llvm::Value* compile() override;

private:
  bool main, declared;
  std::string name;
  std::vector<std::shared_ptr<Entry>> parameters, prev_params;
  std::vector<AST*> def_list, stmts;
  std::shared_ptr<Entry> func;
};

#endif