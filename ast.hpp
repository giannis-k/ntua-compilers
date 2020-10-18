#ifndef __AST_HPP__
#define __AST_HPP__

#pragma once

#include <cstdlib>
#include <iostream>
#include <map>
#include <vector>
#include <memory>
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

protected:
  std::shared_ptr<Type> t;
};

class Int : public AST {
public:
  Int(int i)
  {
    value=i;
    t=std::make_shared<IntType>();
  }

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

private:
  bool value;
};

class Var : public AST {
public:
  Var(std::string n)
  {
    id=n;
  }

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

private:
  AST *atom, *pos;
};

class Nil : public AST {
public:
  Nil()
  {
    t=std::make_shared<ListType>(nullptr);
  }
};

class BinOp : public AST {
public:
  BinOp(AST* t1, Operator o, AST* t2)
  {
    lhs = t1;
    rhs = t2;
    op = o;
  }

private:
  AST *lhs, *rhs;
  Operator op;
};

class UnOp : public AST {
public:
  UnOp(Operator o, AST* t1)
  {
    rhs = t1;
    op = o;
  }

private:
  AST* rhs;
  Operator op;
};

class New : public AST {
public:
  New(std::shared_ptr<Type> ty, AST* s)
  {
    size = s;
    t=std::make_shared<ArrayType>(ty);
  }

private:
  AST* size;
};

class Skip : public AST {
public:
  Skip(){}
};

class Assign : public AST {
public:
  Assign(AST *t1, AST *t2)
  {
    lhs = t1;
    rhs = t2;
  }

private:
  AST *lhs, *rhs;
}

#endif