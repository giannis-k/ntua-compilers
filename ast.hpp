#pragma once

#include <cstdlib>
#include <iostream>
#include <map>
#include <vector>


class AST {
public:
  virtual ~AST() {}
  virtual void printOn(std::ostream &out) const = 0;
  virtual void sem() const = 0;
};


inline std::ostream& operator<<(std::ostream &out, const AST &t) {
  t.printOn(out);
  return out;
}

class Expr: public AST {
  public:
    virtual void sem() {
      
    }

};


class Stmt: public AST {
  public:
    virtual void sem() {

    };
};

class Id: public Expr {

};

class Func_decl : public Stmt{
  public:
    virtual void sem() const override{}
  private:

};

class Parameters : public Stmt{
  public:
    virtual void sem() const override{}
};

class If: public Stmt {
  public:

};

class For: public Stmt {
  public:
};

class Func_call : public Expr{

};

class BinOp: public Expr {
  public:
};
