//
//  function.h
//  alisp
//
//  Created by Matt Gadda on 9/28/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#ifndef alisp_function_h
#define alisp_function_h

#include <iostream>
#include "object.h"
#include "callable.h"

class Environment;
class Cons;

class Function : public Callable {
  Object *form_;
  Cons *argNames_;
  
  Object *(*internalFun_)(Cons*,Environment*);
  
  // arg list
  Environment *functionEnv_;
  
public:
  Function(Object *form, Cons *argNames);
  Function(std::string name, Object *(*internalFun_)(Cons*,Environment*));
  
  virtual const char *type() { return "FUNCTION"; }
  virtual Function *print(std::ostream &os);
  
  virtual Object *call(Cons *cons, Environment *env);
  // call matches up various parameter types with args (rest, optional, etc)
  // then evals its form in a lexical environment identical to the one
  // in which function was invoked but with these additional lexical bindings
  
  virtual bool mark();
  
};

#endif
