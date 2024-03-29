//
//  special_operator.h
//  alisp
//
//  Created by Matt Gadda on 10/1/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#ifndef alisp_special_operator_h
#define alisp_special_operator_h

#include <iostream>
#include "object.h"
#include "callable.h"

class Environment;
class Cons;

class SpecialOperator : public Callable {
  Cons *argNames_;
  Object *(*internalFun_)(Cons*,Environment*);
  
  // arg list
  
public:
  SpecialOperator(std::string name, Object *(*internalFun)(Cons*,Environment*), size_t requiredArgsCount);
  
  virtual const char *type() { return "SPECIAL_OPERATOR"; }
  virtual SpecialOperator *print(std::ostream &os);
  
  Object *call(Cons *cons, Environment *env);
  
  virtual bool mark();
};
#endif
