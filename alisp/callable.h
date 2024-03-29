//
//  callable.h
//  alisp
//
//  Created by Matt Gadda on 10/1/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#ifndef alisp_callable_h
#define alisp_callable_h

#include <iostream>
#include "object.h"

class Environment;
class Cons;
class Symbol;

class Callable : public Object {
protected:
  std::string name_;
  size_t requiredArgsCount_;
  
public:
  Callable(std::string name);
  virtual Object *call(Cons *cons, Environment *env)=0;
  Object *operator()(Cons *cons, Environment *env);
  void setName(Symbol* symbol); 
  bool hasMinArgs(Cons *args);
};

#endif
