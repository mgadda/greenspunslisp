//
//  macro.h
//  alisp
//
//  Created by Matt Gadda on 10/1/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#ifndef alisp_macro_h
#define alisp_macro_h

#include "object.h"
#include "callable.h"

class Environment;
class Cons;

class Macro : public Callable {
  Cons *argNames_;
  Object *form_;
  // arg list
  
public:
  Macro(std::string name, Cons *lambdaList, Object *form);
  
  virtual const char *type() { return "MACRO"; }
  virtual Macro *print(std::ostream &os);
  
  Object *call(Cons *cons, Environment *env);
  
  virtual bool mark();
};


#endif
