//
//  environment.h
//  alisp
//
//  Created by Matt Gadda on 9/27/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#ifndef alisp_environment_h
#define alisp_environment_h

#include <map>
#include "object.h"

class Symbol; 
class Callable;

class Environment : public Object {
  Environment *parent_;
  std::map<Symbol*,Object*> variableBindings_;
  std::map<Symbol*,Callable*> functionBindings_;
  std::map<Symbol*,Callable*> macroBindings_;
  
  static Environment *initial_;
  
public:
  virtual const char *type() { return "ENVIRONMENT"; }
  virtual Object *print(std::ostream &os);
  
  Environment(Environment *parent);
  
  Object *bindVariable(Symbol* symbol, Object* variable);
  void unbindVariable(Symbol* symbol);
  
  Object *variableForSymbol(Symbol *symbol);
  
  virtual bool mark();
  static Environment &initial();
};

#endif
