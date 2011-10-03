//
//  environment.cpp
//  alisp
//
//  Created by Matt Gadda on 9/27/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#include <iostream>
#include "environment.h"
#include "symbol.h"

class Callable;

Object *Environment::print(std::ostream &os) {
  os << "#<ENVIRONMENT >";
  return this;
}
Environment::Environment(Environment *parent) {
  parent_ = parent;  
}

Object *Environment::bindVariable(Symbol* symbol, Object* variable) {
  return variableBindings_[symbol] = variable;
}
Callable *Environment::bindFunction(Symbol* symbol, Callable* function) {
  return functionBindings_[symbol] = function;  
}

Object *Environment::variableForSymbol(Symbol *symbol) {
  Object *obj = variableBindings_[symbol];
  if (!obj) {
    if (parent_)
      obj = parent_->variableForSymbol(symbol); // lexical value
    else 
      obj = symbol->value(); // global value
  }
  return obj;
}

Callable *Environment::functionForSymbol(Symbol *symbol) {
  Callable *fun = functionBindings_[symbol];
  if (!fun) {
    if (parent_)
      fun = parent_->functionForSymbol(symbol);
    else
      fun = symbol->function();
  }
  return fun;  
}
