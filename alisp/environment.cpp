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
#include "callable.h"

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
  Object *obj = NULL;
  if (variableBindings_.count(symbol))
    obj = variableBindings_[symbol];
  else {
    if (parent_)
      obj = parent_->variableForSymbol(symbol); // lexical value
    else 
      obj = symbol->value(); // global value
  }
  return obj;
}

Callable *Environment::functionForSymbol(Symbol *symbol) {  
  Callable *fun = NULL;
  if(functionBindings_.count(symbol))
    fun = functionBindings_[symbol];
  else {
    if (parent_)
      fun = parent_->functionForSymbol(symbol);
    else
      fun = symbol->function();
  }
  return fun;  
}

bool Environment::mark() {
  if (Object::mark()) {
    if (parent_) {
      parent_->mark();
    }
    
    std::map<Symbol*,Object*>::iterator symbolIt;
    for(symbolIt = variableBindings_.begin(); symbolIt != variableBindings_.end(); symbolIt++) {

      Symbol *sym = (Symbol*)((*symbolIt).first);
      sym->mark();

      Object *obj = (Object*)((*symbolIt).second);
      obj->mark();
    }
    
    std::map<Symbol*,Callable*>::iterator callableIt;
    for(callableIt = functionBindings_.begin(); callableIt != functionBindings_.end(); callableIt++) {
      
      Symbol *sym = (Symbol*)((*callableIt).first);
      sym->mark();
      
      Callable *fun = (Callable*)((*callableIt).second);
      fun->mark();
    }
    

    return true;
  }
  return false;
}