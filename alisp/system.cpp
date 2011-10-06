//
//  system.cpp
//  alisp
//
//  Created by Matt Gadda on 10/1/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#include <iostream>
#include "system.h"
#include "package.h"
#include "symbol.h"
#include "special_operator.h"
#include "cons.h"
#include "environment.h"
#include "data_types.h"
#include "function.h"

extern Object *eval(Object* obj, Environment *env);

namespace  {
  void bindSymbolToFunc(Package &package, std::string name, Object *(*funcPtr)(Cons*,Environment*)) {
    Symbol *sym = package.internSymbol(name);
    package.exportSymbol(name);
    
    Callable *fun = new Function(funcPtr);  
    sym->setFunction(fun);
  }

  void bindSymbolToSpecialOperator(Package &package, std::string name, Object *(*funcPtr)(Cons*,Environment*)) {
    Symbol *sym = package.internSymbol(name);
    package.exportSymbol(name);
    
    Callable *fun = new SpecialOperator(funcPtr);  
    sym->setFunction(fun);
  }

};
                        
void initSystem() {
  Package &system = Package::system();
  
  // Special Operators
  bindSymbolToSpecialOperator(system, "QUOTE", quote);
  bindSymbolToSpecialOperator(system, "SETQ", setq);
  
  // System Functions
  bindSymbolToFunc(system, "LENGTH", length);
  bindSymbolToFunc(system, "CAR", car);
  bindSymbolToFunc(system, "CDR", cdr);
}

#pragma mark Special Operators

Object *quote(Cons* args, Environment *env) {
  if (args->length() > 1) {
    throw "too many arguments for special operator QUOTE:";
  }
  return args->car();
}

Object *setq(Cons* args, Environment *env) {
  // check number of arguemnts, if odd, raise error
  if (args->length() % 2 == 1) {
    char *errMsg;
    asprintf(&errMsg, "SETQ: odd number of arguments: %s", ((Object*)args)->print());
    throw errMsg;
  }
  
  // TODO: SETQ should bind multiple pairs
  Symbol *symbol = (Symbol*)args->car();
  Object *value = ((Cons*)args->cdr())->car();
  // extract pairs of values from args
  // bind first to eval of second
  return env->bindVariable(symbol, eval(value, env));
}

#pragma mark System Functions

Object *length(Cons* args, Environment *env) {
  return new Integer((int)args->length());
}

Object *car(Cons* args, Environment *env) {
  Object *list = args->car(); // first argument
  
  if (NILP(args->car()))
    return Symbol::nil();
  
  if (list->type() != std::string("CONS")) {
    char *errMsg;
    asprintf(&errMsg, "CAR: %s is not a list", list->print());
    throw errMsg;
  }
  return ((Cons*)list)->car();
}

Object *cdr(Cons* args, Environment *env) {
  Object *list = args->car(); // first argument
  
  if (NILP(args->car()))
    return Symbol::nil();
  
  if (list->type() != std::string("CONS")) {
    char *errMsg;
    asprintf(&errMsg, "CDR: %s is not a list", list->print());
    throw errMsg;
  }
  return ((Cons*)list)->cdr();
}