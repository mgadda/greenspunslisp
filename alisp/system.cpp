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

void initSystem() {
  Symbol *sym;
  Callable *fun;
  // build special operator functions and system functions
  Package &system = Package::system();
  
  // QUOTE
  sym = system.internSymbol("QUOTE");
  system.exportSymbol("QUOTE");
  
  fun = new SpecialOperator(quote);  
  sym->setFunction(fun);
  
  // SETQ
  sym = system.internSymbol("SETQ");
  system.exportSymbol("SETQ");
  
  fun = new SpecialOperator(setq);  
  sym->setFunction(fun);
  
  
  
//  sym = system.internSymbol("LENGTH");
//  fun = new Funct(ion(&length);
  
  //sym->setFunction(fun);
}

Object *quote(Cons* args, Environment *env) {
  if (args->length() > 1) {
    throw "too many arguments for special operator QUOTE:";
  }
  return args->car();
}

Object *setq(Cons* args, Environment *env) {
  // check number of arguemnts, if odd, raise error
  if (args->length() % 2 == 1) 
    throw "SETQ: odd number of arguments";
  
  Symbol *symbol = (Symbol*)args->car();
  Object *value = ((Cons*)args->cdr())->car();
  // extract pairs of values from args
  // bind first to eval of second
  return env->bindVariable(symbol, eval(value, env));
}

//Int *length(Cons* args, Environment *env) {
//  return new Int((int)args->length());
//}