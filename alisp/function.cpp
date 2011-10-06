//
//  function.cpp
//  alisp
//
//  Created by Matt Gadda on 9/28/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#include <iostream>
#include "function.h"
#include "environment.h"
#include "cons.h"
#include "symbol.h"

extern Object *eval(Object* obj, Environment *env);


Function::Function(Object *form, Cons *argNames) : form_(form), argNames_(argNames) {
}

Function::Function(Object *(*internalFun)(Cons*,Environment*)) : internalFun_(internalFun) {
    
}

Function *Function::print(std::ostream &os) {
  if (form_) {
    form_->print(os);
  }
  else if (internalFun_) {
    os << "#<COMPILED-FUNCTION>";
  }
  
  return this;
}

Object *Function::call(Cons *args, Environment *env) {
  functionEnv_ = new Environment(env);
  
  if (internalFun_) {
    return internalFun_(args, functionEnv_);
  }
  
  __block Cons *argNamesPtr = argNames_;
  
  if (args && args->type() == std::string("CONS")) {
    args->each(^(Object *arg) {

      if (!NILP(argNamesPtr)) {
        functionEnv_->bindVariable((Symbol*)argNamesPtr->car(), arg);
        argNamesPtr = (Cons*)argNamesPtr->cdr();
      }
      else {
        throw "too few arguments"; // we ran out of arg names before args
      }
      
    });
    
    if (NILP(argNamesPtr)) { // we ran out of args before arg names
      throw "too many arguments";
    }
  }
  
  return eval(form_, functionEnv_);
}

bool Function::mark() {
  if (Object::mark()) {
    if (form_)
      form_->mark();
    
    if (argNames_)
      argNames_->mark();
    
    if (functionEnv_)
      functionEnv_->mark();
    return true;
  }
  return false;
}