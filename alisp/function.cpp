//
//  function.cpp
//  alisp
//
//  Created by Matt Gadda on 9/28/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#include <iostream>
#include "function.h"
#include "environment.h"
#include "cons.h"
#include "symbol.h"

extern Object *eval(Object* obj, Environment *env);

// TODO: name should probably be a symbol or possibly a keyword, but the spec
// is kind of fuzzy on the matter
Function::Function(Object *form, Cons *argNames) 
  : form_(form), argNames_(argNames), Callable(":LAMBDA") {
  requiredArgsCount_ = argNames_->length();
}

Function::Function(std::string name, Object *(*internalFun)(Cons*,Environment*), size_t requiredArgsCount) 
  : internalFun_(internalFun), Callable(name) {
  requiredArgsCount_ = requiredArgsCount;
}

Function *Function::print(std::ostream &os) {
  if (form_) {
    os << "#<FUNCTION " << name_ << " ";
    argNames_->print(os);
    os << " ";
    form_->print(os);
    os << ">";
  }
  else if (internalFun_) {
    os << "#<SYSTEM-FUNCTION " << name_ << ">";
  }
  
  return this;
}

Object *Function::call(Cons *args, Environment *env) {
  if (!hasMinArgs(args)) throw std::string("EVAL: too few arguments for ") + name_;
  
  functionEnv_ = env; //new Environment(env);
  
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

    // TODO: this isn't correct, fix it.
//    if (NILP(argNamesPtr)) { // we ran out of args before arg names
//      throw "too many arguments";
//    }
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
