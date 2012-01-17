//
//  macro.cpp
//  alisp
//
//  Created by Matt Gadda on 10/1/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#include <iostream>
#include "macro.h"
#include "symbol.h"
#include "cons.h"

Macro::Macro(std::string name, Cons *lambdaList, Object *form)
  : Callable(name), argNames_(lambdaList), form_(form) {
  requiredArgsCount_ = lambdaList->length();
}

Macro *Macro::print(std::ostream &os) {
  os << "#<MACRO " << name_ << ">";
  return this;
}

Object *Macro::call(Cons *args, Environment *env) {    
  if (!hasMinArgs(args)) throw "EVAL: too few arguments";
  
  // TODO: implement macros
  return Symbol::nil();
}

bool Macro::mark() {
  if (Object::mark()) {
    if (argNames_)
      argNames_->mark();
    
    if (form_)
      argNames_->mark();
    
    return true;
  }
  return false;
}