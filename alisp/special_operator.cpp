//
//  special_operator.cpp
//  alisp
//
//  Created by Matt Gadda on 10/1/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#include <iostream>
#include "special_operator.h"
#include "cons.h"

SpecialOperator::SpecialOperator(std::string name, Object *(*internalFun)(Cons*,Environment*)) 
  : internalFun_(internalFun), Callable(name) {
}

SpecialOperator *SpecialOperator::print(std::ostream &os) {
  os << "#<SPECIAL-OPERATOR " << name_ << ">";
  return this;
}

Object *SpecialOperator::call(Cons *args, Environment *env) {  
  return internalFun_(args, env);
}

bool SpecialOperator::mark() {
  if (Object::mark()) {
    if (argNames_)
      argNames_->mark();
    
    return true;
  }
  return false;
}