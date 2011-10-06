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

SpecialOperator::SpecialOperator(Object *(*internalFun)(Cons*,Environment*)) 
  : internalFun_(internalFun) {
}

SpecialOperator *SpecialOperator::print(std::ostream &os) {
  os << "#<SPECIAL-OPERATOR>";
  return this;
}

Object *SpecialOperator::call(Cons *args, Environment *env) {  
  return internalFun_(args, env);
}