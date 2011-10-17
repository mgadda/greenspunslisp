//
//  callable.cpp
//  alisp
//
//  Created by Matt Gadda on 10/1/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//


#include "callable.h"
#include "symbol.h"
#include "cons.h"

Callable::Callable(std::string name) : name_(name) {}

Object *Callable::operator()(Cons *cons, Environment *env) {
  return call(cons, env);
}

void Callable::setName(Symbol* symbol) {
  name_ = symbol->name();
}

bool Callable::hasMinArgs(Cons *args) {
  return (requiredArgsCount_ == 0) ||
          (args && 
           args->type() == std::string("CONS") && 
           args->length() >= requiredArgsCount_);
}