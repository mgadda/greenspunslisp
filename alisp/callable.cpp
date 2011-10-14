//
//  callable.cpp
//  alisp
//
//  Created by Matt Gadda on 10/1/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//


#include "callable.h"
#include "symbol.h"

Callable::Callable(std::string name) : name_(name) {}

Object *Callable::operator()(Cons *cons, Environment *env) {
  return call(cons, env);
}

void Callable::setName(Symbol* symbol) {
  name_ = symbol->name();
}