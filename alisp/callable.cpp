//
//  callable.cpp
//  alisp
//
//  Created by Matt Gadda on 10/1/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//


#include "callable.h"

Object *Callable::operator()(Cons *cons, Environment *env) {
  return call(cons, env);
}