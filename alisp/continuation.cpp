//
//  continuation.cpp
//  alisp
//
//  Created by Matt Gadda on 10/9/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#include <iostream>
#include "continuation.h"

Continuation::Continuation(jmp_buf &jmp_env, 
                           void (^block)(Object *obj, Environment *env)) 
   : block_(block), jmp_env_(jmp_env), Callable("") {}

Object *Continuation::call(Cons *cons, Environment *env) {
  call(cons, env);
  return NULL; // we'll never reach this line
}

void Continuation::call(Object *obj, Environment *env) {
  block_(obj, env);
  longjmp(jmp_env_, 1);
}

Object *Continuation::print(std::ostream &os) {
  os << "#<CONTINUATION>";
  return this;
}

const char *Continuation::type() { 
  return "CONTINUATION"; 
}