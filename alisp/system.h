//
//  system.h
//  alisp
//
//  Created by Matt Gadda on 10/1/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#ifndef alisp_system_h
#define alisp_system_h

#include "package.h"

class Object;
class Cons;
class Environment;
class Integer;

void initSystem();

#pragma mark Special Operators

Object *quote(Cons* args, Environment *env);
Object *setq(Cons* args, Environment *env);
Object *progn(Cons* args, Environment *env);
Object *let(Cons *args, Environment *env);
Object *letStar(Cons *args, Environment *env);
Object *If(Cons *args, Environment *env);
Object *block(Cons *args, Environment *env);
Object *returnFrom(Cons *args, Environment *env);

#pragma mark System Functions
Object *length(Cons* args, Environment *env);
Object *car(Cons* args, Environment *env);
Object *cdr(Cons* args, Environment *env);
Object *cons(Cons* args, Environment *env);

namespace {
  void bindSymbolToFunc(Package &package, std::string name, Object *(*funcPtr)(Cons*,Environment*));
  void bindSymbolToSpecialOperator(Package &package, std::string name, Object *(*funcPtr)(Cons*,Environment*));
}

#endif
