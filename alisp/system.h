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

Object *length(Cons* args, Environment *env);
Object *car(Cons* args, Environment *env);
Object *cdr(Cons* args, Environment *env);

namespace {
  void bindSymbolToFunc(Package &package, std::string name, Object *(*funcPtr)(Cons*,Environment*));
  void bindSymbolToSpecialOperator(Package &package, std::string name, Object *(*funcPtr)(Cons*,Environment*));
}

#endif
