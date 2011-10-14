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
Object *function(Cons *args, Environment *env);
Object *lambda(Cons *args, Environment *env);
Object *gc(Cons *args, Environment *env);
Object *defmacro(Cons* args, Environment *env);

#pragma mark System Functions
Object *putd(Cons *args, Environment *env);
Object *length(Cons* args, Environment *env);
Object *car(Cons* args, Environment *env);
Object *cdr(Cons* args, Environment *env);
Object *cons(Cons* args, Environment *env);
Object *plus(Cons* args, Environment *env);
Object *list(Cons* args, Environment *env);
Object *listStar(Cons* args, Environment *env);
Object *funcall(Cons *args, Environment *env);
Object *macroexpand_1(Cons *args, Environment *env);

#pragma mark Accessors
Object *symbol_function(Cons *args, Environment *env);

#pragma mark System Macros

// TODO: define these macros in lisp, not in code
// (defmacro setf ...)
// (defmacro defun ...)
//Object *setf(Cons *args, Environment *env);
//Object *defun(Cons *args, Environment *env);

namespace {
  void bindSymbolToFunc(Package &package, std::string name, Object *(*funcPtr)(Cons*,Environment*), bool shouldExportSymbol);
  void bindSymbolToSpecialOperator(Package &package, std::string name, Object *(*funcPtr)(Cons*,Environment*), bool shouldExportSymbol);
}

#endif
