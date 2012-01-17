//
//  system.h
//  alisp
//
//  Created by Matt Gadda on 10/1/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#ifndef alisp_system_h
#define alisp_system_h

#include "package.h"

class Object;
class Cons;
class Environment;
class Integer;

void initSystem();

#define LISPFUN(name) Object *name(Cons* args, Environment *env)

#pragma mark Special Operators

LISPFUN(quote);
LISPFUN(setq);
LISPFUN(progn);
LISPFUN(let);
LISPFUN(letStar);
LISPFUN(If);
LISPFUN(block);
LISPFUN(returnFrom);
LISPFUN(function);
LISPFUN(lambda);
LISPFUN(gc);
LISPFUN(eval);

#pragma mark System Functions
LISPFUN(putd);
LISPFUN(length);
LISPFUN(car);
LISPFUN(cdr);
LISPFUN(cons);
LISPFUN(plus);
LISPFUN(intEqual);
LISPFUN(list);
LISPFUN(listStar);
LISPFUN(funcall);
LISPFUN(findSymbol);
LISPFUN(exportSymbol);

#pragma mark Accessors
LISPFUN(symbol_function);

#pragma mark System Macros

// TODO: define these macros in lisp, not in code
// (defmacro setf ...)
// (defmacro defun ...)
//LISPFUN(setf);
//LISPFUN(defun);

namespace {
  void bindSymbolToFunc(Package &package, std::string name, Object *(*funcPtr)(Cons*,Environment*), size_t minRequiredArgs,bool shouldExportSymbol);
  void bindSymbolToSpecialOperator(Package &package, std::string name, Object *(*funcPtr)(Cons*,Environment*), size_t minRequiredArgs, bool shouldExportSymbol);
}

#endif
