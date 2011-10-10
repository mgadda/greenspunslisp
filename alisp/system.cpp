//
//  system.cpp
//  alisp
//
//  Created by Matt Gadda on 10/1/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#include <iostream>
#include <setjmp.h>

#include "system.h"
#include "package.h"
#include "symbol.h"
#include "special_operator.h"
#include "cons.h"
#include "environment.h"
#include "data_types.h"
#include "function.h"

extern Object *eval(Object* obj, Environment *env);

namespace  {
  void bindSymbolToFunc(Package &package, std::string name, Object *(*funcPtr)(Cons*,Environment*)) {
    Symbol *sym = package.internSymbol(name);
    package.exportSymbol(name);
    
    Callable *fun = new Function(funcPtr);  
    sym->setFunction(fun);
  }

  void bindSymbolToSpecialOperator(Package &package, std::string name, Object *(*funcPtr)(Cons*,Environment*)) {
    Symbol *sym = package.internSymbol(name);
    package.exportSymbol(name);
    
    Callable *fun = new SpecialOperator(funcPtr);  
    sym->setFunction(fun);
  }

};
                        
void initSystem() {
  Package &system = Package::system();
  
  // Special Operators
  bindSymbolToSpecialOperator(system, "QUOTE", quote);
  bindSymbolToSpecialOperator(system, "SETQ", setq);
  bindSymbolToSpecialOperator(system, "PROGN", progn);
  bindSymbolToSpecialOperator(system, "LET", let);
  bindSymbolToSpecialOperator(system, "LET*", letStar);
  bindSymbolToSpecialOperator(system, "IF", If);
  bindSymbolToSpecialOperator(system, "BLOCK", block);
  bindSymbolToSpecialOperator(system, "RETURN-FROM", returnFrom);
  
  // System Functions
  bindSymbolToFunc(system, "LENGTH", length);
  bindSymbolToFunc(system, "CAR", car);
  bindSymbolToFunc(system, "CDR", cdr);
  bindSymbolToFunc(system, "CONS", cons);
}

#pragma mark Special Operators

Object *quote(Cons* args, Environment *env) {
  if (args->length() > 1) {
    throw "too many arguments for special operator QUOTE:";
  }
  Object *first = args->car();
  if (first->type() == std::string("CONS")) {
    Cons* list = (Cons*)args->car();
      
    if (NILP(list->car()) && NILP(list->cdr())) {
      return Symbol::nil();
    }

  }

  return first;
}

Object *setq(Cons* args, Environment *env) {
  // check number of arguemnts, if odd, raise error
  if (args->length() % 2 == 1) {
    char *errMsg;
    asprintf(&errMsg, "SETQ: odd number of arguments: %s", ((Object*)args)->print());
    throw errMsg;
  }
  
  // TODO: SETQ should bind multiple pairs
  Symbol *symbol = (Symbol*)args->car();
  Object *value = ((Cons*)args->cdr())->car();
  // extract pairs of values from args
  // bind first to eval of second
  return env->bindVariable(symbol, eval(value, env));
}

Object *progn(Cons* args, Environment *env) {
  __block Object *last = Symbol::nil();
  args->each(^(Object *obj) {
    last = eval(obj, env);
  });
  return last;
}

Object *let(Cons *args, Environment *env) {
  // same as let, but we have to eval all of the binding values
  // before binding them to their symbols
  /*
   (let* ((var1 init-form-1)
   (var2 init-form-2)
   ...
   (varm init-form-m))
   declaration1
   declaration2
   ...
   declarationp
   form1
   form2
   ...
   formn)
   */
  
  Environment *letEnv = new Environment(env);
  
  // Evaluate binding values  
  if(args->car()->type() == std::string("CONS")) {
    Cons* bindings = (Cons*)args->car();
    
    Cons* bindingValues = bindings->map(^Object *(Object *obj) {
      if (obj->type() == std::string("CONS")) { 
        Cons *binding = (Cons*)obj;
        size_t len = binding->length();
        
        if (len == 0) {
          throw "NIL is a constant, may not be used as a variable";
        }
        else if (len == 1) { // (let (... (a) ...) ...) -> NIL
          return Symbol::nil();
        }
        else if (len == 2) { // (let (... (a 10) ...) ...) -> eval(10)
          if (binding->car()->type() != std::string("SYMBOL"))
            throw "LET: illegal variable specification";
          return eval(((Cons*)binding->cdr())->car(), env);
        }
        else {
          throw "LET: illegal variable specification";
        }
        
      }
      else // (let (... a ...) ...) -> NIL
        return Symbol::nil();
    });
    
    __block Cons* bindingValuesPtr = bindingValues;
    
    // Bind values to symbols
    bindings->each(^(Object *obj) {
      
      if (obj->type() == std::string("CONS")) { 
        
        // (let ((a)) ...) OR (let ((a 10)) ...)
        
        Cons *binding = (Cons*)obj;
        size_t len = binding->length();
        
        if (len == 0) {
          throw "NIL is a constant, may not be used as a variable";
        }
        else if (len == 1) { // (let (... (a) ...) ...) 
          letEnv->bindVariable((Symbol*)binding->car(), bindingValuesPtr->car());
        }
        else if (len == 2) { // (let (... (a 10) ...) ...)
          if (binding->car()->type() != std::string("SYMBOL"))
            throw "LET: illegal variable specification";
          letEnv->bindVariable((Symbol*)binding->car(), bindingValuesPtr->car());
        }
        else {
          throw "LET: illegal variable specification";
        }
      }
      else { 
        // (let (... a ...) ...)
        if (obj->type() != std::string("SYMBOL"))
          throw "LET: illegal variable specification";
        letEnv->bindVariable((Symbol*)obj, bindingValuesPtr->car());        
      }
      
      bindingValuesPtr = (Cons*)bindingValuesPtr->cdr();
    });
  }
  
  return progn((Cons*)args->cdr(), letEnv);
  
}

Object *letStar(Cons *args, Environment *env) {
  /*
   (let* ((var1 init-form-1)
          (var2 init-form-2)
          ...
          (varm init-form-m))
     declaration1
     declaration2
     ...
     declarationp
     form1
     form2
     ...
     formn)
   */
  
  Environment *letEnv = new Environment(env);
  
  // Evaluate and bind
  if(args->car()->type() == std::string("CONS")) {
    Cons* bindings = (Cons*)args->car();
    bindings->each(^(Object *obj) {
      if (obj->type() == std::string("CONS")) { 
        
        // (let ((a)) ...) OR (let ((a 10)) ...)
        
        Cons *binding = (Cons*)obj;
        size_t len = binding->length();
        
        if (len == 0) {
          throw "NIL is a constant, may not be used as a variable";
        }
        else if (len == 1) { // (let (... (a) ...) ...) 
          letEnv->bindVariable((Symbol*)binding->car(), Symbol::nil());
        }
        else if (len == 2) { // (let (... (a 10) ...) ...)
          if (binding->car()->type() != std::string("SYMBOL"))
            throw "LET: illegal variable specification";
          letEnv->bindVariable((Symbol*)binding->car(), eval(((Cons*)binding->cdr())->car(), letEnv));
        }
        else {
          throw "LET: illegal variable specification";
        }
      }
      else { 
        // (let (... a ...) ...)
        if (obj->type() != std::string("SYMBOL"))
          throw "LET: illegal variable specification";
        letEnv->bindVariable((Symbol*)obj, Symbol::nil());        
      }
    });
  }
  
  return progn((Cons*)args->cdr(), letEnv);
}

Object *If(Cons *args, Environment *env) {

  if (args->length() < 2) {
    throw "EVAL: too few parameters for special operator IF:";
  }
  Object *testFormValue = eval(args->car(), env);
  if (testFormValue != Symbol::nil()) {
    return eval((*args)[1], env);
  }
  else {
    if (args->length() == 3) {      
      return eval((*args)[2], env);
    }
    else
      return Symbol::nil();
  }
}

class Continuation : public Callable {
private:
  void (^block_)(Object *obj, Environment *env);
  jmp_buf &jmp_env_;
  
public:
  Continuation(jmp_buf &jmp_env, void (^block)(Object *obj, Environment *env)) : block_(block), jmp_env_(jmp_env) {}
  
  virtual Object *call(Cons *cons, Environment *env) {
    call(cons, env);
    return NULL; // we'll never reach this line
  }

  void call(Object *obj, Environment *env) {
    block_(obj, env);
    longjmp(jmp_env_, 1);
  }
  
  virtual Object *print(std::ostream &os) {
    os << "#<CONTINUATION>";
    return this;
  }
  
  virtual const char *type() { return "CONTINUATION"; }

};

Object *block(Cons *args, Environment *env) {
  if (args->length() == 0)
    throw "EVAL: too few parameters for special operator BLOCK:";

  if ((*args)[0]->type() != std::string("SYMBOL"))
    throw "BLOCK: __ is not a symbol";
  
  Symbol* blockName = (Symbol*)(*args)[0];
  
  Environment *blockEnv = new Environment(env);

  __block Object* returnValFromBlock = NULL;
  
  jmp_buf jmp_env;
  if (setjmp(jmp_env) == 0) {
    
    Continuation *c = new Continuation(jmp_env, ^void (Object *obj, Environment *env) {
      returnValFromBlock = obj;
    });  
    blockEnv->bindVariable(blockName, c);
        
    return progn((Cons*)args->cdr(), blockEnv);
  }

  return returnValFromBlock;
  
}

Object *returnFrom(Cons *args, Environment *env) {
  size_t len = args->length();
  
  if (len == 0)
    throw "block name missing";
  
  if (len > 2) {
    throw "EVAL: too many parameters for special operator RETURN-FROM:";
  }
  
  Object *first = args->car();
  
  if (first->type() != std::string("SYMBOL")) {
    throw "RETURN-FROM: ___ is not a symbol";
  }
  
  Continuation *c = (Continuation*)(env->variableForSymbol((Symbol*)first));
  
  // we don't return from here, because continuation switches contexts
  if (len == 1)
    c->call(Symbol::nil(), env);
  else
    c->call((*args)[1], env); 
  
  return Symbol::nil(); // we'll never reach this point unless longjmp fails
}

#pragma mark System Functions

Object *length(Cons* args, Environment *env) {
  return new Integer((int)args->length());
}

Object *car(Cons* args, Environment *env) {
  Object *list = args->car(); // first argument
  
  if (NILP(args->car()))
    return Symbol::nil();
  
  if (list->type() != std::string("CONS")) {
    char *errMsg;
    asprintf(&errMsg, "CAR: %s is not a list", list->print());
    throw errMsg;
  }
  return ((Cons*)list)->car();
}

Object *cdr(Cons* args, Environment *env) {
  Object *list = args->car(); // first argument
  
  if (NILP(args->car()))
    return Symbol::nil();
  
  if (list->type() != std::string("CONS")) {
    char *errMsg;
    asprintf(&errMsg, "CDR: %s is not a list", list->print());
    throw errMsg;
  }
  return ((Cons*)list)->cdr();
}

Object *cons(Cons* args, Environment *env) {
  if (args->length() > 2)
    throw "too many arguments given to CONS:";
  if (args->length() < 2)
    throw "too few arguments given to CONS:";
  
  return new Cons((*args)[0], (*args)[1]);
}