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
#include "macro.h"
#include "continuation.h"

#include "mother.h"

extern Object *eval(Object* obj, Environment *env);
extern Cons *evalList(Cons *list, Environment *env);

namespace  {
  void bindSymbolToFunc(Package &package, std::string name, Object *(*funcPtr)(Cons*,Environment*), size_t minRequiredArgs, bool shouldExportSymbol) {
    Symbol *sym = package.internSymbol(name);
    package.exportSymbol(name);
    
    Callable *fun = new Function(name, funcPtr, minRequiredArgs);  
    sym->setFunction(fun);
  }

  void bindSymbolToSpecialOperator(Package &package, std::string name, Object *(*funcPtr)(Cons*,Environment*), size_t minRequiredArgs, bool shouldExportSymbol) {
    Symbol *sym = package.internSymbol(name);
    if (shouldExportSymbol)
      package.exportSymbol(name);
    
    Callable *fun = new SpecialOperator(name, funcPtr, minRequiredArgs);  
    sym->setFunction(fun);
  }

};
                        
void initSystem() {
  Package &system = Package::system();
  
  // Special Operators
  bindSymbolToSpecialOperator(system, "QUOTE", quote, 1, true);
  bindSymbolToSpecialOperator(system, "SETQ", setq, 2, true);
  bindSymbolToSpecialOperator(system, "PROGN", progn, 0, true);
  bindSymbolToSpecialOperator(system, "LET", let, 1, true);
  bindSymbolToSpecialOperator(system, "LET*", letStar, 1, true);
  bindSymbolToSpecialOperator(system, "IF", If, 2, true);
  bindSymbolToSpecialOperator(system, "BLOCK", block, 1, true);
  bindSymbolToSpecialOperator(system, "RETURN-FROM", returnFrom, 1, true);
  bindSymbolToSpecialOperator(system, "FUNCTION", function, 1, true);
  bindSymbolToSpecialOperator(system, "LAMBDA", lambda, 1, true);
  bindSymbolToSpecialOperator(system, "GC", gc, 0, false);
  bindSymbolToSpecialOperator(system, "DEFMACRO", defmacro, 2, true);
  bindSymbolToSpecialOperator(system, "EVAL", (Object *(*)(Cons*,Environment*))eval, 1, true);
  
  // System Functions
  bindSymbolToFunc(system, "%PUTD", putd, 2, false);
  bindSymbolToFunc(system, "LENGTH", length, 1, true);
  bindSymbolToFunc(system, "CAR", car, 1, true);
  bindSymbolToFunc(system, "CDR", cdr, 1, true);
  bindSymbolToFunc(system, "CONS", cons, 2, true);
  bindSymbolToFunc(system, "+", plus, 1, true);
  bindSymbolToFunc(system, "=", intEqual, 1, true);
  bindSymbolToFunc(system, "LIST", list, 0, true);
  bindSymbolToFunc(system, "LIST*", listStar, 0, true);
  bindSymbolToFunc(system, "FUNCALL", funcall, 1, true);
  bindSymbolToFunc(system, "MACROEXPAND-1", macroexpand_1, 1, true);
  bindSymbolToFunc(system, "FIND-SYMBOL", findSymbol, 1, true);
  
  // Accessors
  bindSymbolToFunc(system, "SYMBOL-FUNCTION", symbol_function, 1, true);
  
//  bindSymbolToFunc(system, "BACKQUOTE", backquote);
//  bindSymbolToFunc(system, "UNQUOTE", unquote);
//  bindSymbolToFunc(system, "SPLICE", splice);
  
  Package::system().internSymbol("*MACROEXPAND-HOOK*")->setValue(Package::system().resolveInternSymbol("FUNCALL")->function());
  Package::system().exportSymbol("*MACROEXPAND-HOOK*");

}

#pragma mark Special Operators

LISPFUN(quote) {
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

LISPFUN(setq) {
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

LISPFUN(progn) {
  __block Object *last = Symbol::nil();
  args->each(^(Object *obj) {
    last = eval(obj, env);
  });
  return last;
}

LISPFUN(let) {
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

LISPFUN(letStar) {
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

LISPFUN(If) {

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

LISPFUN(block) {
  if (args->length() == 0)
    throw "EVAL: too few parameters for special operator BLOCK:";

  if ((*args)[0]->type() != std::string("SYMBOL"))
    throw "BLOCK: __ is not a symbol";
  
  Symbol* blockName = (Symbol*)(*args)[0];
  
  __block Object* returnValFromBlock = NULL;
  
  jmp_buf jmp_env;
  if (setjmp(jmp_env) == 0) {
    
    Continuation *c = new Continuation(jmp_env, ^void (Object *obj, Environment *env) {
      returnValFromBlock = obj;
    });  
    env->bindVariable(blockName, c);
        
    returnValFromBlock = progn((Cons*)args->cdr(), env);
  }

  env->unbindVariable(blockName);
  
  return returnValFromBlock;
  
}

LISPFUN(returnFrom) {
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

LISPFUN(function) {
  Object *first = args->car();
  Environment *funEnv = NULL;
  
  if (first->type() == std::string("SYMBOL")) {
    if (args->length() == 2 && (*args)[1]->type() == std::string("CONS")) {
      // names a function (does not bind it, nor store it in a function cell) 
      // (function foo (lambda ...) ...) => #<FUNCTION FOO (X) X)
      funEnv = new Environment(env);
      Function *lambdaFunc = (Function*)eval((*args)[1], funEnv);
      if (!lambdaFunc || lambdaFunc->type() != std::string("FUNCTION")) {
        throw "FUNCTION: ___ should be a lambda expression";
      }
      
      lambdaFunc->setName((Symbol*)first);
      return lambdaFunc;
    }
    else {
      // function (function car) => #<SYSTEM-FUNCTION CAR>
      Callable *fun = (Callable*)env->functionForSymbol((Symbol*)first);
      if (!fun || fun->type() != std::string("FUNCTION")) {
        throw "FUNCTION: undefined function ____";
      }
      return fun;
    }
  }
  else if (first->type() == std::string("CONS")) {
    // close over lambda expression
    funEnv = new Environment(env);
    return eval(first, funEnv);  
  }
  else {
    throw "FUNCTION: ____ is not a function name; try using a symbol instead";
  }
}

LISPFUN(lambda) {
  Object *form, *lambdaList;

  lambdaList = (*args)[0];
  form = (*args)[1];
  
  return new Function(form, (Cons*)lambdaList);
}

LISPFUN(gc) {
  Mother::instance().markAndSweep();
  return Symbol::nil();
}

LISPFUN(defmacro) {
  // defmacro is basically what defun would look like if it were defined
  Object *form;
  Cons *lambdaList;
  Symbol *macro_symbol;
  
  macro_symbol = (Symbol*)args->car();
  lambdaList = (Cons*)(*args)[1];
  form = (*args)[2];

  macro_symbol->setFunction(new Macro(macro_symbol->name(), lambdaList, form));
  return macro_symbol->function();
}

LISPFUN(eval) {
  if (args->length() > 1)
    throw "EVAL: too many arguments";
  if (args->length() < 1)
    throw "EVAL: too few arguments";
  
  return eval(args->car(), env);
}

#pragma mark System Functions

LISPFUN(putd) {
  Symbol *sym = (Symbol*)args->car();
  
  Object *value = (*args)[1];
  if (value->type() == std::string("FUNCTION")) {
    sym->setFunction((Callable*)value);
  }
  return value;
}

LISPFUN(length) {
  Cons *list = (Cons*)args->car();
  Integer *integer = new Integer((int)list->length());
  return integer;
}

LISPFUN(car) {
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

LISPFUN(cdr) {
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

LISPFUN(cons) {
  if (args->length() > 2)
    throw "too many arguments given to CONS:";
  if (args->length() < 2)
    throw "too few arguments given to CONS:";
  
  return new Cons((*args)[0], (*args)[1]);
}

LISPFUN(plus) {
  __block int sum = 0;
  args->each(^(Object *obj) {
    if (obj->type() == std::string("INTEGER")) {
      Integer* integer = (Integer*)obj;
      sum += integer->value();
    }    
  });
  return new Integer(sum);
}

LISPFUN(intEqual) {
  Integer *left = (Integer*)args->car();
  Integer *right = (Integer*)(*args)[1];
  
  if (left->type() != std::string("INTEGER"))
    throw left->Object::print() + std::string(" is not a number");
  if (right->type() != std::string("INTEGER"))
    throw right->Object::print() + std::string(" is not a number");
  
  if (left->value() == right->value())
    return Symbol::t();
  else
    return Symbol::nil();
}


LISPFUN(list) {
  Cons *newList = args->map(^Object *(Object *obj) {
    return obj;
  });

  if (NILP(newList->car()) && NILP(newList->cdr())) {
    return Symbol::nil();
  }
  return newList;
}

LISPFUN(listStar) {
  return args->map(^Object *(Object *obj) {
    return obj;
  });
}

LISPFUN(funcall) {
  // funcall applies function to args. If function is a symbol, it is coerced to a function as if by finding its functional value in the global environment.
  Function *fun = NULL;
  
  if (args->car()->type() == std::string("SYMBOL")) {
    fun = (Function*)env->functionForSymbol((Symbol*)args->car());
  }
  else if (args->car()->type() == std::string("FUNCTION")) {
    fun = (Function*)args->car();
    
  }
  
  if (fun && fun->type() == std::string("FUNCTION")) {
    if (args->cdr()->type() == std::string("CONS"))
      return fun->call((Cons*)args->cdr(), env); // (funcall '+ 1 2)
    else
      return fun->call(NULL, env); // (funcall 'callme)
  }
  
  throw "FUNCALL: not a function";
}

LISPFUN(macroexpand_1) {
  /*
   Once macroexpand-1 has determined that the form is a macro form, 
   it obtains an appropriate expansion function for the macro or 
   symbol macro. The value of *macroexpand-hook* is coerced to a 
   function and then called as a function of three arguments: the 
   expansion function, the form, and the env. The value returned 
   from this call is taken to be the expansion of the form.
   */
  
  // determine if the form (macroexpand-1 '(defun foo ...))
  // is a macro form
  // if it is, get the expansion function with
  // (macro-function (car args)) OR really:
  // 
  if (args->car()->type() == std::string("SYMBOL")) {
    throw "not yet implemented: symbol macros";
  }
  
  if (args->car()->type() != std::string("CONS")) {
    throw "MACROEXPAND-1: not a macro form"; // TODO: check if this right
  }
  
  Cons *macroForm = (Cons*)args->car();
  
  if (macroForm->car()->type() != std::string("SYMBOL")) {
    throw "MACROEXPAND-1: not a macro form"; // TODO: check if this right
  }
  
  Symbol *macroSymbol = (Symbol*)macroForm->car();
  
  Callable *expansionFunction = env->functionForSymbol(macroSymbol);
  
  if (!expansionFunction) {
    // TODO: check if this right
    throw "MACROEXPAND-1: could not coerce symbol to macro function";
  }
  
  // Get macroexpansion hook
  Symbol *macroexpand_hook = Package::system().resolveInternSymbol("*MACROEXPAND-HOOK*");
  
  Callable *fun = (Callable*)env->variableForSymbol(macroexpand_hook);
  
  args->setCdr(new Cons(env)); // append env onto end of list
  args = new Cons(expansionFunction, args); // prepend function to front of list
  
  return fun->call(args, env);
}

LISPFUN(findSymbol) {
  if (args->car()->type() != std::string("STRING"))
    throw std::string("EXPORT: argument should be a string not ") + args->car()->print();
  
  String *symbolName = (String*)args->car();
  
  Package *package = NULL;
  if (args->length() >= 2) {
    Object *packageOrPackageName = (*args)[1];
    
    if (packageOrPackageName->type() == std::string("PACKAGE")) {
      package = (Package*)packageOrPackageName;
    }
    else if (packageOrPackageName->type() == std::string("SYMBOL")) {
      package = Package::find(((Symbol*)packageOrPackageName)->name());
    }
    else {
      throw std::string("EXPORT: argument should be a package or a package name, not ") + packageOrPackageName->print();
    }
  }
  else {
    package = (Package*)Package::common_lisp().resolveExternSymbol("*PACKAGE*")->value();    
  }
  
  Symbol *symbol = package->resolveExternSymbol(symbolName->value());
  if (symbol) return symbol;
  
  symbol = package->resolveInternSymbol(symbolName->value());
  if (symbol) return symbol;

  return Symbol::nil();
}

#pragma mark Accessors

LISPFUN(symbol_function) {
  Callable *fun = NULL;
  
  if (args->car()->type() != std::string("SYMBOL")) {
    throw "SYMBOL-FUNCTION: not a symbol";
  }
  
  fun = (Callable*)env->functionForSymbol((Symbol*)args->car());
  
  if (!fun)
    throw "SYMBOL-FUNCTION: no function defined";
  
  return fun;
  
}

#pragma mark System Macros
