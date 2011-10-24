//
//  evaler.cpp
//  alisp
//
//  Created by Matt Gadda on 9/28/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#include <iostream>
#include "evaler.h"
#include "environment.h"
#include "object.h"
#include "cons.h"
#include "function.h"
#include "data_types.h"
#include "symbol.h"
#include "callable.h"

Cons *evalList(Cons *list, Environment *env) {
  return list->map(^Object *(Object *obj) {
    return eval(obj, env);
  });
}

Object *eval(Object* obj, Environment *env) {
  if (obj->type() == std::string("CONS")) {
    Cons *cons = (Cons*)obj;
    if(cons->car()->type() == std::string("SYMBOL")) {

      // Empty lists always evaluate to NIL
      if (NILP(cons->car()) && NILP(cons->cdr())) {
        return Symbol::nil();
      }

      Symbol *symbol = (Symbol*)cons->car();  
      Callable &fun = *env->functionForSymbol(symbol);
      
      if (!&fun) {
        throw "EVAL: not a function name";
      }
      
      Cons *args = (Cons*)cons->cdr();

      if (fun.type() == std::string("SPECIAL_OPERATOR")) {
        return fun(args, env);
      }
      else if (fun.type() == std::string("MACRO")) {
        return fun(args, env);
      }
      else if (fun.type() == std::string("FUNCTION")) {
        Cons* evaldArgs = NULL;
        if(cons->cdr()->type() == std::string("CONS")) {
          evaldArgs = evalList((Cons*)cons->cdr(), env);
        }
        
        return fun(evaldArgs, env);
      }
      
      // this symbol is the name of an operator
      // and the form is either a special, a macro form, or a function form (in that order)
      // depending on the function binding of the operator in the current lexical environment
      // if the operator is neither a special operator nor a macro name,
      // it is assumed to be a function name (even if there's no definition for it)
      
      
      // if symbol names a function in lexical env
      // form represents function form and cdr of list contains the forms which 
      // when evaluated will supply the arguments passed to the function
      
      
      
    }
    else if (cons->car()->type() == std::string("CONS")) {
      // must be lambda form (lambda lambda-list body*)
      // treat identically to (funcall #'(lambda lambda-list body*))
      Cons *lambdaForm = (Cons*)cons->car();
      Function *lambdaFun = (Function*)eval(lambdaForm, env);
      
      Cons* evaldArgs = NULL;
      if(cons->cdr()->type() == std::string("CONS")) {
        evaldArgs = evalList((Cons*)cons->cdr(), env);
      }
      
      if (lambdaFun->type() == std::string("FUNCTION")) 
        return lambdaFun->call(evaldArgs, env);
      //return eval(cons->car(), env); // this is temporary      
      
    }
    else { // self-evaluating object, can't be turned into a function
      throw "not a function name";
    }
  } 
  else { // not a list, must be symbol or self-evaluating object
    if (obj->type() == std::string("SYMBOL")) {
      Object* value = env->variableForSymbol((Symbol*)obj);
      if (!value) {

        std::string error("variable ");
        error += ((Symbol*)obj)->name();
        error += " has no value";
        throw error.c_str();

      }
      return value;
    }
    else { //self-evaluating
      return obj;
    }
  }
  return NULL;
}
