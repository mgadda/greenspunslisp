//
//  continuation.h
//  alisp
//
//  Created by Matt Gadda on 10/9/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#ifndef alisp_continuation_h
#define alisp_continuation_h

#include <setjmp.h>
#include "callable.h"

class Continuation : public Callable {
private:
  void (^block_)(Object *obj, Environment *env);
  jmp_buf &jmp_env_;
  
public:
  Continuation(jmp_buf &jmp_env, void (^block)(Object *obj, Environment *env));
  
  virtual Object *call(Cons *cons, Environment *env);  
  void call(Object *obj, Environment *env);
  
  virtual Object *print(std::ostream &os);
  
  virtual const char *type();
  
};

#endif
