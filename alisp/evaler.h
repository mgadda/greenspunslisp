//
//  evaler.h
//  alisp
//
//  Created by Matt Gadda on 9/28/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#ifndef alisp_evaler_h
#define alisp_evaler_h

#include "environment.h"
#include "object.h"
#include "cons.h"
//
//class Cons;
//class Environment;
//class Object;

Cons *evalList(Cons *list, Environment *env);
Object *eval(Object* obj, Environment *env);

#endif
