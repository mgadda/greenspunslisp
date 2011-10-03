//
//  system.h
//  alisp
//
//  Created by Matt Gadda on 10/1/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#ifndef alisp_system_h
#define alisp_system_h

class Object;
class Cons;
class Environment;
class Int;

void initSystem();

#pragma mark Special Operators

Object *quote(Cons* args, Environment *env);
Object *setq(Cons* args, Environment *env);
//Int *length(Cons* args, Environment *env);


#endif
