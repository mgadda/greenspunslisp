//
//  object.h
//  alisp
//
//  Created by Matt Gadda on 9/24/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#ifndef alisp_object_h
#define alisp_object_h

#include <iostream>

#define TYPEOF(obj) std::string(((Object*)obj)->type())

class Object {
  
public:
	virtual const char *type();
  virtual Object *print(std::ostream &os)=0;
};


#endif
