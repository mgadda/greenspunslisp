//
//  cons.h
//  alisp
//
//  Created by Matt Gadda on 9/25/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#ifndef alisp_cons_h
#define alisp_cons_h

#include <iostream>
#include "object.h"

class Cons : public Object {
	Object *car_;
	Object *cdr_;
  
public:  
  Cons() : car_(NULL), cdr_(NULL) {}
  
	Cons(Object *car);
	Cons(Object *car, Object *cdr);
	
	virtual const char *type();
  Object *print(std::ostream &os);
  
  Object *car();
  void setCar(Object *car);
  
  Object *cdr();
  void setCdr(Object *cdr);
  
  Cons *map(Object *(^block)(Object *));
  void each(void (^block)(Object *));
  
  size_t length(); 
};


#endif
