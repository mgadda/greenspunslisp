//
//  object.h
//  alisp
//
//  Created by Matt Gadda on 9/24/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#ifndef alisp_object_h
#define alisp_object_h

#include <iostream>
#include <set>

class Mother;

#define TYPEOF(obj) std::string(((Object*)obj)->type())

class Object {
private:
  bool marked_;
  bool noGC_;
  
public:
  Object();
	virtual const char *type();
  virtual Object *print(std::ostream &os)=0;
  const char *print();
  
  virtual bool mark();
  void clearMark();
  bool marked();

  // nogc objects still participate in the marking process
  // but they will not be collected during the sweep
  void setNoGC(bool noGC);
  
  static void* operator new(size_t size);
  static void operator delete(void *obj);
  
};


#endif
