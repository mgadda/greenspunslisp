//
//  object.cpp
//  alisp
//
//  Created by Matt Gadda on 10/3/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#include <iostream>
#include <string>
#include <sstream>
#include "object.h"
#include "mother.h"

Object::Object() : marked_(false), noGC_(false) {
}

const char *Object::type() {
  return "OBJECT";
}

const char *Object::print() {
  std::ostringstream oss;
  print(oss);
  return oss.str().c_str();
}

bool Object::mark() {
  if (!marked_) {
    std::cout << "Marking " << print() << std::endl;
    marked_ = true;
    return true;
  }
  return false;
}

void Object::clearMark() {
  std::cout << "Clearing mark on " << print() << std::endl;
  marked_ = false;
}

bool Object::marked() {
  return marked_ || noGC_;
}

void Object::setNoGC(bool noGC) {
  noGC_ = noGC;
}

void* Object::operator new(size_t size) {
  std::cout << "allocated " << size << " bytes." << std::endl;  
  return (void*)Mother::instance().newObject(size);
}

void Object::operator delete(void *obj) {
  // noop because deallocation is handled by Mother through free()
  // downside? class destructor not invoked automatically, can Mother do this?
  __noop;
  //free(obj); 
}
