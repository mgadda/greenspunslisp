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
  Mother::instance().recordAllocation(this);
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