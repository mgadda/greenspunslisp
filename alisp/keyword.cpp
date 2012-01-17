//
//  keyword.cpp
//  alisp
//
//  Created by Matt Gadda on 9/26/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#include <iostream>
#include <string>
#include "keyword.h"

Keyword::Keyword(std::string name) : Symbol(name) {
  
}

Object *Keyword::print(std::ostream &os) {
  os << ":" << name();
  return this;
}

bool Keyword::mark() {
  return Object::mark();
}