//
//  data_types.cpp
//  alisp
//
//  Created by Matt Gadda on 9/23/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#include <iostream>
#include "data_types.h"
#include "package.h"
#include "symbol.h"

#pragma mark String

String::String(const char *value) {
  value_ = value;
}
String::String(std::string value) {
  value_ = value;  
};

const char *String::type() { 
  return "STRING"; 
}

Object *String::print(std::ostream &os) {
  // TODO: make string escaping more efficient
  std::string escaped;
  escaped += '"';
  
  std::size_t n = value_.length();
  
  for(std::size_t i=0; i < n; i++) {
    if (value_[i] == '"') {
      escaped += '\\';
    }
    escaped += value_[i];
  }
  escaped += '"';
  
  os << escaped;
  return this;
}

#pragma mark Integer

Integer::Integer(std::string &str) {
  value_ = atoi(str.c_str());
}
