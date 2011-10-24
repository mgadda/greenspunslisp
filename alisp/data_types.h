//
//  data_types.h
//  alisp
//
//  Created by Matt Gadda on 9/22/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#ifndef alisp_data_types_h
#define alisp_data_types_h

#include <string>
#include <exception>
#include "object.h"

class Package;

class String : public Object {
  std::string value_;
	
public:
	virtual const char *type();
  Object *print(std::ostream &os);
  
	String() {}
  String(const char *value);
	String(std::string value);
	
  std::string value();
};


class Integer : public Object {
	int value_;
	
public:
	virtual const char *type() { return "INTEGER"; }
  Object *print(std::ostream &os) { os << value_; return this; }
  
	Integer() {}
	Integer(int val) : value_(val) {}
  Integer(std::string &str);
  
  int value();
};

class Readtable : public Object {
public:
	virtual const char *type() { return "READTABLE"; }
  Object *print(std::ostream &os) { os << "#<READTABLE>"; return this; }
};

#endif
