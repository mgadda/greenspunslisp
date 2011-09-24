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

class Object {
  
public:
	virtual const char *type();
};

class Cons : public Object {
	Object *car_;
	Object *cdr_;
  
public:  
  Cons() {}
  
	Cons(Object *car);
	Cons(Object *car, Object *cdr);
	
	virtual const char *type();

  Object *car();
  void setCar(Object *car);
  
  Object *cdr();
  void setCdr(Object *cdr);

};

class String : public Object {
  std::string value_;
	
public:
	virtual const char *type();
	
	String() {}
  String(const char *value);
	String(std::string value);
	
};

class Symbol : public Object {
  std::string name_;
  
  static Symbol *nil_;
  static Symbol *t_;

protected:
  
  Symbol(std::string name);

public:
  virtual const char *type();
  
  std::string &name();
  static Symbol *nil();  
  static Symbol *t();
  
  // TODO: make this function take a package
  // and then lookup or construct and return symbol reference
  static Symbol *symbolWithString(std::string name);

};

class Int : public Object {
	int value_;
	
public:
	virtual const char *type() { return "INT"; }
	
	Int() {}
	Int(int val) {
		value_ = val;
	}
};

class Readtable : public Object {
public:
	virtual const char *type() { return "READTABLE"; }
	
};

#endif
