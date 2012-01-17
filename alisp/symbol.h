//
//  symbol.h
//  alisp
//
//  Created by Matt Gadda on 9/25/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#ifndef alisp_symbol_h
#define alisp_symbol_h

#include <map>
#include "object.h"

#define NILP(obj) ((Symbol*)obj == Symbol::nil())

class Package;
class Cons;
class Callable;

class Symbol : public Object {
  std::string name_; // name cell
  Package *package_; // package cell
  
  Cons *propertyList_;
  
  static Symbol *nil_;
  static Symbol *t_;

public:
  Symbol(std::string name);

  virtual const char *type();
  virtual Object *print(std::ostream &os);
  
  std::string &name();
  
  Package *package();
  void setPackage(Object *package);
  
  Cons *propertyList();
  
  static Symbol *nil();  
  static Symbol *t();
  
  virtual bool mark();  
};

#endif
