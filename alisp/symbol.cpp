//
//  symbol.cpp
//  alisp
//
//  Created by Matt Gadda on 9/25/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#include <iostream>
#include "symbol.h"
#include "package.h"
#include "cons.h"

class Callable;

Symbol::Symbol(std::string name) { 
  name_ = name;
}

Object *Symbol::print(std::ostream &os) {
  os << name_;
  return this;
}

const char *Symbol::type() {
  return "SYMBOL";
}

Symbol *Symbol::nil_;
Symbol *Symbol::t_;

Symbol *Symbol::nil() {
  if (!nil_) {
    nil_ = Package::common_lisp().internSymbol("NIL");
  }
  nil_->setValue(nil_);
  Package::common_lisp().exportSymbol("NIL");
  
  return nil_;
}

Symbol *Symbol::t() {
  if(!t_) {
    t_ = Package::common_lisp().internSymbol("T");
  }
  t_->setValue(t_);
  Package::common_lisp().exportSymbol("T");
  
  return t_;
}

std::string &Symbol::name() {
  return name_;
}

Package *Symbol::package() {
  if (!package_) {
    package_ = (Package*)Symbol::nil(); // for uninterned symbols
  }
  return package_;
}

void Symbol::setPackage(Object *package) {
  package_ = (Package*)package; // could be nil symbol
}

Cons *Symbol::propertyList() {
  if (!propertyList_) {
    propertyList_ = new Cons(Symbol::nil(), Symbol::nil());
  }
  return propertyList_;
}

Object *Symbol::value() { return value_; }
void Symbol::setValue(Object *value) { value_ = value; }

Callable *Symbol::function() { return function_; }
void Symbol::setFunction(Callable *function) { function_ = function; }


//Symbol *Symbol::symbolWithStringInPackage(std::string name, Package &package) {
//  // TODO: replace this with proper symbol lookup
//  // if symbol has home package, look there first
//  // look for symbol name in specified package (presumed to be current)
//  // then look for symbol in other packages?
//  // failing that, create new symbol in specified package (presumed to be current)
//
//  if (name == "NIL" || name == "nil") {
//    return Symbol::nil();
//  }
//  else if (name == "T" || name == "t") {
//    return Symbol::t();
//  }
//  else {
//    // intern new symbols
//    return package.registerSymbol(new Symbol(name));
//  }
//}
