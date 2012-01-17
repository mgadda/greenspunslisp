//
//  symbol.cpp
//  alisp
//
//  Created by Matt Gadda on 9/25/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#include <iostream>
#include "symbol.h"
#include "package.h"
#include "cons.h"
#include "callable.h"

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
void Symbol::setFunction(Callable *value) { function_ = value; }

Callable *Symbol::macro() { return macro_; }
void Symbol::setMacro(Callable *value) { macro_ = value; }

bool Symbol::mark() {
  if (Object::mark()) {
    if (package_)
      package_->mark();
    
    if (value_)
      value_->mark();
    
    if (function_)
      function_->mark();

    if (macro_)
      macro_->mark();

    if (propertyList_)
      propertyList_->mark();
    
    nil_->mark();
    t_->mark();
    
    return true;
  }
  return false;
}