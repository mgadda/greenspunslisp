//
//  data_types.cpp
//  alisp
//
//  Created by Matt Gadda on 9/23/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#include <iostream>
#include "data_types.h"

#pragma mark Object

const char *Object::type() {
  return "OBJECT";
}

#pragma mark Cons

Cons::Cons(Object *car) { 
  car_ = car;
}	

Cons::Cons(Object *car, Object *cdr) {
  car_ = car;
  cdr_ = cdr;
}

const char *Cons::type() { 
  return "CONS"; 
}

Object *Cons::car() { 
  return car_; 
}
void Cons::setCar(Object *car) { 
  car_ = car; 
}

Object *Cons::cdr() { 
  return cdr_; 
}

void Cons::setCdr(Object *cdr) { 
  cdr_ = cdr; 
}

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

#pragma mark Symbol

Symbol::Symbol(std::string name) { 
  name_ = name; 
}

const char *Symbol::type() {
  return "SYMBOL";
}

Symbol *Symbol::nil_;
Symbol *Symbol::t_;

Symbol *Symbol::nil() {
  if (!nil_) {
    nil_ = new Symbol("NIL");
  }
  return nil_;  
}

Symbol *Symbol::t() {
  if(!t_) {
    t_ = new Symbol("T");
  }
  return t_;
}

std::string &Symbol::name() {
  return name_;
}

Symbol *Symbol::symbolWithString(std::string name) {
  if (name == "NIL" || name == "nil") {
    return Symbol::nil();
  }
  else if (name == "T" || name == "t") {
    return Symbol::t();
  }
  else {
    return new Symbol(name);
  }
}

