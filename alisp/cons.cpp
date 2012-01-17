//
//  cons.cpp
//  alisp
//
//  Created by Matt Gadda on 9/25/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#include <iostream>
#include "cons.h"
#include "symbol.h"

Cons::Cons() : car_(Symbol::nil()), cdr_(Symbol::nil()) {
  
}
Cons::Cons(Object *car) : car_(car), cdr_(Symbol::nil()){ 
}	

Cons::Cons(Object *car, Object *cdr) : car_(car), cdr_(cdr) {
}

const char *Cons::type() { 
  return "CONS"; 
}

Object *Cons::print(std::ostream &os) {
  // TODO: revise object printing so that << syntax be used
  // it appears that some implementations of print() return their
  // argument as their value, which might be incompatible with the
  // << syntax.
  
  os << "(";
  Cons *cons = this;
  
  cons->car()->print(os);
  cons = (Cons*)cons->cdr();
  
  while (strcmp(cons->type(), "CONS") == 0) {
    os << ' ';
    cons->car()->print(os);
    cons = (Cons*)cons->cdr();
  }
  
  if(strcmp(cons->type(), "CONS") != 0 && (Symbol*)cons != Symbol::nil()) { 
    os << " . ";
    cons->print(os);
  }
  os << ")";
  
  return this;
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

Cons *Cons::map(Object *(^block)(Object *)) {
  Cons *ptr = this;
  Cons *newList = new Cons();
  Cons *newListPtr = newList;
  /*
   create a cons

   loop
     set new_car = car of current cons
     if cdr is a cons
      set new_cdr = a new cons
      set current element to cdr (both new and old lists)
     else
      set new_cdr = cdr of current element
   */
  const std::string consType("CONS");
  
  while (ptr && ptr->type() == consType) {
    newListPtr->setCar(block(ptr->car())); // map to new value, save in new list
    
    if (ptr->cdr()->type() != consType) { // end of list

      // this extra condition exists so that we don't 
      // attempt to map the nil terminator of a regular list                                                   
      if (ptr->cdr() != Symbol::nil()) {// dotted notation, end of list
        newListPtr->setCdr(block(ptr->cdr()));
      }
      else {
        newListPtr->setCdr(Symbol::nil()); // normal end of list
      }
      
    }
    else { // list continues, build another cons for remainder and loop
      newListPtr->setCdr(new Cons());
      newListPtr = (Cons*)newListPtr->cdr();      
    }    

    ptr = (Cons*)ptr->cdr(); // advance ptr down list one    
  }
  
  return newList;
}

void Cons::each(void (^block)(Object *)) {
  Cons *ptr = this;
  
  const std::string consType("CONS");
  
  while (TYPEOF(ptr) == "CONS") {
    block(ptr->car());
    
    if (ptr->cdr()->type() != consType) { // end of list
      
      // this extra condition exists so that we don't 
      // attempt to invoke the block on the nil terminator of a regular list                                                   
      if (ptr->cdr() != Symbol::nil()) {// dotted notation, end of list
        block(ptr->cdr());
      }
      
    }
    
    ptr = (Cons*)ptr->cdr(); // advance ptr down list one    
  }

}

size_t Cons::length() {
  size_t length = 0;
  
  Cons *ptr = this;
  
  const std::string consType("CONS");
  
  while (TYPEOF(ptr) == "CONS") {
    length++;
    ptr = (Cons*)ptr->cdr(); // advance ptr down list one    
  }
  return length;
}

bool Cons::mark() {
  if (Object::mark()) {
    if (car_)
      car_->mark();
    
    if (cdr_)
      cdr_->mark();
    
    return true;
  }
  return false;
}

Object* Cons::operator[](size_t index) {
  index %= length();
  
  Cons* ptr = this;
  for(size_t i=0; i<index; ptr = (Cons*)ptr->cdr(), i++);
  return ptr->car();
}