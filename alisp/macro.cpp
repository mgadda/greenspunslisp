//
//  macro.cpp
//  alisp
//
//  Created by Matt Gadda on 10/1/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#include <iostream>
#include "macro.h"
#include "symbol.h"

Macro::Macro() {
}

Macro *Macro::print(std::ostream &os) {
  os << "#<MACRO >";
  return this;
}

Object *Macro::call(Cons *args, Environment *env) {  
  // TODO: implement macros
  return Symbol::nil();
}