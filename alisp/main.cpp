//
//  main.cpp
//  A-Lisp
//
//  Created by Matt Gadda on 9/13/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#include <iostream>
#include <cstdio>
#include <sstream>
#include <string>
#include "data_types.h"
#include "reader.h"
#include "symbol.h"
#include "package.h"
#include "environment.h"

#include "evaler.h"
#include "system.h"
#include "mother.h"

int main (int argc, const char * argv[])
{
  // Setup Mother's roots
  Mother &mother = Mother::instance();
  mother.addRoot(&Package::system());
  mother.addRoot(&Package::common_lisp());
  mother.addRoot(&Package::common_lisp_user());
  mother.addRoot(&Package::keyword());
  
  Environment &env = Environment::initial();
  mother.addRoot(&env);

  // Import basic packages
  Package::system().usePackage(Package::keyword());
  Package::common_lisp().usePackage(Package::system());
  Package::common_lisp_user().usePackage(Package::common_lisp());
  
  
  // Other necessary symbols for the reader to function
  Symbol *sym = Package::common_lisp().internSymbol("*PACKAGE*");  
  env.bindVariable(sym, &Package::common_lisp_user());
  sym->setNoGC(true);
  
  Package::common_lisp().exportSymbol(sym->name());
  
  // Never collect these symbols
  Symbol::nil()->setNoGC(true);
  Symbol::t()->setNoGC(true);
  
  initSystem();
  
  std::stringbuf * buf;  
  std::stringstream ss;

  __block int lineno = 0;
  std::cout << "[" << Mother::instance().allocation_size()/1000.0 << "KB " << lineno << "]> ";

  while (true) {
    
    unsigned int nestedCount = 0;
    std::string input;
    char c;
    do {
      //c = getc(std::cin);
      c = getchar();
      
      if (c == '(' && input[input.length()-1] != '\\') nestedCount++;
      else if (c == ')' && input[input.length()-1] != '\\') nestedCount--;
      
      input += c;
      
    } while (nestedCount > 0 || input[input.length()-1] != '\n');
    
    ss.clear();
    ss << input;
    buf = ss.rdbuf();
    
    __block Object *obj = NULL;
    
    // We set aside any allocations while evaluating something so that they're
    // not collected until after the evaluation is complete.
    Mother::instance().deferGC(^Object *{
    
      try {
        obj = read(*buf);
      }
      catch (const char *msg) {
        std::cout << msg;
      }
      
      if (obj) {
        try {        
          Object* evaledObject = eval(obj, &Environment::initial());
          evaledObject->print(std::cout);          
        } catch (const char *msg) {        
          std::cout << msg;
        }
        
        std::cout << std::endl;
        std::cout << "[" << Mother::instance().allocation_size()/1000.0 << "KB " << lineno << "]> ";

      }
      return NULL;
    });
    
  }
	
  return 0;
}
