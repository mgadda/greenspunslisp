//
//  main.cpp
//  A-Lisp
//
//  Created by Matt Gadda on 9/13/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
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

//class Foo {
//public:
//	int print(int);
//};
//
//int Foo::print(int in) {
//	return 5;
//}
//
//int print(int in);
//
//int print(int in) {
//	return in;
//}

//Symbol *f = Package::common_lisp().internSymbol("foo");
//env->bindVariable(f, Symbol::nil());

//Environment *env2 =  new Environment(env);

//env2->variableForSymbol(f)->print(std::cout);

int main (int argc, const char * argv[])
{
  
  // Setup Mother's roots
  Mother &mother = Mother::instance();
  mother.addRoot(&Package::system());
  mother.addRoot(&Package::common_lisp());
  mother.addRoot(&Package::common_lisp_user());
  mother.addRoot(&Package::keyword());
  
  Environment *env =  new Environment(NULL);
  
  mother.addRoot(env);
  
  // Import basic packages
  Package::system().usePackage(Package::keyword());
  Package::common_lisp().usePackage(Package::system());
  Package::common_lisp_user().usePackage(Package::common_lisp());
  
  // Other necessary symbols for the reader to function
  Symbol *sym = Package::common_lisp().internSymbol("*PACKAGE*");  
  sym->setValue(&Package::common_lisp_user());
  sym->setNoGC(true);
  Package::common_lisp().exportSymbol(sym->name());
  
  // Never collect these symbols
  Symbol::nil()->setNoGC(true);
  Symbol::t()->setNoGC(true);
  
  initSystem();
  
  std::stringbuf * buf;  
  std::stringstream ss;

  int lineno = 0;
  std::cout << "[" << lineno << "]> ";

  while (true) {
    
    unsigned int nestedCount = 0;
    std::string input;
    char c;
    do {
      //c = getc(std::cin);
      c = getchar();
      
      if (c == '(') nestedCount++;
      else if (c == ')') nestedCount--;
      
      input += c;
      
    } while (nestedCount > 0 || input[input.length()-1] != '\n');
    
    ss.clear();
    ss << input;
    buf = ss.rdbuf();
    
    Object *obj = NULL;
    
    try {
      obj = read(*buf);
    }
    catch (const char *msg) {
      std::cout << msg;
    }
    
    if (obj) {
      try {
        eval(obj, env)->print(std::cout);
        Mother::instance().markAndSweep();
      } catch (const char *msg) {        
        std::cout << msg;
      }
      
      std::cout << std::endl;
      std::cout << "[" << ++lineno << "]> ";

    }
  }
  
	// storing class pointer
//	int (Foo::*func)(int) = &Foo::print;
//	
//	Foo *foo = new Foo();
//	
//  std::cout << (*foo.*func)(100);
//	
//	Stack<string> stack;
//	
//	stack.push("bottom");
//	stack.push("middle");
//	stack.push("top");
//	
//	Node<string> *node = stack.find("bananas");
//	if(node)
//		cout << node->data;
//	else
//		cout << "Couldn't find bananas.";
//	
//	stack.reverse();
//	cout << "The stack is currently: " << stack << endl;
	
	
    return 0;
}
