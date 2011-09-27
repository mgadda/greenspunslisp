//
//  main.cpp
//  A-Lisp
//
//  Created by Matt Gadda on 9/13/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#include <iostream>
#include <sstream>
#include <string>
#include "data_types.h"
#include "reader.h"
#include "symbol.h"
#include "package.h"

using namespace std;

class Foo {
public:
	int print(int);
};

int Foo::print(int in) {
	return 5;
}

int print(int in);

int print(int in) {
	return in;
}

#include "read_tables.h"
#include <map>

int main (int argc, const char * argv[])
{
  Package::system().usePackage(Package::keyword());
  Package::common_lisp().usePackage(Package::system());
  Package::common_lisp_user().usePackage(Package::common_lisp());
  
  // Set up some stuff...
  Symbol *package = Package::common_lisp().internSymbol("*package*");  
  package->setValue(&Package::common_lisp_user());
  
  stringbuf * buf;  
  stringstream ss;
  
  //ss << "(defun foo\n; a comment\n the rest)"; 
  ss << "(foo nil test)";
  buf = ss.rdbuf();
    
  Object *obj = read(*buf);
  
  obj->print(std::cout);
  cout << endl;
  
	// storing class pointer
	int (Foo::*func)(int) = &Foo::print;
	
	Foo *foo = new Foo();
	
	cout << (*foo.*func)(100);
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

