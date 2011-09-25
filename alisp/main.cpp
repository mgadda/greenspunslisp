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

using namespace std;

class Foo {
public:
	int print(int);
};

int Foo::print(int in) {
	return 5;
}

int print(int in) {
	return in;
}



/*
 syntax types:
 
 constituent - used in "tokens" which represent numbers or symbols 
	traits:
		alphabetic
		digit
		package marker
		plus sign
		minus sign
		dot
		decimal point
		ratio marker
		exponent marker
		invalid
 
 macro character
 single escape
 multiple escape
 invalid
 whitespace
 
 
*/


#include "read_tables.h"

int main (int argc, const char * argv[])
{
  stringbuf * buf;  
  stringstream ss;
  
  //ss << "(defun foo\n; a comment\n the rest)"; 
  ss << "`((,a b) ,c ,@d)";
  buf = ss.rdbuf();
    
  Object *obj = read(*buf);
  
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

