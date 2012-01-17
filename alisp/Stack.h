//
//  Stack.h
//  A-Lisp
//
//  Created by Matt Gadda on 9/13/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#ifndef alisp_Stack_h
#define alisp_Stack_h

#include <iostream>
#include "Node.h"

using namespace std;

template <class T>
class Stack {
	Node<T> *_head;
	
public:
	void push(T obj);
	T pop();	
	Node<T>* find(T obj);
	
	void reverse();
	
	template <class U>
	friend ostream& operator<<(ostream& os, const Stack<U>& stack);
};


#endif
