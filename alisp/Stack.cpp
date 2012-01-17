//
//  Stack.cpp
//  A-Lisp
//
//  Created by Matt Gadda on 9/13/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#include <iostream>
#include "Stack.h"
#include "Node.h"

template <class T>
void Stack<T>::push(T obj) {
	Node<T> *newHead = new Node<T>;
	if(_head) {
		_head->next = newHead;
		newHead->prev = _head;
	}
	_head = newHead;	
	_head->data = obj;
	return;
}

template <class T>
T Stack<T>::pop() {
	if (_head->prev)
		_head->prev->next = NULL;
	T data = _head->data;
	
	delete _head;
	
	return data;
}

template <class T>
Node<T>* Stack<T>::find(T obj) {
	Node<T>* ptr = _head;
	
	while(ptr && ptr->data != obj)
		ptr = ptr->prev;
	
	return ptr;
}

template <class T>
void Stack<T>::reverse() {
	Node<T> *ptr, *prev, *next;
	
	ptr = _head;
	while(ptr) {
		next = ptr->next;
		prev = ptr->prev;
		ptr->next = prev;
		ptr->prev = next;
		
		if(!prev) {
			_head = ptr;
		}
		ptr = prev;
	}
	
}

template <class T>
ostream& operator<<(ostream& os, const Stack<T>& stack) {
	if(!stack._head) 
		return os;
	
	const Node<T> *ptr = stack._head;
	do {
		os << ptr->data << endl;
		ptr = ptr->prev;
	} while (ptr);
	
	return os;	
}
