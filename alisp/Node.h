//
//  Node.h
//  alisp
//
//  Created by Matt Gadda on 9/20/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#ifndef alisp_Node_h
#define alisp_Node_h

template <class T>
struct Node {
	Node *prev;
	Node *next;
	T data;
};


#endif
