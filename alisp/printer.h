//
//  printer.h
//  alisp
//
//  Created by Matt Gadda on 9/25/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#ifndef alisp_printer_h
#define alisp_printer_h

#include <iostream>

class Object;

Object *print(std::ostream &os, Object *obj);

#endif
