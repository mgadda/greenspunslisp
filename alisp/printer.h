//
//  printer.h
//  alisp
//
//  Created by Matt Gadda on 9/25/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#ifndef alisp_printer_h
#define alisp_printer_h

#include <iostream>

class Object;

Object *print(std::ostream &os, Object *obj);

#endif
