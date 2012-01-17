//
//  keyword.h
//  alisp
//
//  Created by Matt Gadda on 9/26/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#ifndef alisp_keyword_h
#define alisp_keyword_h

#include "symbol.h"

class Keyword : public Symbol {
public:
  virtual const char *type() { return "KEYWORD"; }
  Keyword(std::string name);
  
  virtual Object *print(std::ostream &os);
  
  virtual bool mark();
};

#endif
