//
//  reader.h
//  alisp
//
//  Created by Matt Gadda on 9/22/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#ifndef alisp_reader_h
#define alisp_reader_h

#include <iostream>
#include <string>
#include <sstream>

class Object;
class Cons;
class String;

namespace  {
  Cons *readList(std::streambuf &buf);
  String *readString(std::streambuf &buf);
  
  Object *quote(Object *exp);
  Object *backquote(Object *exp);
  Object *unquote(Object *exp);
  Object *splice(Object *exp);
  Object *function(Object *exp);
  
  Object *readerMacro(char c, std::streambuf &buf);
  void unread(char c, std::streambuf &buf);
  std::string readToken(std::string token, std::streambuf &buf);
  Object *makeObjectForToken(std::string token);  
}

Object *read(std::streambuf &buf);


#endif
