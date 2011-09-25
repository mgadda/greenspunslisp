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

using namespace std;

class Object;

Cons *readList(streambuf &buf);
String *readString(streambuf &buf);

Object *quote(Object *exp);
Object *backquote(Object *exp);
Object *unquote(Object *exp);
Object *splice(Object *exp);

Object *readerMacro(char c, streambuf &buf);
void unread(char c, streambuf &buf);
string readToken(string token, streambuf &buf);
Object *makeObjectForToken(string token);
Object *read(streambuf &buf);


#endif
