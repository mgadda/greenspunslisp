//
//  reader.cpp
//  alisp
//
//  Created by Matt Gadda on 9/22/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#include <iostream>
#include <string>
#include "read_tables.h"
#include "data_types.h"
#include "reader.h"

//using namespace std;

// Read lists recursively, caller should retain reference to head 
Cons *readList(streambuf &buf) {
  // could be an object, could have hit a terminating macro character
  // in which case it would be nil!
  // ah ha! An exit condition!
  Cons *cons = NULL;
  Cons *cdr = NULL;
  Object *obj = NULL;
  
  if ((obj = read(buf))) {
    if (strcmp(obj->type(), "SYMBOL") == 0 && ((Symbol*)obj)->name() == ".") {
      obj = read(buf); // read next thing after . in "(a b . c)"
                       // check that there's nothing after c
      if(read(buf)) {
        // TODO: throw correct error type here
        throw "illegal end of dotted list";
      }
      // TODO: make a cast not required here
      return (Cons*)obj; // becomes cdr of previous cons
    }
    cons = new Cons(obj);
    cdr = readList(buf);
    
    if (cdr) 
      cons->setCdr(cdr);
    else 
      cons->setCdr(Symbol::nil()); // terminate list with NIL
  }

  return cons;
}

String *readString(streambuf &buf) {
  std::string theString;
  char c = buf.sbumpc();
  
  while (c != '"') {    
    if (c == '\\') {
      theString += buf.sbumpc();
    }
    else {
      theString += c;
    }
    c = buf.sbumpc();
  };

  return new String(theString);
}
// (lambda (x y) x)
Object *readerMacro(char c, streambuf &buf) {
  Cons *cons = NULL;
  char next;
  
  switch (c) {
    case '(':
      cons = readList(buf);
      if(!cons) {
        cons = new Cons(Symbol::nil(), Symbol::nil());
        return cons;
      }
      break;
    case ';':
      // eat characters until we find newline or eof
      
      do {
        next = buf.sbumpc();        
      } while(next != '\n' && next != EOF);
      // then get next object and return, as if comment never happened
      // is this problematic?
      return read(buf);
    case '\'':
      cons = new Cons(Symbol::symbolWithString("QUOTE"));
      cons->setCdr(new Cons(read(buf),Symbol::nil()));
      return cons;
    case '"':
      // read characters until we encounter a second double quote
      // ignore escape characters
      return readString(buf);
      break;
    case '`':
      // TODO: fancy shit here.
      break;
    case ')':
    default:
      break;
  }
  return cons;
}


void unread(char c, streambuf &buf) {
  buf.sputbackc(c);
}

string readToken(string token, streambuf &buf) {
  bool done = false;
  char y, z;
  
  do {
    y = buf.sbumpc();
    if (y == EOF) { return token; }
    
    switch (syntax_type_for_char[y]) {
      case constituent:
      case non_terminating_macro_character:
        token += y;
        break;
      case single_escape:
        z = buf.sbumpc();
        if (z == EOF) { throw "end-of-file"; }
        token += z;
        break;
      case multiple_escape:
        break;
      case terminating_macro_character:
      case whitespace:
        unread(y, buf);
        done = true;
        break;
      case invalid:
        throw "reader-error";
      default:
        break;
    }
  } while (!done);
  
  return token;
}

Object *makeObjectForToken(string token) {
  // TODO: add number/symbol detection and return correct type of Object
  return Symbol::symbolWithString(token);
}

Object *read(streambuf &buf) {
	// 1.
	char x, y;
  
  Object *ret = NULL;
	do {
    
		x = buf.sbumpc();

    string token;
    
		switch(syntax_type_for_char[x]) {
			case invalid: // 2.
        throw "reader-error";
        
			case terminating_macro_character:
			case non_terminating_macro_character:
        // dispatch to appropriate macro method
				// invoke reader macro function				
        return readerMacro(x, buf);        
				break;
			case single_escape: // 5.
        y = buf.sbumpc();
        if (y == EOF) { throw "end-of-file"; }
        
        //   0010 value
        // & 0010 alphabetic
        //   != 0
        
        //   0010 value
        // & 1101 !alphabetic
        //   0000
        //   == 0
        
        if ((character_traits_for_char[y] & alphabetic) && !(character_traits_for_char[y] & alphabetic)) {
          token.erase();
          token = y;
          token = readToken(token, buf);
        }
				break;
			case multiple_escape:	// 6.	
                            // TODO: goto step 9, making amends
        
				break;
			case constituent: // 7.
        token = x;
        token = readToken(token, buf);
        return makeObjectForToken(token);        
				break;
      case whitespace: // 3.     
			default:
				break;				
		}
	} while(x != EOF);
	
  
	return ret;
}