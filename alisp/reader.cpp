//
//  reader.cpp
//  alisp
//
//  Created by Matt Gadda on 9/22/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#include <iostream>
#include <string>
#include <algorithm>
#include <pcre.h>

#include "read_tables.h"
#include "reader.h"
#include "data_types.h"
#include "symbol.h"
#include "package.h"
#include "cons.h"
#include "keyword.h"
#include "environment.h"

namespace  {

    // Read lists recursively, caller should retain reference to head 
  Cons *readList(std::streambuf &buf) {
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

  String *readString(std::streambuf &buf) {
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

  Object *quote(Object *exp) {  
    Cons *cons = new Cons(Package::system().internSymbol("QUOTE"));
    cons->setCdr(new Cons(exp,Symbol::nil()));

    return cons;
  }

  Object *backquote(Object *exp) {
    Cons *cons = new Cons(Package::system().internSymbol("BACKQUOTE"));
    cons->setCdr(new Cons(exp,Symbol::nil()));
    
    return cons;
  }

  Object *unquote(Object *exp) {
    Cons *cons = new Cons(Package::system().internSymbol("UNQUOTE"));
    cons->setCdr(new Cons(exp,Symbol::nil()));
    
    return cons;  
  }

  Object *splice(Object *exp) {
    // TODO: use splice symbol from system package
    Cons *cons = new Cons(Package::system().internSymbol("SPLICE"));
    cons->setCdr(new Cons(exp,Symbol::nil()));
    
    return cons;
  }

  Object *function(Object *exp) {
    Cons *cons = new Cons(Package::system().internSymbol("FUNCTION"));
    cons->setCdr(new Cons(exp,Symbol::nil()));
    
    return cons;    
  }
  // (lambda (x y) x)
  Object *readerMacro(char c, std::streambuf &buf) {
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
        // TODO: is this problematic? time will tell.
        return read(buf);
      case '\'':
        return quote(read(buf));
        break;
      case '"':
        // read characters until we encounter a second double quote
        // ignore escape characters
        return readString(buf);
        break;
      case '`':
        // TODO: fancy shit here.
        return backquote(read(buf));
        break;
      case ',':
        // TODO: also do fancy shit here.
        if(buf.sgetc() == '@') {
          buf.sbumpc(); // throw away @
          // splice
          return splice(read(buf));
        }
        else {
          // unquote
          return unquote(read(buf));
        }
      case '#':
        // TODO: dispatch to appropriate macro handler based on whatever
        // character follows. See 2.4.8.
        if(buf.sgetc() == '\'') {
          buf.sbumpc();
          return function(read(buf));
        }
//        else if(buf.sgetc() == '|') { // reader comment #| anything goes here |#
//          char a,b;
//          while (a != '|' || b != '#') {
//            a = b;
//            b = buf.sbumpc();
//          }
//        }
      case ')':
      default:
        break;
    }
    return cons;
  }


  void unread(char c, std::streambuf &buf) {
    buf.sputbackc(c);
  }

  std::string readToken(std::string token, std::streambuf &buf) {
    bool done = false;
    char y, z;
    
    do {
      y = buf.sbumpc();
      if (y == EOF) { return token; }
      
      switch (syntax_type_for_char[y]) {
        case constituent:
        case non_terminating_macro_character:
          token += toupper(y); // TODO: make this conditional based on *print-case* or some other variable
          break;
        case single_escape:
          z = buf.sbumpc();
          if (z == EOF) { throw "end-of-file"; }
          token += z;
          break;
        case multiple_escape:
          done = true;
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

  Object *makeObjectForToken(std::string token) {
    // TODO: move compiled regexes into array so they're not compiled each time.
    // TODO: move all this into another function
    const char *error;
    int errorOffset;
    pcre *intRegex = pcre_compile("^[+-]?[0-9]+$", 0, &error, &errorOffset, NULL);

    if (intRegex == NULL)
    {
      printf("PCRE compilation failed at offset %d: %s\n", errorOffset, error);
      return NULL;
    }
    
    int matches[30];    
    int ret = pcre_exec(intRegex, NULL, token.c_str(), (int)token.length(), 0, 0, matches, 30);
    if (ret >= 0) {
      int val=0;
      if (sscanf(token.c_str(), "%d", &val) > 0) {
        Integer *intVal = new Integer(val);
        return intVal;
      }
      else
        throw "invalid integer specification"; // TODO: handle this case properly
    }

    pcre_free(intRegex);
    
    Package *package = NULL;
    
    // TODO: add number/symbol detection and return correct type of Object
    // TODO: read upcase setting from environment before upcasing here
    std::string symbolName;
    std::string packageName;
    
    std::transform(token.begin(), token.end(), std::back_inserter(symbolName), ::toupper);
    
    // Keyword
    if (symbolName[0] == ':') {    
      return Package::keywordForName(symbolName.substr(1));
    }

    // Symbol with intern package specifier foo::bar
    size_t pos = symbolName.find("::");
    if (pos > 0 && pos < symbolName.length() - 2) {
      packageName = symbolName.substr(0, pos);
      symbolName = symbolName.substr(pos+2);
      
      package = Package::find(packageName);
      if (!package) {
        throw "package does not exist";
      }
      return package->internSymbol(symbolName);
    }
    
    // Extern Package specifier foo:bar
    pos = token.find(':');
    if (pos > 0 && pos < token.length() - 1) {
      packageName = symbolName.substr(0, pos);
      symbolName = symbolName.substr(pos+1);
      
      //Package &externalPackage = Package::packageWithName(packageName);
      package = Package::find(packageName);
      if (!package) {
        throw "package does not exist";
      }
      
      Symbol *externalSym = (*package)[token.substr(pos+1)];
      if (!externalSym) {
        throw "package has no symbol with name ____";
      }
      return externalSym;
    }

    // Symbol with no package specifier
    // search in current package interned symbols
    // then search externed and recursively search parents externed
    
    package = (Package*)Environment::initial().variableForSymbol(Package::common_lisp().resolveExternSymbol("*PACKAGE*"));

    Symbol *sym = package->resolveInternSymbol(symbolName);
    if (sym) return sym;
      
    sym = package->resolveExternSymbol(symbolName);
    if (sym) return sym;
    
    sym = package->internSymbol(symbolName);
    return sym;
    
  }
}

Object *read(std::streambuf &buf) {
	// 1.
	char x, y;


	while(buf.sgetc() != EOF) {
    x = buf.sbumpc();
    std::string token;
    
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
        
        //if ((character_traits_for_char[y] & alphabetic) && !(character_traits_for_char[y] & ~alphabetic)) {
        token.erase();
        token = y;
        token = readToken(token, buf); // step 8
        return makeObjectForToken(token);
        //}
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
                       // start over
			default:
				break;				
		}

	}

  return NULL;
}