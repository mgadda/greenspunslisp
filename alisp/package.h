//
//  package.h
//  alisp
//
//  Created by Matt Gadda on 9/24/11.
//  Copyright (c) 2012 Matt Gadda. All rights reserved.
//

#ifndef alisp_package_h
#define alisp_package_h

#include <string>
#include <map>
#include <set>

#include "object.h"

class Symbol;
class Keyword;

class Package : public Object {
  std::string name_;
  std::map<std::string, Symbol*> symbols_;
  std::set<std::string> externedSymbols_;  
  std::set<Package*> parents_;
  
  std::set<std::string> nicknames_;
  
  static std::map<std::string, Package*> packages_;
  
  // TODO: add nicknames list
  // TODO: add parent packages list
  // can this be implemented in lisp instead?

  // TODO: add way to keep track of extern'd symbols vs interned symbols

  static Package system_;
  static Package lisp_;
  static Package user_;
  static Package keyword_;

protected:
  void setSymbol(std::string name, Symbol *symbol);
  
public:

  Package(std::string name);

  virtual const char *type();
  Object *print(std::ostream &os);
  
  static Package *find(std::string name);
  
  Symbol *internSymbol(std::string name); 
  void uninternSymbol(Symbol *symbol);
  void exportSymbol(std::string name);
  void unexportSymbol(std::string name);

  Symbol *resolveInternSymbol(std::string name);
  Symbol *resolveExternSymbol(std::string name);
  Symbol *operator[](std::string name);

  void importSymbol(std::string name);
  void usePackage(Package &package);
  void unusePackage(Package &package);
  
  static Package &system();
  static Package &common_lisp();
  static Package &common_lisp_user();  
  static Package &keyword();  
  
  static Keyword *keywordForName(std::string name);
  
  virtual bool mark();
};


#endif
