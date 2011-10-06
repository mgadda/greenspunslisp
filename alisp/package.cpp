//
//  package.cpp
//  alisp
//
//  Created by Matt Gadda on 9/24/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#include <iostream>
#include "package.h"
#include "symbol.h"
#include "keyword.h"

void Package::setSymbol(std::string name, Symbol *symbol) {
  symbols_[name] = symbol;
}

Package::Package(std::string name) {
  name_ = name;

  if (Package::packages_.count(name) > 0)
    throw "duplicate package";
  else 
    Package::packages_[name] = this;
}

std::map<std::string, Package*> Package::packages_;

Package *Package::find(std::string name) {
  if (Package::packages_.count(name))
    return Package::packages_[name];
  else
    return NULL;
}

const char *Package::type() {
  return "PACKAGE";
}

Package &Package::system() { return Package::system_; }
Package &Package::common_lisp() { return Package::lisp_; } 
Package &Package::common_lisp_user() { return Package::user_; } 
Package &Package::keyword() { return Package::keyword_; } 

Keyword *Package::keywordForName(std::string name) {
  Keyword *keyword = new Keyword(name);
  Package::keyword().setSymbol(name, keyword);
  keyword->setValue(keyword);
  Package::keyword().exportSymbol(name);
  return keyword;
}

Symbol *Package::operator[](std::string name) {
  return this->resolveExternSymbol(name);
}

Symbol *Package::resolveInternSymbol(std::string name) {
  if (symbols_.count(name)) 
    return symbols_[name];
  else
    return NULL;
}

Symbol *Package::resolveExternSymbol(std::string name) {
  if(externedSymbols_.find(name) != externedSymbols_.end()) {
    return symbols_[name];
  }
  else {
    std::set<Package*>::iterator it;
    
    for(it = parents_.begin(); it != parents_.end(); it++) {
      Package *parentPackage = *it;
      return parentPackage->resolveExternSymbol(name);
    }    
  }  
  return NULL;
}

Symbol *Package::internSymbol(std::string name) {  
  if (!resolveInternSymbol(name) && !resolveExternSymbol(name)) {
    symbols_[name] = new Symbol(name);
    std::cout << "Interning new symbol " << name << "=" << (long)symbols_[name] << std::endl;
    symbols_[name]->setPackage(this);
  }  
  return symbols_[name];
}

void Package::uninternSymbol(Symbol *symbol) {
  symbols_.erase(symbol->name());

  if (symbol->package() == this) {
    // symbol becomes "apparently uninterned" 
    // but may still be interned elsewhere
    symbol->setPackage(Symbol::nil());
  }
}

void Package::exportSymbol(std::string name) {
  // find symbol locally, extern it
  Symbol *sym = resolveInternSymbol(name);
  if (sym) {
    externedSymbols_.insert(name);
    return;
  }
  else {
    // find symbol from inheritance, intern it, then extern it
    importSymbol(name);
    externedSymbols_.insert(name); // extern it 
  }
}

void Package::unexportSymbol(std::string name) {
  std::set<std::string>::iterator it = externedSymbols_.find(name);
  if(it != externedSymbols_.end()) {
    externedSymbols_.erase(it);
  }
}

void Package::importSymbol(std::string name) {
  Symbol *sym = resolveExternSymbol(name);
  symbols_[name] = sym;
}

void Package::usePackage(Package &package) {
  parents_.insert(&package);
}

void Package::unusePackage(Package &package) {
  std::set<Package*>::iterator it = parents_.find(&package);
  if(it != parents_.end()) {
    parents_.erase(it);
  }  
}

Object *Package::print(std::ostream &os) {
  os << "#<PACKAGE " << name_ << ">";
  return this;
}

#pragma mark Default Packages

Package Package::system_("SYSTEM");
Package Package::lisp_("COMMON-LISP"); // ???
Package Package::user_("COMMON-LISP-USER"); // ???
Package Package::keyword_("KEYWORD"); // ???


