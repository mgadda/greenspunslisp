//
//  mother.cpp
//  alisp
//
//  Created by Matt Gadda on 10/3/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#include <iostream>
#include "mother.h"
#include "object.h"
#include "package.h"

std::vector<Object*> Mother::allocated_objects_;
Mother *Mother::singleton_ = NULL;

Mother::Mother() : allocation_size_(0) {
  
}

Mother &Mother::instance() {
  if (!Mother::singleton_) {
    Mother::singleton_ = new Mother();
  }
  
  return *singleton_;
}

void Mother::recordAllocation(Object *obj) {
  allocated_objects_.push_back(obj);
  
  allocation_size_ += sizeof(*obj);
  std::cout << allocation_size_ << std::endl;

}

void Mother::markAndSweep() {
  if (allocation_size_ > MAX_HEAP_SIZE) {
    Object *obj = NULL;
    
    // mark
    std::cout << "Marking...." << std::endl;
    std::cout << roots_.size() << " roots" << std::endl;
    std::vector<Object*>::iterator it;
    for(it = roots_.begin(); it != roots_.end(); it++) {
      obj = (Object*)(*it);
      obj->mark();
    }
    
    // sweep
    std::cout << "Sweeping...." << std::endl;
    it = allocated_objects_.begin();
    while(it != allocated_objects_.end()) {

      obj = (Object*)(*it);
      if (!obj->marked() && it != allocated_objects_.end()) {

        allocated_objects_.erase(it);

        //std::cout << "Deleting " << obj->print() << " (" << (long)obj << ")" << std::endl;
        allocation_size_ -= sizeof(*obj);
        std::cout << allocation_size_ << std::endl;
        delete obj;
      }
      else {
        obj->clearMark();
        ++it;
      }
        
    }
    
    for(it = roots_.begin(); it != roots_.end(); it++) {
      obj = (Object*)(*it);
      obj->clearMark();
    }
    
  }
}

void Mother::addRoot(Object *root) {
  roots_.push_back(root);
}