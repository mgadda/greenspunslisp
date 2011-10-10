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

#include "symbol.h"
#include "environment.h"
#include "data_types.h"
#include "cons.h"

Mother *Mother::singleton_ = NULL;

Mother::Mother() : allocation_size_(0), deferGC_(false), heapTriggerSize_(2048) {
}

Mother &Mother::instance() {
  if (!Mother::singleton_) {
    Mother::singleton_ = new Mother();
  }
  
  return *singleton_;
}

Object *Mother::newObject(size_t size) {
  Object *newObject = (Object*)malloc(size);
  memset((void*)newObject, 0, size);
  
  HeapNode hnode;
  hnode.object = newObject;
  hnode.size = size;
  
  if (deferGC_)
    delayed_gc_objects_.push_back(hnode);
  else
    allocated_objects_.push_back(hnode);  
  
  allocation_size_ += size;
  std::cout << allocation_size_ << "/" << heapTriggerSize_ <<  std::endl;

  if (allocation_size_ > heapTriggerSize_) {
    markAndSweep();
    if (allocation_size_ >= heapTriggerSize_*.9) {
      // if we could reduce memory usage to 80% of heap size
      heapTriggerSize_ *= 1.5; // increase by 20%
    }
    else {
      heapTriggerSize_ /= 1.5; // decrease by 20%
    }

  }

  return newObject;
}

void Mother::markAndSweep() {
  if (allocation_size_ > heapTriggerSize_) {
    Object *obj = NULL;

    // mark
    std::cout << "Marking...." << std::endl;
    std::cout << roots_.size() << " roots" << std::endl;

    std::vector<Object*>::iterator rootIt;

    for(rootIt = roots_.begin(); rootIt != roots_.end(); rootIt++) {
      obj = (Object*)(*rootIt);
      obj->mark();
    }
    
    // sweep
    std::cout << "Sweeping...." << std::endl;
    std::vector<HeapNode>::iterator it;    
    it = allocated_objects_.begin();
    while(it != allocated_objects_.end()) {

      obj = (*it).object;
      if (!obj->marked() && it != allocated_objects_.end()) {

        allocation_size_ -= (*it).size;
        std::cout << allocation_size_ << std::endl;

        allocated_objects_.erase(it);

        //std::cout << "Deleting " << obj->print() << std::endl;
        free(obj);
      }
      else {
        obj->clearMark();
        ++it;
      }
        
    }
    
    for(rootIt = roots_.begin(); rootIt != roots_.end(); rootIt++) {
      obj = (Object*)(*rootIt);
      obj->clearMark();
    }
    
  }
}

void Mother::addRoot(Object *root) {
  roots_.push_back(root);
}

Object *Mother::deferGC(Object *(^block)()) {
  Object *ret = NULL;
  
  deferGC_ = true;
  ret = block();
  deferGC_ = false;

  std::vector<HeapNode>::iterator it;
  
  for(it = delayed_gc_objects_.begin(); it != delayed_gc_objects_.end(); it++) {
    allocated_objects_.push_back(*it);
  }
  
  delayed_gc_objects_.clear();
  
  return ret;  
}