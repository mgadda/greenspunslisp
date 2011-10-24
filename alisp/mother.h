//
//  mother.h
//  alisp
//
//  Created by Matt Gadda on 10/3/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#ifndef alisp_mother_h
#define alisp_mother_h

#include <vector>

class Object;

struct HeapNode {
  Object *object;
  size_t size;
};

class Mother {
private:
  std::vector<HeapNode> allocated_objects_;
  std::vector<HeapNode> delayed_gc_objects_;  
  size_t heapTriggerSize_;
  
  static Mother *singleton_;

  std::vector<Object*> roots_;
  
  size_t allocation_size_;
  
  bool deferGC_;
  
  Mother();
  
public:
  void markAndSweep();
  void recordAllocation(Object *obj);  
  
  void addRoot(Object *root);
  static Mother &instance();
  
  Object *newObject(size_t size);
  
  Object *deferGC(Object *(^block)());
//  void each(void (^block)(Object *));
  
  size_t allocation_size() { return allocation_size_; }
  size_t heapTriggerSize() { return heapTriggerSize_; }
};

#endif
