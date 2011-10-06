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

#define MAX_HEAP_SIZE 512

class Mother {
private:
  static std::vector<Object*> allocated_objects_;
  static Mother *singleton_;

  std::vector<Object*> roots_;
  
  unsigned int allocation_size_;
  

  Mother();
  
public:
  void markAndSweep();
  void recordAllocation(Object *obj);  
  
  void addRoot(Object *root);
  static Mother &instance();
};

#endif
