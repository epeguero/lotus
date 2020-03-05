#include "gem5/header/bind_defs.h"

void stats_off()
{
#if !defined(__x86_64__) && !defined(__i386__)
  int off = 10; // can't use 0, but anything other than 1
 __asm__ volatile ("csrw 0x7C1, %0;"
                    :
                    : "r" (off)
                    :);
#endif
}

int getVecMask(int origin_x, int origin_y, int tid_x, int tid_y, int dim_x, int dim_y) {
  int mask = ALL_NORM;
  
  #ifndef _VEC
  return mask;
  #else
  
  // upper left corner is the master
  if (tid_x == 0 && tid_y == 0) {
    mask = FET_O_INST_DOWN_SEND | FET_O_INST_RIGHT_SEND;
  }
  
  // right edge does not send to anyone
  else if (tid_x == dim_x - 1) {
    mask = FET_I_INST_LEFT;
  }
  
  // bottom left corner just sends to the right
  else if (tid_x == 0 && tid_y == dim_y - 1) {
    mask = FET_I_INST_UP | FET_O_INST_RIGHT_SEND;
  }
  
  // the left edge (besides corners) sends down and to the right
  else if (tid_x == 0) {
    mask = FET_I_INST_UP | FET_O_INST_DOWN_SEND | FET_O_INST_RIGHT_SEND;
  }
  
  // otherwise we're just forwarding to the right in the middle area
  else {
    mask = FET_I_INST_LEFT | FET_O_INST_RIGHT_SEND;
  }
  
  // specify the vlen
  int vlenX = dim_x;
  int vlenY = dim_y;
  mask |= (origin_x << FET_XORIGIN_SHAMT) | (origin_y << FET_YORIGIN_SHAMT) | (vlenX << FET_XLEN_SHAMT) | (vlenY << FET_YLEN_SHAMT);

  // specify each core is an execute core
  mask |= (0 << FET_DAE_SHAMT);

  return mask;
  #endif
}

// mask that guarentees a linear chain with no fanout
// implements a snake pattern
// -> -> -> v
// v <- <- <-
// -> -> -> v
// 0 <- <- <-
int getSerializedMask(int origin_x, int origin_y, int tid_x, int tid_y, int dim_x, int dim_y) {
  int mask = ALL_NORM;
  
  #ifndef _VEC
  return mask;
  #else
  
  // each row alternates between different behavior
  if (tid_y % 2 == 0) {
    // if first column either recv from above or not at all
    if (tid_x == 0) {
      if (tid_y == 0) {
        mask |= ALL_NORM;
      }
      else {
        mask |= FET_I_INST_UP;
      }
    }
    // otherwise recv from the left
    else {
      mask |= FET_I_INST_LEFT;
    }

    // send to the right if not at edge
    if (tid_x < dim_x - 1) {
      mask |= FET_O_INST_RIGHT_SEND;
    }
    // if at the edge send down
    else {
      mask |= FET_O_INST_DOWN_SEND;
    }
  }
  else {
    // input either above if at the right edge or from the right
    if (tid_x == dim_x - 1) {
      mask |= FET_I_INST_UP;
    }
    else {
      mask |= FET_I_INST_RIGHT;
    }

    // output either to the left or down if at left edge
    if (tid_x == 0) {
      mask |= FET_O_INST_DOWN_SEND;
    }
    else {
      mask |= FET_O_INST_LEFT_SEND;
    }
  }
  
  // specify the vlen
  int vlenX = dim_x;
  int vlenY = dim_y;
  mask |= (origin_x << FET_XORIGIN_SHAMT) | (origin_y << FET_YORIGIN_SHAMT) | (vlenX << FET_XLEN_SHAMT) | (vlenY << FET_YLEN_SHAMT);

  // specify each core is an execute core
  mask |= (0 << FET_DAE_SHAMT);

  return mask;
  #endif
}

int getDAEMask(int origin_x, int origin_y, int tid_x, int tid_y, int dim_x, int dim_y) {
  int mask = (1 << FET_DAE_SHAMT) | 
            (origin_x << FET_XORIGIN_SHAMT) | 
            (origin_y << FET_YORIGIN_SHAMT) | 
            (dim_x << FET_XLEN_SHAMT) | 
            (dim_y << FET_YLEN_SHAMT);
  return mask;
}
