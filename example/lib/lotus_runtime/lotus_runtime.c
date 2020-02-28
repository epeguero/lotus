#include <stdlib.h>
#include <stdio.h>
#include "header/lotus_runtime.h"


Coord* build_1Dcoord(int coord) {
  int* coord_arr = (int*) malloc(sizeof(int));
  coord_arr[0] = coord;

  Coord* c = (Coord*) malloc(sizeof(Coord));
  *c = (Coord) {
    .coord = coord_arr,
    .size = 1
  };
  return c;
}

Range* build_range(int start, int end) {
  Range* r = malloc(sizeof(Range));
  *r = (Range) {
    .start = start,
    .end = end,
    .size = end-start
  };
  return r;
}


Tensor* build_tensor(char* name, int* data, int* size, int dims) {
  Tensor* t = malloc(sizeof(Tensor));
  *t = (Tensor) {
    .name = name,
    .data = data,
    .size = size,
    .dims = dims
  };
  return t;
}

Tensor* build_1Dtensor(char* name, int n) {
  int* a = (int*)malloc(n * sizeof(int));
  int* a_size = (int*) malloc(sizeof(int));
  a_size[0] = n;
  Tensor* A = build_tensor(name, a, a_size, 1);
  return A;
}

void const_init(Tensor* a, int val) {
  for(int dim = 0; dim < a->dims; dim++) {
    for(int i = 0; i < a->size[dim]; i++) {
      a->data[i] = val;
    }
  }
}

void print_1Dtensor(Tensor* a) {
  printf("Contents of %s:\n", a->name);
  printf("[");
  int n = a->size[0];
  for(int i = 0; i < n-1; i++) {
    if(i > 0 && i % 10 == 0) { printf("\n"); }
    printf("%d, ", a->data[i]);
  }
  printf("%d]\n", a->data[n-1]);
}



Partition* build_partition(Range** slices, int dims) {
  int size = 1;
  for(int i = 0; i < dims; i++) {
    size *= slices[i]->end - slices[i]->start;
  }

  Partition* p = (Partition*) malloc(sizeof(Partition));
  *p = (Partition) {
    .slices = slices,
    .dims = dims,
    .size = size
  };

  /* printf("\tbuilt size %d partition (%d,%d)\n",  */
  /*     p->size, p->slices[0]->start, p->slices[0]->end); */

  return p;
}


Tile* build_tile(Coord* gid, int phys_tid_row, int phys_tid_col, int phys_tid) {
  Tile* t = malloc(sizeof(Tile));
  *t = (Tile) {
    .gid = gid,
    .phys_tid_row = phys_tid_row, 
    .phys_tid_col = phys_tid_col,
    .phys_tid = phys_tid 
  };
  printf("\tbuilt tile with gid %d at (%d, %d)\n", t->gid->coord[0], t->phys_tid_row, t->phys_tid_col);
  return t;
}


Group* build_1Dgroup(
    char* name, 
    int max_gid,
    range_builder1D par_rows_gen, 
    range_builder1D par_cols_gen,
    int grid_phys_rows, int grid_phys_cols) {

  printf("building 1D group %s:\n", name);

  // generate tiles corresponding to each group index
  Tile** tiles = malloc(sizeof(Tile*) * max_gid);
  for(int gid = 0; gid < max_gid; gid++) {
    Coord* gid_coord = build_1Dcoord(gid);

    // evaluate physical indices for group index
    Range par_rows = *(*par_rows_gen)(gid);
    Range par_cols = *(*par_cols_gen)(gid);
    for(int phys_row = par_rows.start; phys_row < par_rows.end; phys_row++) {
      for(int phys_col = par_cols.start; phys_col < par_cols.end; phys_col++) {

        // build metadata for each member tile
        int phys_tid = phys_col + phys_row * grid_phys_cols;
        tiles[gid] = build_tile(gid_coord, phys_row, phys_col, phys_tid);
      }
    }
  }

  int* size = malloc(sizeof(int));
  size[0] = max_gid;

  Group* group = malloc(sizeof(Group));
  *group= (Group) {
    .name = name,
    .tiles = tiles,
    .size = size,
    .num_rows_par = grid_phys_rows,
    .num_cols_par = grid_phys_cols
  };

  return group;
}






