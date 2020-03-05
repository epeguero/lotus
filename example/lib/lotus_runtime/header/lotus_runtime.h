#ifndef LOTUS_RUNTIME_H
#define LOTUS_RUNTIME_H

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

typedef struct Coord {
  int* coord;
  int size;
} Coord;

typedef struct Range {
  // half-open interval: [start, end)
  int start, end;
  int size;
} Range;

typedef struct Tensor {
  char* name;
  int* data;
  int* size;
  int dims;
} Tensor;

typedef struct Partition {
  Range** slices;
  int dims;
  int size;
} Partition;

typedef struct Tile {
  Coord* gid;
  int phys_tid_row, phys_tid_col;

  // 1D tile id according to HB architecture:
  // row-major id assignment
  // TODO: confirm this, look at the HB docs
  int phys_tid; 
} Tile;

typedef Range* (*range_builder1D)(int);

typedef struct Group {
  char* name;
  Tile** tiles;
  int* size;
  int dims;
  int num_rows_par;
  int num_cols_par;
} Group;

typedef struct PartitionedTensor {
  Tensor* t;
  Partition** partitions;
} PartitionedTensor;


Coord* build_1Dcoord(int coord);

Range* build_range(int start, int end); 

Tensor* build_tensor(char* name, int* data, int* size, int dims); 

Tensor* build_1Dtensor(char* name, int n); 

void const_init(Tensor* a, int val); 

void print_1Dtensor(Tensor* a); 

Partition* build_partition(Range** slices, int dims); 


Tile* build_tile(Coord* gid, int phys_tid_row, int phys_tid_col, int phys_tid); 

Group* build_1Dgroup(
    char* name, 
    int max_gid,
    range_builder1D par_rows_gen, 
    range_builder1D par_cols_gen,
    int grid_phys_rows,
    int grid_phys_cols); 

int dram_1Dstream(int* t_sp, int chunk_size, PartitionedTensor* pt, int gid, int load_ix);

void dram_vvadd_store(int gid, PartitionedTensor* out, int* a_sp, int* b_sp);


int getVecMask(int origin_x, int origin_y, int tid_x, int tid_y, int dim_x, int dim_y);

#endif /* LOTUS_RUNTIME_H  */
