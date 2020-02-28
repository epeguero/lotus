#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "../headers/pthread_launch.h"
#include "../headers/spad.h"
#include "../headers/bind_defs.h"
#include "vvadd.h"

int SP_SIZE = 1000;

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

typedef struct Coord {
  int* coord;
  int size;
} Coord;

Coord* one_dim_coord(int coord) {
  int* coord_arr = (int*) malloc(sizeof(int));
  coord_arr[0] = coord;

  Coord* c = (Coord*) malloc(sizeof(Coord));
  *c = (Coord) {
    .coord = coord_arr,
    .size = 1
  }
  return c;
}

typedef struct Range {
  // half-open interval: [start, end)
  int start, end;
  int size;
} Range;



typedef struct Partition {
  Range** slices;
  int size;
} Partition;

// partition strategies
// TODO: generate from data section
int equipartition1D_start(Tensor* a, Group* g, int i) {
  return i * a->size[0]/g->size[0];
}

int equipartition1D_end(Tensor* a, Group* g, int i) {
  return (i+1) * a->size[0]/g->size[0];
}

// builds slice partition(a, g)[i]
// NOTE: i is currently hardcoded as 0
Range* build_slice(Tensor* a, char* name, Group* g, int gid) {
  Range* slice = (Range*) malloc(sizeof(Range));

  // TODO: generate from data section
  if(strcmp(name, "equipartition1D") == 0) {
    *slice = (Range) {
      .start = equipartition1D_start(a,g,gid),
      .end = equipartition1D_end(a,g,gid)
    };
  }

  return slice;
}

Partition* build_partition(Tensor* a, char* part_strat_name, Group* g, int gid) {
  Range** slices = 
    (Range**) malloc(sizeof(Range*) * a->dims);
  for(int slice_ix = 0; slice_ix < a->dims; slice_ix++) {
    Range* slice = build_slice(a, part_strat_name, g, gid);    
    slices[slice_ix] = slice;
  }

  Partition* p = (Partition*) malloc(sizeof(Partition));
  *p = (Partition) {
    .slices = slices,
    .size = slices[0]->end - slices[0]->start
  };

  printf("\tbuilt size %d partition (%d,%d) for gid %d\n", 
      p->size, p->slices[0]->start, p->slices[0]->end, gid);

  return p;
}





typedef struct Tensor {
  int* data;
  int* size;
  int dims;
  Partition** partitions;
} Tensor;

// builds partitions for all members of g
// NOTE: currently hardcoded for 1D group
Partition** partition(Tensor* t,
                    char* partition_name,
                    Group* g) {

  int max_gid = g->size[0];
  Partition** partitions = 
    (Partition**) malloc(sizeof(Partition*) * max_gid);

  printf("partitioning tensor as %s over %d tiles...\n", 
      partition_name, max_gid);
  for(int i = 0; i < max_gid; i++) {
    Partition* p = build_partition(t, partition_name, g, i);
    partitions[i] = p;
  }

  return partitions;
}

Tensor* to_tensor(
    int* data, int* size, int dims, 
    char* partition_strategy_name, Group* g) {
  Tensor* t = malloc(sizeof(Tensor));
  *t = (Tensor) {
    .data = data,
    .dims = dims,
    .size = size
  };
  t->partitions = partition(t, partition_strategy_name, g);

  return t;
}





typedef struct Tile {
  Coord* gid;
  int phys_tid_row, phys_tid_col;

  // 1D tile id according to HB architecture:
  // row-major id assignment
  // TODO: confirm this, look at the HB docs
  int phys_tid; 
} Tile;

Tile* build_tile(Coord* gid, int phys_tid_row, int phys_tid_col, int phys_tid) {
  Tile* t = malloc(sizeof(Tile));
  *t = (Tile) {
    .gid = gid,
    .phys_tid_row = phys_tid_row, 
    .phys_tid_col = phys_tid_col,
    .phys_tid = phys_tid 
  };
  printf("\tbuilt tile gid %d at %d x %d\n", t->gid[0], t->phys_tid_row, t->phys_tid_col);
  return t;
}




typedef struct Group {
  Tile** tiles;
  int* size;
  int dims;
  int num_rows_par;
  int num_cols_par;
} Group;

Group* build_1Dgroup(
    char* name, 
    int max_gid,
    Range* (par_rows_gen*)(int), 
    Range* (par_cols_gen*)(int),
    int grid_cols) {

  printf("building 1D group %s:\n", name);

  // generate tiles corresponding to each group index
  Tile** tiles = malloc(sizeof(Tile*) * max_gid);
  for(int i = 0; i < max_gid; i++) {
    Coord* gid_coord = build_1Dcoord(gid);

    // evaluate physical indices for group index
    Range* par_rows = par_rows_gen(gid);
    Range* par_cols = par_cols_gen(gid);
    for(int phys_row = par_rows.start; phys_row < par_rows.end; phys_row++) {
      for(int phys_col = par_cols.start; phys_col < par_cols.end; phys_col++) {

        // build metadata for each member tile
        int phys_tid = phys_col + phys_row * grid_cols;
        tiles[i] = build_tile(gid_coord, phys_row, phys_col, phys_tid);
      }
    }
  }

  int* size = malloc(sizeof(int));
  size[0] = max_gid;

  Group* group = malloc(sizeof(Group));
  *oneDimGroup = (Group) {
    .tiles = tiles,
    .size = size,
    .num_rows_par = rows,
    .num_cols_par = cols
  };

  return group;
}





int dram_1Dstream(int* t_sp, int t_sp_size, Tensor* t, int gid, int load_ix) {
  for(int i = 0; i < t_sp_size; i++) {
    int offset = load_ix * t_sp_size;

    Range* slice = t->partitions[gid]->slices[0];
    return t->data[slice->start + offset + i];
  }
}

void dram_vvadd_store(int gid, Tensor* out, int* a_sp, int* b_sp) {
  Range* slice = out->partitions[gid]->slices[0];
  int n = slice->end - slice->start;
  for(int i = 0; i < n; i++) {
    STORE_NOACK(A_sp[i] + B_sp[i], out->data + slice->start + i, 0);
  }
}

// TODO: generate code block declaration
void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) 
vvadd(Tensor* A, Tensor* B, Tensor* C, Group* oneDimGroup, int gid) {
  // scratchpad setup boilerplate
  int *spAddr = (int*)getSpAddr(oneDimGroup->tiles[gid]->phys_tid, 0);

  int stream_inputs = 2;
  Tensor* first_tensor = A;
  int sp_chunk_size = MIN(SP_SIZE/stream_inputs, first_tensor->partitions[gid]->size);

  int *A_sp = spAddr;
  int *B_sp = spAddr + sp_chunk_size;

  // TODO: generate from `A|i, dram|, B|i, dram| ~> A|i, sp|, B|i, sp|`
  int num_sp_loads = MAX(A->partitions[gid]->size / t_sp_size, 1);
  for (int load_ix = 0; load_ix < num_loads; load_ix++) {

    dram_1Dstream(A_sp, sp_chunk_size, A, gid, load_ix);
    dram_1Dstream(B_sp, sp_chunk_size, B, gid, load_ix);

    // TODO: generate from `C|i, dram| = A|i, sp| + B|i, sp|`
    dram_vvadd_store(gid, C, A_sp, B_sp);
  }
}




// TODO: generate from code block declaration
typedef struct Vvadd_Args {
  Tensor* A;
  Tensor* B;
  Tensor* C;
  Group* oneDimGroup;
  int gid;
} Vvadd_Args;

// TODO: generate device kernel name from code block
void *device_vvadd(void *args) {
  // guarentee one thread goes to each core, 
  //by preventing any threads from finishing early
  pthread_barrier_wait(&start_barrier);
  
  // TODO: generate from code block
  Vvadd_Args *a = (Vvadd_Args*)args;
  Tensor* A = a->A;
  Tensor* B = a->B;
  Tensor* C = a->C;
  Group* group = a->oneDimGroup;
  int gid = a->gid;

  // start recording all stats (all cores)
  if (group->tiles[gid]->phys_tid_row == 0 && 
      group->tiles[gid]->phys_tid_col== 0) { 
    stats_on(); 
  }

  // configure i-cache mask
  Tile* tile = group->tiles[gid];
  int mask = getVecMask(tile->phys_tid_col,
                        tile->phys_tid_row,
                        tile->phys_tid_col,
                        tile->phys_tid_row,
                        group->num_cols_par, 
                        group->num_rows_par);

  VECTOR_EPOCH(mask);

  // TODO: generate kernel call from code block
  vvadd(A, B, C, group, gid);
      
  // reset i-cache configuration mask
  VECTOR_EPOCH(0);

  pthread_barrier_wait(&start_barrier);

  if (group->tiles[gid]->phys_tid_row == 0 && 
      group->tiles[gid]->phys_tid_col== 0) { 
    stats_off(); 
  }

  return NULL;
}

// TODO: generate host kernel function decl from code block
void host_vvadd(
    int* a, int* a_size, int a_dims, 
    int* b, int* b_size, int b_dims, 
    int* c, int* c_size, int c_dims) {

  int num_core_rows, num_core_cols;
  int num_cores = 
    get_dimensions(&num_core_cols, &num_core_rows);

  Group* oneDimGroup = 
    build_1Dgroup();

  char* equipartition1D = "equipartition1D";

  // specify partitions across oneDimGroup
  Tensor* A = to_tensor(a, a_size, a_dims, 
                                   equipartition1D, 
                                   oneDimGroup);
  Tensor* B = to_tensor(b, b_size, b_dims, 
                                   equipartition1D, 
                                   oneDimGroup);
  Tensor* C = to_tensor(c, c_size, c_dims, 
                                   equipartition1D, 
                                   oneDimGroup);

  // build kernel arguments for each tile
  Vvadd_Args** args = 
    malloc(sizeof(Vvadd_Args*) * oneDimGroup->size[0]);

  for(int gid = 0; gid < oneDimGroup->size[0]; gid++) {
    Vvadd_Args* tile_args = malloc(sizeof(Vvadd_Args));
    *tile_args = (Vvadd_Args) {
      .A = A,
      .B = B,
      .C = C,
      .oneDimGroup= oneDimGroup,
      .gid = gid
    };
    args[gid] = tile_args;
  }

  printf("initiating scratchpads...\n");
  initScratchpads();
  printf("scratchpads ready!\n");

  printf("launching vvadd kernel on %dx%d core grid...\n", num_core_rows, num_core_cols);
  launch_kernel(device_vvadd, (void**)args, num_core_cols, num_core_rows); 
  printf("vvadd kernel finished!\n");
}

void print_int_array(char* name, int* arr, int n) {
  printf("Contents of %s:\n", name);
  printf("[");
  for(int i = 0; i < n-1; i++) {
    if(i > 0 && i % 10 == 0) { printf("\n"); }
    printf("%d, ", arr[i]);
  }
  printf("%d]\n", arr[n-1]);
}

void main() {
  int n = 100;
  int* A = (int*)malloc(n * sizeof(int));
  int* A_size = (int*) malloc(sizeof(int));
  A_size[0] = n;

  int* B = (int*)malloc(n * sizeof(int));
  int* B_size = (int*) malloc(sizeof(int));
  B_size[0] = n;

  int* C = (int*)malloc(n * sizeof(int));
  int* C_size = (int*) malloc(sizeof(int));
  C_size[0] = n;

  for(int i = 0; i < n; i++) {
    A[i] = 1;
    B[i] = 2;
    C[i] = 0;
  }
  print_int_array("A", A, n);
  print_int_array("B", B, n);

  host_vvadd(A, A_size, 1, B, B_size, 1, C, C_size, 1);

  print_int_array("C", C, n);
}
