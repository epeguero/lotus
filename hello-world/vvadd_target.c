#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "../headers/pthread_launch.h"
#include "../headers/spad.h"
#include "../headers/bind_defs.h"
#include "vvadd.h"

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

// TODO generate from target section
int SP_SIZE = 1000;
int PHYS_ROWS = 2;
int PHYS_COLS = 2;

int target_section_is_compatible() {
  int num_core_rows, num_core_cols;
  int num_cores = 
    get_dimensions(&num_core_cols, &num_core_rows);
  if(num_core_rows != PHYS_ROWS || num_core_cols != PHYS_COLS) {
    printf("grid size (%d,%d) in target section incompatible with actual grid sizei (%d, %d).\n",
        PHYS_ROWS, PHYS_COLS, num_core_rows, num_core_cols);
    return 0;
  }

  return 1;
}



typedef struct Coord {
  int* coord;
  int size;
} Coord;

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

typedef struct Range {
  // half-open interval: [start, end)
  int start, end;
  int size;
} Range;

Range* build_range(int start, int end) {
  Range* r = malloc(sizeof(Range));
  *r = (Range) {
    .start = start,
    .end = end,
    .size = end-start
  };
  return r;
}




typedef struct Tensor {
  char* name;
  int* data;
  int* size;
  int dims;
} Tensor;

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






typedef struct Partition {
  Range** slices;
  int dims;
  int size;
} Partition;

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
  printf("\tbuilt tile with gid %d at (%d, %d)\n", t->gid->coord[0], t->phys_tid_row, t->phys_tid_col);
  return t;
}



typedef Range* (*range_builder1D)(int);

typedef struct Group {
  char* name;
  Tile** tiles;
  int* size;
  int dims;
  int num_rows_par;
  int num_cols_par;
} Group;

Group* build_1Dgroup(
    char* name, 
    int max_gid,
    range_builder1D par_rows_gen, 
    range_builder1D par_cols_gen) {

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
        int phys_tid = phys_col + phys_row * PHYS_COLS;
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
    .num_rows_par = PHYS_ROWS,
    .num_cols_par = PHYS_COLS
  };

  return group;
}






typedef struct PartitionedTensor {
  Tensor* t;
  Partition** partitions;
} PartitionedTensor;

// partition strategies
// TODO: generate from data section
Partition** equipartition1D(Tensor* a, Group* g) {
  Partition** partitions = (Partition**) malloc(sizeof(Partition*) * g->size[0]);

  int dims = 1;

  for(int i = 0; i < g->size[0]; i++) {
    Range** slices = (Range**) malloc(sizeof(Range*) * dims);
    slices[0] = build_range(
        i * a->size[0]/g->size[0],
        (i+1) * a->size[0]/g->size[0]);
    partitions[i] = build_partition(slices, dims);
    printf("  built equipartition1D for %s[%d] at %s[%d:%d] (size:%d)\n",
        g->name, i, a->name,
        partitions[i]->slices[0]->start,
        partitions[i]->slices[0]->end,
        partitions[i]->slices[0]->size);
  }

  return partitions;
}

PartitionedTensor* partition(Tensor* a, char* part_strat_name, Group* g) {
  printf("partitioning %s as %s across %s\n", a->name, part_strat_name, g->name);
  PartitionedTensor* pt = (PartitionedTensor*) malloc(sizeof(PartitionedTensor));
  pt->t = a;

  if(strcmp(part_strat_name, "equipartition1D") == 0) {
    pt->partitions = equipartition1D(a, g);
  }
  else {
    printf("ERROR: unimplemented partition strategy '%s'\n", part_strat_name);
  }

  return pt;
}












int dram_1Dstream(int* t_sp, int chunk_size, PartitionedTensor* pt, int gid, int load_ix) {

  int stream_offset = load_ix * chunk_size;
  Range* slice = pt->partitions[gid]->slices[0];

  for(int i = 0; i < chunk_size; i++) {
    t_sp[i] = pt->t->data[slice->start + stream_offset + i];
  }
}

void dram_vvadd_store(int gid, PartitionedTensor* out, int* a_sp, int* b_sp) {
  Range* slice = out->partitions[gid]->slices[0];
  int n = slice->end - slice->start;
  for(int i = 0; i < n; i++) {
    STORE_NOACK(a_sp[i] + b_sp[i], out->t->data + slice->start + i, 0);
  }
}



// TODO: generate code block declaration
void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) 
vvadd(PartitionedTensor* A, PartitionedTensor* B, PartitionedTensor* C, Group* oneDimGroup, int gid) {
  // scratchpad setup boilerplate
  int *spAddr = (int*)getSpAddr(oneDimGroup->tiles[gid]->phys_tid, 0);

  // TODO: generate from `A|i, dram|, B|i, dram| ~> A|i, sp|, B|i, sp|`
  int stream_inputs = 2;
  PartitionedTensor* first_tensor = A;
  int sp_chunk_size = MIN(SP_SIZE/stream_inputs, first_tensor->partitions[gid]->size);

  int *A_sp = spAddr;
  int *B_sp = spAddr + sp_chunk_size;

  int num_sp_loads = MAX(A->partitions[gid]->size / sp_chunk_size, 1);
  for (int load_ix = 0; load_ix < num_sp_loads; load_ix++) {

    dram_1Dstream(A_sp, sp_chunk_size, A, gid, load_ix);
    dram_1Dstream(B_sp, sp_chunk_size, B, gid, load_ix);

    // TODO: generate from `C|i, dram| = A|i, sp| + B|i, sp|`
    dram_vvadd_store(gid, C, A_sp, B_sp);
  }
}




// TODO: generate from code block declaration
typedef struct Vvadd_Args {
  PartitionedTensor* A;
  PartitionedTensor* B;
  PartitionedTensor* C;
  Group* oneDimGroup;
  int gid;
} Vvadd_Args;

Vvadd_Args** build_vvadd_args(PartitionedTensor* A, PartitionedTensor* B, PartitionedTensor* C, Group* group) {
  Vvadd_Args** args = 
    malloc(sizeof(Vvadd_Args*) * group->size[0]);

  for(int gid = 0; gid < group->size[0]; gid++) {
    Vvadd_Args* tile_args = malloc(sizeof(Vvadd_Args));
    *tile_args = (Vvadd_Args) {
      .A = A,
      .B = B,
      .C = C,
      .oneDimGroup= group,
      .gid = gid
    };
    args[gid] = tile_args;
  }

  return args;
}

// TODO: generate device kernel name from code block
void *device_vvadd(void *args) {
  // guarentee one thread goes to each core, 
  //by preventing any threads from finishing early
  pthread_barrier_wait(&start_barrier);
  
  // TODO: generate from code block
  Vvadd_Args *a = (Vvadd_Args*)args;
  PartitionedTensor* A = a->A;
  PartitionedTensor* B = a->B;
  PartitionedTensor* C = a->C;
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

char* partition_strategy = "equipartition1D";

// TODO: generate for each group
Range* oneDimGroup_par_rows_gen(int i) {
  return build_range(i/2, i/2+1);
}

Range* oneDimGroup_par_cols_gen(int i) {
  return build_range(i%2, i%2+1);
}

int oneDimGroup_size = 4;
Group* oneDimGroup;

void initialize_groups() {
  oneDimGroup = build_1Dgroup("oneDimGroup", oneDimGroup_size, &oneDimGroup_par_rows_gen, &oneDimGroup_par_cols_gen);
}


// TODO: generate host kernel function decl from code block
void host_vvadd(Tensor* A, Tensor* B, Tensor* C) {
  // TODO: generate from kernel decl

  // build kernel arguments for each tile
  // TODO: generate from kernel name
  PartitionedTensor* pA = partition(A,  "equipartition1D", oneDimGroup);
  PartitionedTensor* pB = partition(B, "equipartition1D", oneDimGroup);
  PartitionedTensor* pC = partition(C, "equipartition1D", oneDimGroup);
  Vvadd_Args** args = build_vvadd_args(pA, pB, pC, oneDimGroup);

  printf("initiating scratchpads...\n");
  initScratchpads();
  printf("scratchpads ready!\n");

  // TODO: generate from kernel name
  printf("launching vvadd kernel on %dx%d core grid...\n", PHYS_ROWS, PHYS_COLS);
  launch_kernel(device_vvadd, (void**)args, PHYS_COLS, PHYS_ROWS); 
  printf("vvadd kernel finished!\n");
}

//TODO: generate from code section
void code_section() {
  int n = 100;

  Tensor* A = build_1Dtensor("A", n);
  const_init(A, 1);

  Tensor* B = build_1Dtensor("B", n);
  const_init(B, 2);

  Tensor* C = build_1Dtensor("C", n);
  const_init(C, 0);

  print_1Dtensor(A);
  print_1Dtensor(B);

  host_vvadd(A, B, C);

  print_1Dtensor(C);
}

void main() {
  if(!target_section_is_compatible()) {
    printf("Aborting kernel.");
    return;
  } 

  initialize_groups();
  
  code_section();

  return;
}
