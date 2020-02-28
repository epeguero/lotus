#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "gem5/header/pthread_launch.h"
#include "gem5/header/spad.h"
#include "gem5/header/bind_defs.h"
#include "lotus_runtime/header/lotus_runtime.h"

// TODO generate from target section
static int SP_SIZE;
static int PHYS_ROWS;
static int PHYS_COLS;

int initialize_target_section(int sp_size, int phys_rows, int phys_cols) {
  SP_SIZE = sp_size;
  PHYS_ROWS = phys_rows;
  PHYS_COLS = phys_cols;
}

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
  oneDimGroup = build_1Dgroup("oneDimGroup", oneDimGroup_size, &oneDimGroup_par_rows_gen, &oneDimGroup_par_cols_gen, PHYS_ROWS, PHYS_COLS);
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
  initialize_target_section(1000, 2, 2);

  if(!target_section_is_compatible()) {
    printf("Aborting kernel.");
    return;
  } 

  initialize_groups();
  
  code_section();

  return;
}
