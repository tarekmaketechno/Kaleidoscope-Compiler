#include "generator.h"
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/Transforms/PassManagerBuilder.h>

#include "/usr/include/llvm-c-7/llvm-c/Transforms/Scalar.h"
#include "/usr/include/llvm-c-7/llvm-c/Transforms/Vectorize.h"
#include "/usr/include/llvm-c-7/llvm-c/Transforms/IPO.h"

int optimize(generator_t * gen, int optimize_level);
