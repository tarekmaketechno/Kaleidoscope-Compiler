/*************************************************|
 *
 * All of the shit needed to generate code, 
 * (I hope?) basically...
 *
 *************************************************/

#pragma once

#include <stdio.h>
#include <stdlib.h>

#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>

#include "symbol.h"
#include "bindings.h"

typedef struct generator_t {
	LLVMBuilderRef builder;
	LLVMModuleRef module;
} generator_t;

generator_t * initialize_llvm();

LLVMTypeRef llvm_data_type(data_type_type dt);

int validate_and_run(generator_t * gen);
