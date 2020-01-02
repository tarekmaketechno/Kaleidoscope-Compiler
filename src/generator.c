#include "generator.h"

generator_t * initialize_llvm() {
	LLVMModuleRef module = LLVMModuleCreateWithName("main_module");
	LLVMBuilderRef builder = LLVMCreateBuilder();

	generator_t * gen = 
		(generator_t *) malloc(sizeof(generator_t));

	gen->builder = builder;
	gen->module = module;

	return gen;
}


LLVMTypeRef llvm_data_type(data_type_type dt) {
	switch(dt) {
		case Int_type:
			return LLVMInt32Type();
		case Cint_type:
			return LLVMInt32Type();
		case Float_type:
			return LLVMFloatType();
		case Bool_type:
			return LLVMInt1Type();
		case Char_type:
			return LLVMPointerType(LLVMInt8Type(), 0);
		case Void_type:
			return LLVMVoidType();
	}

}


int validate_and_run(generator_t * gen) {
	// Validation moved to main to work
	// better with optimizer!
	// TODO: update name of this function

	// Declare an engine
	LLVMExecutionEngineRef engine;

	// Null out error var
	char *error = NULL;

	// Initialize ASM printer
	LLVMInitializeNativeAsmPrinter();
	LLVMInitializeNativeDisassembler();

	// Link in MC JIT
	LLVMLinkInMCJIT();

	LLVMInitializeNativeTarget();

	if (LLVMCreateExecutionEngineForModule(&engine, gen->module, &error) != 0) {
		fprintf(stderr, "failed to create execution engine\n");
		abort();
	}

	if (error) {
		fprintf(stderr, "error: %s\n", error);
		LLVMDisposeMessage(error);
		exit(EXIT_FAILURE);
	}

	LLVMCreateMCJITCompilerForModule(
			&engine, gen->module, 0, 0, &error);


	int (*run_func)() = 
		(int (*)(void))LLVMGetFunctionAddress(engine, "run");

	if (!run_func) {
		fprintf(stderr, "ERROR! run function not found!\n");
		exit(EXIT_FAILURE);
	}

	int rtn = run_func();
	return rtn;
}
